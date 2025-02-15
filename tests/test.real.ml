(* elpi: embedded lambda prolog interpreter                                  *)
(* license: GNU Lesser General Public License Version 2.1 or later           *)
(* ------------------------------------------------------------------------- *)
open Test_suite
open Suite
    
module Printer : sig

  type status = [ `OK | `KO | `SKIPPED | `TIMEOUT | `RUNNING ]
  val print :
    executable:string -> name:string -> description:string -> float -> float -> float -> int -> status -> unit

  val print_header :
    executables:string list -> seed:int -> timeout:float -> unit
    
  val print_summary :
    total:int -> ok:int -> ko:int -> skipped:int -> unit

  val print_log :
    fname:string -> unit

end = struct
open ANSITerminal

type status = [ `OK | `KO | `SKIPPED | `TIMEOUT | `RUNNING ]
let print_state col s = printf [col] "%-9s%!" s
let print_timing name t0 t1 t2 mem ex = printf [] "%-20s %6.2f %6.2f %6.2f %6.1fM  %s%!" name t0 t1 t2 (float_of_int mem /. 1024.0) ex
let print_info name descr ex = printf [] "%-43s %s" (name ^" ("^descr^")") ex

let print ~executable ~name ~description:descr t0 t1 t2 mem = function
  | `OK -> print_state green "OK"; print_timing name t0 t1 t2 mem executable
  | `TIMEOUT -> print_state red "TIMEOUT"; print_info name descr executable
  | `KO -> print_state red "KO"; print_info name descr executable
  | `RUNNING -> print_state blue "RUNNING"; print_info name descr executable
  | `SKIPPED -> print_state yellow "SKIPPED"; print_info name descr executable

let print_header ~executables ~seed ~timeout =
  printf [blue] "------------------------------------------------------------------\n";
  printf [blue] "Runners:"; printf [] " %s\n" (String.concat " " executables);
  printf [blue] "Random seed:"; printf [] " %d\n" seed;
  printf [blue] "Timeout:"; printf [] " %.2f seconds\n" timeout;
  printf [blue] "Fiber stack:"; printf [] " %d\n" (Gc.get ()).Gc.stack_limit;
  printf [blue] "\n";
  printf [blue] "status   test                  time   typchk wall   mem     runner\n";
  printf [blue] "------------------------------------------------------------------\n";
;;

let print_summary ~total ~ok ~ko ~skipped =
  printf [blue] "------------------------------------------------------------------\n";
  printf [blue] "Tests: "; printf [] "%d\n" total;
  printf [blue] "Passed: "; printf [] "%d\n" ok;
  printf [blue] "Failed: "; printf [] "%d\n" ko;
  printf [blue] "Skipped: "; printf [] "%d\n" skipped;
;;

let print_file fname =
  try
    let ic = open_in fname in
    while true do
      let s = input_line ic in
      printf [] "%s\n" s
    done
  with
  | End_of_file -> ()
  | e -> printf [red] "Error reading %s: %s\n" fname (Printexc.to_string e)

let print_log ~fname =
  printf [red] "------------------------------------------------------------------\n";
  printf [blue] "Log of the first failure: "; printf [] "%s\n" fname;
  printf [red] "------------------------------------------------------------------\n";
  print_file fname;
  printf [red] "------------------------------------------------------------------\n";

end

let aNSITerminal_move_bol () =
  if Sys.win32 then ANSITerminal.printf [] "\n%!"
  else ANSITerminal.move_bol ()

let run timeout _seed sources env { Runner.run; test; executable }  =

  let { Test.name; description; _ } = test in
  let print = Printer.print ~executable:(Filename.basename executable) ~name ~description in

  print 0.0 0.0 0.0 0 `RUNNING;
  aNSITerminal_move_bol ();

  let rc = match run ~timeout ~env ~sources with
    | Runner.Skipped -> print 0.0 0.0 0.0 0 `SKIPPED; None
    | Runner.Done ({ Runner.rc; _ } as x) ->
      begin match rc with
        | Runner.Timeout timeout -> print timeout timeout timeout 0 `TIMEOUT
        | Runner.Failure { Runner.execution; typechecking; walltime; mem } ->
            print execution typechecking walltime mem `KO
        | Runner.Success { Runner.execution; typechecking; walltime; mem } ->
            print execution typechecking walltime mem `OK
      end;
      Some x
  in
  ANSITerminal.(erase Eol);
  ANSITerminal.printf [] "\n%!";
  rc

let print_csv plot results =
  let oc = open_out "data.csv" in
  results |> List.iter
    (function 
      | Some { Runner.rc; executable; test = { Test.name; _ }; _ } ->
          begin match rc with
          | Runner.Timeout _ -> ()
          | Runner.Failure _ -> ()
          | Runner.Success { Runner.execution; walltime; mem; _ } -> (* TODO: print typechecking time *)
              Printf.fprintf oc "%s,%s,%f,%f,%d\n"
                executable name execution walltime mem
          end
      | None -> ());
  close_out oc;
  ignore(Sys.command (plot ^ " data.csv"));
  ignore(Sys.command "gnuplot data.csv.plot")
;;

let rec find_map f = function
  | [] -> raise Not_found
  | x :: xs ->
      match f x with
      | Some y -> y
      | None -> find_map f xs

let main sources plot timeout executables namef catskip timetool seed =
  Random.init seed;
  let filter_name =
    let rex = Str.regexp (".*"^namef) in
    fun ~name:x -> Str.string_match rex x 0 in
  let cruft = "CRUFT="^ String.make (Random.bits () mod (2 lsl 16)) 'x' in
  let env = Array.concat [[|cruft|];Unix.environment ()] in
  let tests = Suite.Test.get ~catskip filter_name in
  Printer.print_header ~executables ~seed ~timeout;
  let jobs =
    tests |> List.map (Suite.Runner.jobs ~timetool ~executables)
          |> List.concat in
  let results =
    List.map (run timeout seed sources env) jobs in
  let total, ok, ko, skipped =
    let skip, rest =
      List.partition (function None -> true | _ -> false) results in
    let ok, ko =
      List.partition (function
        | Some { Runner.rc = Runner.Success _; _ } -> true
        | _ -> false) rest in
    List.(length jobs, length ok, length ko, length skip) in
  Printer.print_summary ~total ~ok ~ko ~skipped;
  begin try
    let log_first_failure =
      results |> find_map (function
        | Some { Runner.rc = Runner.Failure _; log; _ } -> Some log
        | _ -> None) in
    Printer.print_log ~fname:log_first_failure
  with Not_found -> ()
  end;
  if List.length executables > 1 then print_csv plot results;
  if ko = 0 then exit 0 else exit 1

open Cmdliner

let runners =
  let doc = "Run tests against $(docv)." in
  Arg.(non_empty & opt_all non_dir_file [] & info ["runner"] ~docv:"RUNNER" ~doc)

let valid_category_parser c =
  if List.exists (fun (c',_) -> c = c') (Test.names ())
  then `Ok c
  else `Error ("unknown category " ^ c)

let valid_category = Arg.(valid_category_parser,conv_printer string)

let namef =
  let doc = "Run only tests with a name that matches $(docv)." in
  Arg.(value & opt string "." & info ["name-match"] ~docv:"REX" ~doc)

let catskip =
  let doc = "Skip tests belonging to category $(docv)." in
  Arg.(value & opt_all valid_category [] & info ["cat-skip"] ~docv:"STRING" ~doc)
  
let seed =
  let doc = "Uses $(docv) as the random number generator seed." in
  Arg.(value & opt int 0 & info ["seed"] ~docv:"INT" ~doc)

let timeout =
  let doc = "Uses $(docv) as the timeout (in seconds)." in
  Arg.(value & opt float 30.0 & info ["timeout"] ~docv:"FLOAT" ~doc)

let src =
  let doc = "Looks for the sources in $(docv)." in
  Arg.(value & opt string "sources/" & info ["sources"] ~docv:"DIR" ~doc)

let plot =
  let doc = "Path for the plot utility is $(docv)." in
  Arg.(value & opt non_dir_file "./plot" & info ["plot"] ~docv:"PATH" ~doc)

let mem =
  let doc = "Uses $(docv) as the tool to measure memory." in
  Arg.(value & opt non_dir_file "/usr/bin/time" & info ["time"] ~docv:"PATH" ~doc)

let info =
  let doc = "run the test suite" in
  let tests = Test.names ()
    |> List.map (fun (cat,ts) -> [ `I(cat,String.concat ", " ts) ])
    |> List.concat in
  let man = [`Blocks [`S "KNOWN TESTS" ; `Blocks tests ] ] in
  (Term.info ~doc ~exits:Term.default_exits ~man "test") [@ warning "-A"]
  (* ocaml >= 4.08 | Cmd.info ~doc ~exits:Cmd.Exit.defaults ~man "test" *)
;;

let () =
  (Term.exit @@ Term.eval (Term.(const main $ src $ plot $ timeout $ runners $ namef $ catskip $ mem $ seed),info)) [@ warning "-A"]
  (* ocaml >= 4.08 | exit @@ Cmd.eval (Cmd.v info Term.(const main $ src $ plot $ timeout $ runners $ namef $ catskip $ mem $ seed)) *)
