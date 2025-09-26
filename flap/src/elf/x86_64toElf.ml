module Source = X86_64
module Target = Elf

type environment = unit

let initial_environment () =
  ()

let find_file_in_sites filename =
  match List.find_map (fun d ->
            if Options.get_debug_mode ()
            then Printf.eprintf "[DEBUG] (in? %s %s)\n" filename d;
            let full = Filename.concat d filename in
            if Sys.file_exists full then Some full else None
          ) Sites.Sites.runtime with
  | Some r -> r
  | None ->
     Error.global_error "internal" ("Could not find file " ^ filename ^ " in "
                                    ^ List.fold_left (^) "" Sites.Sites.runtime)

let gcc ~src ~tgt =
  let runtime = find_file_in_sites "runtime.c" in
  Printf.sprintf
    "gcc -no-pie -g %s %s -o %s"
    src
    runtime
    tgt

let translate (p : X86_64.ast) _env =
  (* 1. Generate a temporary .s file.
     2. Call gcc to generate an executable linked with runtime.o
     3. Execute this program, capturing its stdout/stderr
   *)
  let asmf = Filename.temp_file "flap" ".s" in
  let elff = Filename.temp_file "flap" ".elf" in
  let oc = open_out asmf in
  PPrint.ToChannel.compact oc (X86_64_PrettyPrinter.program p);
  close_out oc;
  let exit_status, _, stderr =
    ExtStd.Unix.output_and_error_of_command (gcc ~src:asmf ~tgt:elff)
  in
  if exit_status <> Unix.WEXITED 0
  then
    Error.error
      "ELF"
      Position.dummy
      (Printf.sprintf "Could not assemble or link file \"%s\":\n%s" asmf stderr)
  else
    let ic = open_in elff in
    let b = ExtStd.Buffer.slurp ic in
    close_in ic;
    List.iter Sys.remove [asmf; elff];
    b,
    ()
