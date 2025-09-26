let find_flap () =
  try
    let base_dir = Path.find_upwards "flap" in
    let flap =
      List.fold_left Filename.concat base_dir
        ["_build"; "default"; "src"; "flap.exe"]
    in
    if Sys.file_exists flap then flap
    else begin
        Printf.eprintf "Could not find flap.exe at %s.\n" flap;
        Printf.eprintf "Have you run `dune build`?\n";
        exit 1
      end
  with Failure _ ->
    "flap"

let flap = ref ""

let dump_html contents =
  let page = Report.html_of ~mode:`Full contents in
  print_string Pure_html.(to_string page)

let run_test cfg filename =
  Test.from_file filename
  |> Result.map Run.(run cfg)
  |> Report.TestResult.wrap_error ~name:filename

let usage =
  Printf.sprintf
    "Usage: %s [OPTIONS] [DIRECTORIES OR TEST FILES]\n"
    Sys.argv.(0)

let () =
  let files = ref [] in
  let cfgr = ref Run.Configuration.{ command = find_flap (); } in
  let verbose = ref false in
  let excluded = ref [] in
  let only = ref `All in
  let parallel = ref true in
  let html = ref false in
  Arg.parse
    Arg.(align
           [
             "-v", Set verbose, " Be more verbose";
             "--exclude", String (fun s -> excluded := s :: !excluded),
             " Exclude directories matching this regular expression";
             "--mode", Symbol (["seq"; "par"],
                               function "seq" -> parallel := false
                                      | _ -> ()),
             " Run in sequential or parallel (default) mode";
             "--only", Symbol (["ok"; "ko"; "all"; "none"],
                               function "ok" -> only := `Ok
                                      | "ko" -> only := `Ko
                                      | "none" -> only := `None
                                      | _ -> ()),
             " Only print test results matching";
             "--html", Set html,
             " Write test results to HTML file";
           ] @ Run.Configuration.command_line_options cfgr)
    (fun s -> files := s :: !files)
    usage;
  if !flap = "" then flap := find_flap ();
  let map = if !parallel
            then Utils.Parallel.map ~grain:2 Report.TestResult.empty
            else List.map in
  let excluded = List.map Re.(fun s -> Pcre.re s |> compile) !excluded in
  files := List.rev !files;
  begin match !files with
  | [] -> Printf.eprintf "%s" usage; exit 1
  | _ :: _ ->
     let rec walk p =
       let open Unix in
       match lstat p, Filename.check_suffix p ".test", p with
       | { st_kind = S_REG; _ }, true, _ ->
          [p]
       | { st_kind = S_DIR; _ }, _, _
            when List.for_all (fun r -> not (Re.execp r p)) excluded ->
          Path.contents p
          |> List.map (Filename.concat p)
          |> List.concat_map walk
       | _ ->
          []
     in
     let report =
       List.concat_map walk (!files)
       |> map (run_test !cfgr)
       |> Report.make
     in
     if !html
     then dump_html report
     else
       Report.pp ~verbose:(!verbose) ~only:(!only) report
       |> PPrint.ToChannel.pretty 1.0 80 stdout;
     exit (if Report.all_good report then 0 else 1)
  end
