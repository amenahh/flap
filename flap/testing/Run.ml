module Configuration = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

  type t =
    {
      command : string;
    } [@@deriving yojson]

  let command_line_options cfgr =
    [
      "-cfg-command",
      Arg.String (fun command -> cfgr := { command; }),
      " set command to test";
    ]
end

open Configuration

type matcher = string -> Report.Render.t option

let matcher t : matcher =
  match t.Test.expectation with
  | Test.Regex re_s ->
     let re = Re.(Pcre.re ~flags:[`MULTILINE] re_s |> compile) in
     let s =
       Printf.sprintf "Could not match the regular expression \"%s\""
         (String.escaped re_s)
     in
     let doc = Report.Render.make
                 ~text:(fun () -> PPrint.string s)
                 ~html:(fun () ->
                   let open Pure_html in
                   HTML.null [
                       txt "Could not match regular expression.";
                       HTML.pre [] [txt "%s" String.(escaped re_s)]]) () in
     fun s ->
     if Re.execp re s then None else Some doc
  | Test.Expect { filename; } ->
     let expected_contents = Filename.concat t.Test.directory filename
                             |> Utils.slurp_file in
     fun contents ->
     let hunks = Utils.Diff.string ~expected:expected_contents ~contents in
     if List.for_all Patience_diff_lib.Patience_diff.Hunk.all_same hunks
     then None
     else Some (let text hunks =
                  Utils.Diff.to_string ~output:Ansi hunks
                  |> PPrint.string in
                let html hunks =
                  Utils.Diff.to_string ~output:Html hunks
                  |> Pure_html.txt ~raw:true "%s" in
                Report.Render.make ~text ~html hunks)
  | Anything ->
     fun _ -> None

let run cfg t =
  let open Test in
  let matcher = matcher t in
  let input_file_name = Filename.concat t.directory t.source in
  let input = Utils.slurp_file input_file_name in
  let commandline = t.arguments @ [input_file_name] in
  let exitcode, output = Utils.slurp_command_output cfg.command commandline in
  let status =
    let open Report in
    if exitcode <> t.exitcode
    then Status.Ko Error.(make
                            ~details:Render.(of_string output)
                            "exit code mismatch")
    else
      begin match matcher output with
      | None -> Status.Ok
      | Some details -> Status.Ko Error.(make ~details "wrong output")
      end
  in
  Report.TestResult.make
    ~name:t.name
    ~directory:t.directory
    ~commandline:PPrint.(prefix 2 1 (string cfg.command)
                           (group @@ separate_map (break 1) string commandline))
    ~exitcode
    ~input
    ~output
    ~tags:t.tags
    status
