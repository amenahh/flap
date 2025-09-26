module Render = struct
  type t =
    Pack : 'a * ('a -> PPrint.document) * ('a -> Pure_html.node) -> t

  let make ~text ~html value = Pack (value, text, html)

  let of_string s =
    make ~text:PPrint.string ~html:(fun s -> Pure_html.txt "%s" s) s

  let text_of (Pack (value, text, _)) = text value

  let html_of (Pack (value, _, html)) = html value

  let nothing =
    make
      ~text:(fun () -> PPrint.empty)
      ~html:(fun () -> Pure_html.HTML.null [])
      ()
end

module Error = struct
  type t = { kind : string; details : Render.t option; }

 let pp { kind; details; } =
   let open PPrint in
   group
     (string kind
      ^^ Utils.PPrint.option ~left:(!^ " [") ~right:(!^ "]")
           Option.(map Render.text_of details))

 let make ?details kind = { kind; details; }

 let html_of { kind; details; } =
   let open Pure_html in
   HTML.p [] [
       HTML.h6 [] [txt "%s" kind];
       Option.fold ~none:HTML.(null []) ~some:Render.html_of details;
     ]
end

module Status = struct
  type t =
    | Ok                          (** Test succeeded. *)
    | Ko of Error.t               (** Test failed. *)
    | Error of PPrint.document    (** Could not run test. *)

  let pp =
    let open PPrint in
    function
    | Ok -> string "OK"
    | Ko err -> string "KO" ^/^ Error.pp err
    | Error msg -> group (!^ "ERROR " ^/^ msg)

  let html_of =
    let open Pure_html in
    let open HTML in
    fun s ->
    let msg, css =
      match s with
      | Ok -> "OK", "color: green"
      | Ko _ -> "KO", "color: red"
      | Error _ -> "Error", "color: red"
    in
    div [style_ "font-weight: bold; %s" css] [txt "%s" msg]

  let html_details_of =
    let open Pure_html in
    let open HTML in
    function
    | Ok -> txt "-"
    | Ko err -> Error.html_of err
    | Error msg -> pre [] [txt "%s" @@ Utils.PPrint.string_of msg]
end

module TestResult = struct
  type t =
    {
      name : string;
      directory : string;
      commandline : PPrint.document;
      exitcode : int;
      input : string;
      output : string;
      status : Status.t;
      tags : string list;
    }

  let compare t1 t2 = Stdlib.compare t1.name t2.name

  let make ~name ~directory ~commandline ~exitcode ~input ~output ~tags status =
    { name; directory; commandline; exitcode; input; output; status; tags; }

  let full_name r = Filename.concat r.directory r.name

  let pp ?(verbose = false) r =
    let open PPrint in
    let result, reason, details = match r.status with
      | Status.Ok -> "OK", None, None
      | Status.Ko err -> "KO", Some PPrint.(string err.kind), err.details
      | Status.Error reason -> "ERR", Some reason, None
    in
    let header =
      group @@ begin
          Utils.PPrint.(brackets (!^ result))
          ^^ space ^^ string (full_name r)
          ^^ Utils.PPrint.option ~left:(colon ^^ break 1) reason
        end
    in
    header
    :: Option.(map Render.text_of details |> to_list)
    @ (if verbose then
         [separate hardline [
              group r.commandline;
              !^ "$? = " ^^ OCaml.int r.exitcode;
              string r.output]]
        else [])
    |> separate hardline

  let html_of r =
    let open Pure_html in
    let open HTML in
    tr [] [
        td [] [Status.html_of r.status];
        td [] [txt "%s (%s)" r.name r.directory];
        td [] [
            List.map (fun s -> li [] [txt "%s" s]) r.tags
            |> ul [];
          ];
        td [] [Status.html_details_of r.status];
        td [] [
            pre
              [style_ "overflow-y:scroll; max-width: 400px;"]
              [txt "%s" r.output]
          ];
        td [] [
            pre
              [style_ "overflow-y:scroll; max-width: 400px;"]
              [txt "%s" r.input]
          ];
      ]

  let wrap_error ?(name = "") = function
    | Result.Ok r ->
       r
    | Result.Error msg ->
       { name; directory = ""; commandline = PPrint.empty;
         exitcode = -1; input = ""; output = ""; status = Error msg;
         tags = []; }

  let empty =
    { name = ""; directory = ""; commandline = PPrint.empty;
      exitcode = -1; input = ""; output = ""; status = Status.Ok; tags = []; }
end

module Statistics = struct
  type t = { ok : int; ko : int; err : int; total : int; }

  let zero = { ok = 0; ko = 0; err = 0; total = 0; }

  let add_status s = function
    | Status.Ok ->
       { s with ok = s.ok + 1; total = s.total + 1; }
    | Status.Ko _ ->
       { s with ko = s.ko + 1; total = s.total + 1; }
    | Status.Error _ ->
       { s with err = s.err + 1; total = s.total + 1; }

  let ok_ratio { ok; total; _ } =
    float_of_int ok /. float_of_int total *. 100.

  let ko_ratio { ko; total; _ } =
    float_of_int ko /. float_of_int total *. 100.

  let err_ratio { err; total; _ } =
    float_of_int err /. float_of_int total *. 100.
end

type t =
  {
    time : Unix.tm;
    system : string;
    ocaml_version : string;
    results : TestResult.t list;
    stats : Statistics.t;
  }

let make results =
  {
    time = Unix.(time () |> localtime);
    system = Sys.os_type;
    ocaml_version = Sys.ocaml_version;
    results = List.sort TestResult.compare results;
    stats =
      List.fold_left
        (fun s r -> Statistics.add_status s r.TestResult.status)
        Statistics.zero
        results;
  }

let pp ?verbose ?(only = `All) r =
  let open PPrint in
  let prefix d =
    let open Statistics in
    let s =
      Printf.sprintf
        "[%s | %s %s (%s)] %.2f%% OK, %.2f%% KO%s (%d,%d,%d,%d)"
        Utils.Unix.(string_of_tm r.time)
        Utils.Sys.backend_type_s r.ocaml_version r.system
        (ok_ratio r.stats)
        (ko_ratio r.stats)
        (if r.stats.err > 0
         then Printf.sprintf ", %.2f%% ERROR" (err_ratio r.stats)
         else "")
        r.stats.ok r.stats.ko r.stats.err r.stats.total
    in
    string s ^^ hardline ^^ d
  in
  List.filter (fun { TestResult.status; _ } ->
      match only, status with
      | `All, _ ->
         true
      | `None, _ ->
         false
      | `Ok, Status.Ok ->
         true
      | `Ko, Status.(Ko _ | Error _) ->
         true
      | _ ->
         false)
    r.results
  |> List.map (fun d -> TestResult.pp ?verbose d ^^ hardline)
  |> concat
  |> prefix

let js_code =
{|
const get = (tr, idx) => tr.children[idx].innerText;

const comparer = (idx, asc) => (a, b) => ((v1, v2) =>
    v1 !== '' && v2 !== '' && !isNaN(v1) && !isNaN(v2)
      ? v1 - v2 : v1.toString().localeCompare(v2)
    )(get(asc ? a : b, idx), get(asc ? b : a, idx));

// do the work...
addEventListener("DOMContentLoaded", () => {
  document.querySelectorAll('#results th').forEach(
 th => th.addEventListener('click', (() => {
            const tbody = document.querySelector('#results tbody');
            Array.from(tbody.querySelectorAll('tr'))
                 .sort(comparer(Array.from(th.parentNode.children).indexOf(th),
                                this.asc = !this.asc))
                 .forEach(tr => tbody.appendChild(tr));
          })))
});
|}

let css_code = {| #results th { cursor: pointer; } |}

let html_article_of r =
  let open Statistics in
  let open Pure_html in
  let open HTML in
  article [] [
      h1 [] [txt "Test Results" ];
      article [] [
          table [] [
              tbody [] [
                  tr [] [
                      th [] [txt "Date"];
                      td [] [txt "%s" Utils.Unix.(string_of_tm r.time)];
                    ];
                  tr [] [
                      th [] [txt "OCaml version"];
                      td [] [txt "%s %s - %s"
                               Utils.Sys.backend_type_s
                               r.ocaml_version r.system];
                    ];
                  let make (name, proj) =
                    tr [] [
                        th [] [txt "%s" name];
                        td [] [txt "%2.2f%% (%d/%d)"
                                 (float_of_int (proj r)
                                  /. float_of_int r.stats.total *. 100.0)
                                 (proj r) r.stats.total];
                      ]
                  in
                  null (List.map make ["OK tests", (fun r -> r.stats.ok);
                                       "KO tests", (fun r -> r.stats.ko);
                                       "ERROR tests", (fun r -> r.stats.err)]);
                ];
            ];
        ];
      table [id "results"] [
          thead [] [
              tr [] [
                  th [] [txt "Status"];
                  th [] [txt "Test"];
                  th [] [txt "Tags"];
                  th [] [txt "Détails"];
                  th [] [txt "Sortie"];
                  th [] [txt "Entrée"];
                ];
            ];
          tbody [] [
              null @@ List.map TestResult.html_of r.results;
            ];
        ]
    ]

let html_full_page contents =
  let open Pure_html in
  let open HTML in
  let css = [
      "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css";
      "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.colors.min.css";
    ]
  in
  html [lang "en"] [
      head [] [
          meta [charset "utf-8"];
          meta [name "viewport";
                content "width=device-width, initial-scale=1"];
          meta [name "color-scheme"; content "light dark"];
          List.map (fun h -> link [rel "stylesheet"; href "%s" h])
            css |> null;
          script [defer] "%s" js_code;
          style [] "%s" css_code;
        ];
      body [] [
          main [class_ "container"] contents;
        ];
    ]

let html_of ?(mode = `Full) r =
  let art = html_article_of r in
  match mode with `Full -> html_full_page [art] | `Article -> art

let all_good r =
  r.stats.ok = r.stats.total
