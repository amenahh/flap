open Ppx_yojson_conv_lib.Yojson_conv
open Primitives

type expectation =
  | Regex of string
  | Expect of { filename : string [@default ""] }
  | Anything
  [@@deriving yojson]

type t =
  {
    name : string; [@default ""]
    directory : string; [@default ""]
    description : string; [@default ""]
    source : string;
    arguments : string list;
    exitcode : int;
    expectation : expectation; [@default Anything]
    tags : string list;
  } [@@deriving yojson]

let from_file filename =
  let filename_normalized =
    let default = Filename.basename filename in
    Filename.chop_suffix_opt ~suffix:".test" default
    |> Option.value ~default
  in
  try
    let t = open_in filename |> Yojson.Safe.from_channel |> t_of_yojson in
    let directory =
      if t.directory = "" then Filename.dirname filename else t.directory
    in
    let name = if t.name = "" then filename_normalized else t.name in
    let expectation =
      match t.expectation with
      | Expect { filename = ""; } -> Expect { filename = name ^ ".expected"; }
      | Regex _ | Anything | Expect _ -> t.expectation
    in
    Result.Ok { t with directory; name; expectation; }
  with
  | Sys_error _ ->
     Result.Error PPrint.(!^ "Could not open file")
  | Yojson.Json_error reason ->
     Result.Error PPrint.(
      prefix 2 1 (!^ "Could not read JSON file:") (!^ reason)
     )
  | Of_yojson_error (Failure reason, _) ->
     Result.Error PPrint.(
      prefix 2 1 (!^ "Wrong JSON format:") (!^ reason)
     )

type _t = t
module Set =
  Set.Make(struct
             type t = _t
             let compare c1 c2 = Stdlib.compare c1.name c2.name
           end)
