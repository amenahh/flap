type t = string

let exists dir = try Sys.is_directory dir with _ -> false

let contents dir =
  match Unix.opendir dir with
  | d ->
     let rec loop () =
       begin match Unix.readdir d with
       | "." | ".." -> loop ()
       | p -> p :: loop ()
       | exception End_of_file -> []
       end
     in
     let xs = loop () in
     Unix.closedir d;
     xs
  | exception (Unix.Unix_error (e, funname, param)) ->
     failwith
       (Printf.sprintf "%s(%s): %s" funname param (Unix.error_message e))

let rec all_files dir =
  contents dir
  |> List.concat_map begin fun entry ->
         let path = Filename.concat dir entry in
         let st = Unix.lstat path in
         match st.Unix.st_kind with
         | Unix.S_REG -> [path]
         | Unix.S_DIR -> all_files dir
         | _ -> []
       end

let find_upwards dir =
  let open Filename in
  let rec loop current_dir =
    let tentative_dir = concat current_dir dir in
    if exists tentative_dir then tentative_dir
    else
      let parent_dir =
        Unix.realpath @@ concat current_dir parent_dir_name in
      if parent_dir = current_dir
      then failwith ("could not find " ^ dir)
      else loop parent_dir
  in
  loop (Unix.getcwd ())
