let slurp_in_channel ic =
  let b = Buffer.create 4096 in
  let rec loop () =
    match Buffer.add_channel b ic 4096 with
    | () -> loop ()
    | exception End_of_file -> Buffer.contents b
  in
  loop ()

let slurp_in_channel_and_close ic =
  let contents = slurp_in_channel ic in
  close_in ic;
  contents

let slurp_file filename =
  open_in filename |> slurp_in_channel_and_close

let slurp_fd_and_close fd =
  Unix.in_channel_of_descr fd |> slurp_in_channel_and_close

let slurp_command_output command commandline =
  let commandline = Array.of_list commandline in
  let p_r, p_w = Unix.pipe () in
  let pid = Unix.create_process command commandline Unix.stdin p_w p_w in
  let exitcode =
    let open Unix in
    match waitpid [] pid with
    | _, WEXITED code -> code
    | _, (WSIGNALED _ | WSTOPPED _) -> 255
  in
  Unix.close p_w;
  exitcode, slurp_fd_and_close p_r

module PPrint = struct
  open PPrint
  let brackets d = surround 2 0 (!^ "[") (group d) (!^ "]")
  let option ?(left = empty) ?(right = empty) = function
    | Some d -> left ^^ d ^^ right
    | None -> empty

  let string_of ?(width = 80) doc =
    let b = Buffer.create 4096 in
    PPrint.ToBuffer.pretty 1.0 width b doc;
    Buffer.contents b
end

module Unix = struct
  include Unix
  let string_of_tm t =
    Printf.sprintf "%.4d-%.2d-%.2d %.2d:%.2d:%.2d"
      (1900 + t.tm_year) (1 + t.tm_mon) t.tm_mday
      t.tm_hour t.tm_min t.tm_sec
end

module Sys = struct
  include Sys
  let backend_type_s =
    match backend_type with
    | Native -> "ocamlopt"
    | Bytecode -> "ocamlc"
    | Other c -> c
end

module Diff = struct
  open Patdiff

  let string ~expected ~contents =
    Patdiff_core.diff
      ~context:Patdiff_core.default_context
      ~line_big_enough:Patdiff_core.default_line_big_enough
      ~keep_ws:false
      ~find_moves:true
      ~prev:[| contents |]
      ~next:[| expected |]

  let to_string
        ?(expected = "expected output") ?(actual = "actual output") ~output
        hunks =
    Patdiff_core.refine
      ~rules:Patdiff_kernel.Format.Rules.default
      ~produce_unified_lines:true ~output
      ~keep_ws:false ~split_long_lines:false ~interleave:true
      ~word_big_enough:80
      hunks
    |> Patdiff_core.output_to_string
         ~print_global_header:false
         ~file_names:(Fake actual, Fake expected)
         ~rules:Patdiff_kernel.Format.Rules.default
         ~location_style:Diff
         ~output
end

module Parallel = struct
  open Domainslib

  let map ?(grain = 1) default f xs =
    assert (grain >= 1);
    let pool =
      Task.setup_pool ~num_domains:Domain.(recommended_domain_count ()) ()
    in
    let a = Array.of_list xs in
    let l = Array.length a in
    let b = Array.make l default in

    let rec loop ~lo ~hi () =
      assert (lo <= hi);
      if hi - lo <= grain
      then
        for i = lo to hi - 1 do
          b.(i) <- f a.(i);
        done
      else
        let m = (hi + lo) / 2 in
        let r = Task.async pool (loop ~lo:m ~hi) in
        loop ~lo ~hi:m ();
        Task.await pool r
    in

    Task.run pool (fun () -> loop ~lo:0 ~hi:l ());
    Task.teardown_pool pool;
    Array.to_list b
end
