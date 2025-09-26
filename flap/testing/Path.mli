type t = string

val exists : t -> bool

(* [contents dir] returns the sequence of files and directories found in the
   directory [dir]. The order in which the results are listed is unspecified.

   Raises {! Failure} in case of error. *)
val contents : t -> t List.t

(* [all_files dir] returns the sequence of regular files found in the directory
   [dir] as well as all its subdirectories. The order in which the results are
   listed is unspecified.

   Raises {! Failure} in case of error.

   This function does not traverse symbolic links. *)
val all_files : t -> t List.t

(** [find_upwards dir] looks for the closest parent directory of the current
    directory that contains [dir] as a subdirectory.

    Raises {! Failure} if no such directory exists. *)
val find_upwards : string -> t
