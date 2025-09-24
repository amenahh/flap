let rec interactive_loop () =
  welcome_message ();
  let rec loop () =
    try
      read () |> eval |> print
    with End_of_file -> print_newline ()
       | exn ->
          Printf.printf "Error: %s\n%!" (Printexc.to_string exn);
          loop ()
  in
  loop ()

and welcome_message () =
  Printf.printf "
  ====================================================\n
   Welcome to the incredible Marthe interactive loop! \n
  ====================================================\n
"

and read () =
  prompt (); input_line stdin |> parse

and prompt () =
  Printf.printf "> %!"

and parse input =
  let lexbuf = Lexing.from_string input in
  Parser.phrase Lexer.token lexbuf

and print e =
  Printf.printf ":- %s\n%!" (Printer.string_of_exp e)

and eval e =
  e

let main = interactive_loop ()
