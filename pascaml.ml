open Printf
open Lexing

(* rappresentazione errore *)
let error_rep fname line msg = 
    printf "File \"%s\", linea %d :\n\t%s\n" fname line msg; 
    exit 1

let main () =

  let filename = if (Array.length Sys.argv = 1) then
      (Printf.printf "USAGE:\n\t%s <filename>\n" Sys.argv.(0) ; exit 0) 
  else Sys.argv.(1) in 

  let infile = open_in filename in   
        let lexbuf = Lexing.from_channel infile in
            try
        while true do
                Parser.program Lexer.token lexbuf            

        done
            with End_of_file -> exit 0
                  |Auxparser.Procedure_init_error id ->  
                          printf "File \"%s\" :\n\tRuntime error eseguendo %s: parametri.\n"  filename id
                          ; exit 0
                  | Parsing.Parse_error -> 
                          error_rep filename lexbuf.lex_curr_p.pos_lnum "Syntax error"
                  | Sys.Break -> printf "Interrotto.\n" ; exit 1

let _ = Printexc.print main ()

