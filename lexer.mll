{
    open Parser

(* 
Struttura dati nel modulo Lexing posizione nel file.

type position = {
     pos_fname : string;    (* nome file *)
     pos_lnum : int;		(* numero linea *)
     pos_bol : int;		    
     pos_cnum : int;		
  } 
*)
    
    (* incrementa il numero di linea (vedi lexer tutorial) *)
    let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

}

rule token = parse  
    [' ' '\t']        { token lexbuf }     (* ignora gli spazi *)
    |'\n'             { incr_linenum lexbuf; token lexbuf } (* incrementa numero di linea *)

    (* letterali *)
    | '-'?['0'-'9']+ as l                 { L_INT(int_of_string l) }
    | '-'?['0'-'9']+'.'['0'-'9']+ as l    { L_REAL(float_of_string l) }

    (* simboli e operatori *)
    | "and" { AND }
    | ":="  { ASS }
    | '/'   { DIV }
    | '='   { UG }
    | ">="  { MAU }
    | '>'   { MA }
    | "<="  { MIU }
    | '<'   { MI }
    | '-'   { MENO }
    | "<>"  { NU }
    | "or"  { OR }
    | '+'   { PIU }
    | '*'   { PER }
    
    | '('   { SPAREN }
    | ')'   { DPAREN }

    | ':'   { DUEP }
    | ','   { VIRG }
    | '.'   { PUN }
    | ';'   { PVIRG }

    (* keywords *)
    | "begin"   { BEGIN }
    | "do"      { DO }
    | "dumpenv" { DUMPENV }
    | "else"    { ELSE }    
    | "end"     { END }
    | "if"      { IF }
    | "int"     { INT }
    | "procedure" { PROCEDURE }
    | "program" { PROGRAM }
    | "real"    { REAL }
    | "then"    { THEN }
    | "while"   { WHILE }   
    | "var"     { VAR }

    (* built-in *)
    | "write"        { WRITE }
    | "writeln"      { WRITELN }

    (* identifier *)
    | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as i           { IDENTIFIER i }

    | _         { Printf.printf "Token non riconosciuto.\n" ; exit 0 }
    | eof       { EOF }
