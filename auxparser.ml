open Printf

(* Eccezioni *)

(* Eccezione errore sugli ambienti (non utilizzata) *)
exception Enviroment_exception of string
(* Identifier non trovato *)
exception Identifier_not_found of string
exception Procedure_init_error of string

(* Definizione dei tipi di dato *)

(* Tipo Numero *)
type num = Int of int | Real of float

(* Espressioni *)
type expr = 
     And   of expr * expr
    | Div   of expr * expr
    | Ug    of expr * expr
    | Mau    of expr * expr
    | Ma    of expr * expr
    | Ident of string
    | Miu    of expr * expr
    | Mi    of expr * expr
    | Meno of expr * expr
    | Nu    of expr * expr
    | Neg   of expr
    | Num   of num
    | Or    of expr * expr
    | Piu  of expr * expr
    | Per of expr * expr

(* Istruzioni (Statements) *)
type stmt = 
     Assign of string * expr     
    | Dumpenv of unit
    | If of expr * stmt * stmt 
    | Procedure of string * expr list
    | Seq of stmt * stmt
    | Void of unit
    | While of expr * stmt
    | Write of expr
    | Writeln of expr

(* Ambiente (Enviroment) *)
and env = {
    mutable idlist : (string * num) list;
    (* identifier procedura, lista parametri, ambiente *)
    mutable proclist : (string * (string * num) list * env) list;    
    mutable stmts : stmt;
}

(* Lista di ambienti *)
type global = {
    mutable enviroments : env list;   
}

(* Dichiarazione dell'ambiente globale *)
let global_enviroment = { 
    enviroments =  [{ idlist = [] ; proclist = [] ; stmts = (Void ()) }]
}

    (* Rappresentazione in stringa di un oggetto <num>. *)
let string_of_num = function
        (Int x) -> string_of_int x
        | (Real x) -> string_of_float x

    (* Stampa una lista di variabili *)
let dump_vars idlist = 
    printf "vars ->";
    let rec aux = function
        [] -> printf "."
        | (x,y)::[] -> printf " (%s, %s). " x (string_of_num y)
        | (x,y)::rest -> printf " (%s, %s);" x (string_of_num y) ; aux rest
    in aux idlist

    (* Stampa la rappresentazione di un ambiente globale *)
let dump_env global_env =
    printf "\nenviroment stack: {\n" ;
    let rec aux = function
        [] -> printf "]\n"
        |x::[] -> printf "\t[ " ; dump_vars x.idlist ; printf "].\n"
        |x::rest -> printf "\t[ " ; dump_vars x.idlist ; printf "]; " ; aux rest
    in aux global_env.enviroments ; printf "}\n"

    (* Inserisce un ambiente in testa allo stack degli ambienti *)
let push_enviroment enviroment = 
    global_enviroment.enviroments <- enviroment::(global_enviroment.enviroments)
    
let gpe_aux_aux identifier proclist =
    let rec aux = function
        [] -> raise Not_found
        | (id,par,env)::rest -> 
                if id = identifier then (par,env) else aux rest
    in aux proclist

let gpe_aux identifier global_env =    
    let rec aux = function
        [] -> raise Not_found
        |x::[] -> gpe_aux_aux identifier x.proclist
        |x::rest -> try gpe_aux_aux identifier x.proclist 
                    with Not_found -> aux rest
    in aux global_env

    (* ritorna la coppia (parametri, ambiente) di una procedura 'identifier' *)
let get_procedure_env identifier =
    gpe_aux identifier global_enviroment.enviroments

    (* Genera la lista delle variabili. (i.e. var1,var2 : real -> [("var1", Real 0.0); ("var2", Real 0.0)] *)
let init_vars id_tipo id_list =
    let rec initvar variabili = function
        [] -> variabili
        | x::rest -> let value = if id_tipo = "int" then Int 0
                else Real 0.0 in initvar ((x, value)::variabili) rest
    in initvar [] id_list

    (* cerca identifier negli ambienti *)
let get_value_aux identifier global_env =
    let rec aux = function
        [] -> raise Not_found
        |x::[] -> List.assoc identifier x.idlist
        |x::rest -> try List.assoc identifier x.idlist
                   with Not_found -> aux rest
    in aux global_enviroment.enviroments

    (* Restituisce il valore della prima occorrenza di 'identifier' nello stack degli ambienti  *)
let get_value identifier =
    get_value_aux identifier global_enviroment



    (* Expression Evaluation  *)

let _neg = function
    (Int a) -> (Int ~-a)
    |(Real a) -> (Real ~-.a)

    (* addop *)
let _piu = function
    ((Int a), (Int b)) -> (Int (a + b))
    |((Real a), (Real b)) -> (Real (a +. b))
    |((Real a), (Int b)) -> (Real (a +. (float_of_int b)))
    |((Int a), (Real b)) -> (Real ((float_of_int a) +. b))

let _meno = function
    ((Int a), (Int b)) -> (Int (a - b))
    |((Real a), (Real b)) -> (Real (a -. b))
    |((Real a), (Int b)) -> (Real (a -. (float_of_int b)))
    |((Int a), (Real b)) -> (Real ((float_of_int a) -. b))

    (* i valori booleani sono trattati come in c -> i.e. !=0 true; 0 false *)
let _or = function
    ((Int a), (Int b)) -> if (a != 0 || b != 0) then (Int 1) else (Int 0)
    |((Real a), (Real b)) -> if (a != 0.0 || b != 0.0) then (Int 1) else (Int 0)
    |((Real a), (Int b)) -> if (a != 0.0 || b != 0) then (Int 1) else (Int 0)
    |((Int a), (Real b)) -> if (a != 0 || b != 0.0) then (Int 1) else (Int 0)

let _div = function
    ((Int a), (Int b)) -> (Int (a / b))
    |((Real a), (Real b)) -> (Real (a /. b))
    |((Real a), (Int b)) -> (Real (a /. (float_of_int b)))
    |((Int a), (Real b)) -> (Real ((float_of_int a) /. b))

let _per = function
    ((Int a), (Int b)) -> (Int (a * b))
    |((Real a), (Real b)) -> (Real (a *. b))
    |((Real a), (Int b)) -> (Real (a *. (float_of_int b)))
    |((Int a), (Real b)) -> (Real ((float_of_int a) *. b))
   
    (* vedi _or *)
let _and = function
    ((Int a), (Int b)) -> if (a != 0 && b != 0) then (Int 1) else (Int 0)
    |((Real a), (Real b)) -> if (a != 0.0 && b != 0.0) then (Int 1) else (Int 0)
    |((Real a), (Int b)) -> if (a != 0.0 && b != 0) then (Int 1) else (Int 0)
    |((Int a), (Real b)) -> if (a != 0 && b != 0.0) then (Int 1) else (Int 0)

    (* operatori di relazione (vedi _or) *)
let _ug = function
    (a,b) -> if a = b then (Int 1) else (Int 0)

let _nu = function
    (a,b) -> if a = b then (Int 0) else (Int 1)

let _mau = function
    (a,b) -> if a >= b then (Int 1) else (Int 0)

let _ma = function
    (a,b) -> if a > b then (Int 1) else (Int 0)

let _miu = function
    (a,b) -> if a <= b then (Int 1) else (Int 0)

let _mi = function
    (a,b) -> if a < b then (Int 1) else (Int 0)

    (* Expressions evaluation! Wow! *)
let evaluate expression =  
    let rec aux = function
        And (e1, e2)  -> _and (aux e1, aux e2)
        | Div (e1, e2) -> _div (aux e1, aux e2)
        | Ug (e1, e2) -> _ug (aux e1, aux e2)
        | Mau (e1, e2) -> _mau (aux e1, aux e2)
        | Ma (e1, e2) -> _ma (aux e1, aux e2)
        | Ident s -> get_value s
        | Miu (e1, e2) -> _miu (aux e1, aux e2)
        | Mi (e1, e2) -> _mi (aux e1, aux e2)
        | Meno (e1, e2) -> _meno (aux e1, aux e2)
        | Nu (e1, e2) -> _nu (aux e1, aux e2)
        | Neg e -> _neg (aux e)
        | Num n -> n
        | Or (e1, e2) -> _or (aux e1, aux e2)
        | Piu (e1, e2) -> _piu (aux e1, aux e2)
        | Per (e1, e2) -> _per (aux e1, aux e2)
    in aux expression

    (* valutazione booleana da PasCaml (0,1) a Ocaml (true, false) *)
let bool_eval expression =
    match (evaluate expression) with
    (Int n) -> n != 0
    | (Real n) -> n != 0.0 



    (* Istruzioni *)   

    (* (string * expr * env) -> unit *)
let assign_idlist identifier expression enviroment =
    let rec aux tidlist = function (* tailidlist *)      
        [] -> raise (Identifier_not_found identifier)
        |(x,y)::rest -> 
                if x = identifier then (identifier, (evaluate expression))::tidlist @ rest
                else aux((x,y)::tidlist) rest
    in enviroment.idlist <- (aux [] enviroment.idlist)

    (* (string * expr * global) -> global *)
let assign_aux identifier expression global_env =
    let rec aux = function
        [] -> raise Not_found
        |x::[] -> assign_idlist identifier expression x 
        |x::rest -> 
                try assign_idlist identifier expression x 
                with
                (Identifier_not_found identifier) -> aux rest
    in aux global_env.enviroments

let assign identifier expression =
    assign_aux identifier expression global_enviroment

(* genera gli assegnamenti per l'inizializzazione dei parametri 
let init_procedure (id, par) (fpar, env) = 
    let rec aux = function
        ([],[],st) -> st
        |(_::_, [], _) -> raise (Procedure_init_error id)
        |([], _::_, _) -> raise (Procedure_init_error id)
        |(n::ns, (var,x)::vars, st) -> aux(ns , vars, Seq(st, Assign(var, n)))
        
    in  Seq ( aux (par, fpar, Void ()), env.stmts ) 
*)

    (* Costruisce l'ambiente per la procedura 'id' con i parametri inizializzati  *)
let init_procedure_env id par =    

    let (fpar, env) = get_procedure_env id in

        let rec aux idlist = function
            ([],[]) -> idlist
            |(_::_, []) -> raise (Procedure_init_error id) 
            |([], _::_) -> raise (Procedure_init_error id)            
            |((var,x)::vs, value::ns) -> aux ([(var, (evaluate value))] @ idlist) (vs,ns)

        in { idlist = env.idlist @ (aux [] (fpar, par)) ; proclist = env.proclist ; stmts = env.stmts}


(* Funzione principale di esecuzione delle istruzioni *)
let execute statement = 
    let rec exec = function
        Void unit -> unit
        | Seq (st1, st2) -> exec(st1) ; exec(st2)
        | Assign (id, exp) -> (assign id exp)
        | While (exp, st1) as whst -> 
                if bool_eval exp then (exec(st1) ; exec(whst)) else ()
        | Write exp -> printf "%s " (string_of_num (evaluate exp))
        | Writeln exp -> printf "%s\n" (string_of_num (evaluate exp))
        | If (exp, st1, st2) -> 
                if bool_eval exp then exec(st1) else exec(st2) 
        | Procedure (id, par) ->
                global_enviroment.enviroments <- (init_procedure_env id par)::global_enviroment.enviroments;                                
                exec (List.hd global_enviroment.enviroments).stmts ;
                global_enviroment.enviroments <- List.tl global_enviroment.enviroments ;
           
        | Dumpenv unit -> dump_env global_enviroment
    in exec statement

(* Estrae le istruzioni dal primo ambiente della lista e le esegue *)
let execute_global global =    
    execute (List.hd global).stmts
    
