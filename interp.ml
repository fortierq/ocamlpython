
open Ast
open Format

(* Exception levée pour signaler une erreur pendant l'interprétation *)
exception Error of string
let error s = raise (Error s)

(* Les valeurs de Mini-Python

   - une différence notable avec Python : on
     utilise ici le type int alors que les entiers de Python sont de
     précision arbitraire ; on pourrait utiliser le module Big_int d'OCaml
     mais on choisit la facilité
   - ce que Python appelle une liste est en réalité un tableau
     redimensionnable ; dans le fragment considéré ici, il n'y a pas
     de possibilité d'en modifier la longueur, donc un simple tableau OCaml
     convient *)
type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

(* Affichage d'une valeur sur la sortie standard *)
let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
    printf "]"

(* Interprétation booléenne d'une valeur

   En Python, toute valeur peut être utilisée comme un booléen : None,
   la liste vide, la chaîne vide et l'entier 0 sont considérés comme
   False et toute autre valeurs comme True *)

let is_false = function
    | Vnone | Vbool false | Vint 0 | Vstring "" -> true
    | _ -> false

let is_true v = not (is_false v)

(* Les fonctions sont ici uniquement globales *)

let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

(* L'instruction 'return' de Python est interprétée à l'aide d'une exception *)

exception Return of value

(* Les variables locales (paramètres de fonctions et variables introduites
   par des affectations) sont stockées dans une table de hachage passée en
   arguments aux fonctions suivantes sous le nom 'ctx' *)

type ctx = (string, value) Hashtbl.t

(* Interprétation d'une expression (renvoie une valeur) *)

let rec expr ctx = function
  | Ecst Cnone ->
      Vnone
  | Ecst (Cstring s) ->
      Vstring s
  (* arithmétique *)
  | Ecst (Cint n) ->
    Vint n
  | Ebinop (Badd | Bsub | Bmul | Bdiv | Bmod |
            Beq | Bneq | Blt | Ble | Bgt | Bge as op, e1, e2) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      begin match op, v1, v2 with
        | Badd, Vint n1, Vint n2 -> Vint (n1 + n2)
        | Bsub, Vint n1, Vint n2 -> Vint (n1 - n2)
        | Bmul, Vint n1, Vint n2 -> Vint (n1 * n2)
        | Bdiv, Vint n1, Vint n2 -> Vint (n1 / n2)
        | Bmod, Vint n1, Vint n2 -> Vint (n1 mod n2)
        | Beq, _, _  -> Vbool (v1 = v2)
        | Bneq, _, _ -> Vbool (v1 <> v2)
        | Blt, _, _  -> Vbool (v1 < v2)
        | Ble, _, _  -> Vbool (v1 <= v2)
        | Bgt, _, _  -> Vbool (v1 > v2)
        | Bge, _, _  -> Vbool (v1 >= v2)
        | Badd, Vstring s1, Vstring s2 ->
          Vstring (s1 ^ s2)
        | Badd, Vlist l1, Vlist l2 ->
            assert false (* à compléter (question 5) *)
        | _ -> error "unsupported operand types"
      end
  | Eunop (Uneg, e1) -> (match expr ctx e1 with
    | Vint n -> Vint (-n)
    | _ -> error "unsupported operand type"
  )
  (* booléens *)
  | Ecst (Cbool b) -> Vbool b
  | Ebinop (Band, e1, e2) -> (match expr ctx e1 with
    | Vbool b1 -> Vbool (b1 &&
        match expr ctx e2 with 
          | Vbool b2 -> b2
          | _ -> error "unsupported operand type"
          ) 
    | _ -> error "unsupported operand type"
  )
  | Ebinop (Bor, e1, e2) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      begin match v1, v2 with
        | Vbool b1, Vbool b2 -> Vbool (b1 || b2)
        | _ -> error "unsupported operand type"
      end
  | Eunop (Unot, e1) -> (match expr ctx e1 with
    | Vbool b -> Vbool (not b)
    | _ -> error "unsupported operand type"
  )
  | Epipe (e, f) -> expr ctx (Ecall (f, match e with Elist el -> el | _ -> [e]))
  | Eident id ->
    Hashtbl.find ctx id
  (* appel de fonction *)
  | Ecall ("len", [e1]) -> (match expr ctx e1 with
    | Vlist l -> Vint (Array.length l)
    | _ -> error "argument to len must be a list"
  )
  | Ecall ("range", [e1]) -> (match expr ctx e1 with
    | Vint n -> Vlist (Array.init n (fun i -> Vint i))
    | _ -> error "argument to range must be an integer"
  )
  | Ecall (f, el) ->  (* à compléter (question 5) *)
      let fns = Hashtbl.find functions f in
      let (params, body) = fns in
      let ctx' = Hashtbl.copy ctx in (
      List.iter2 (fun p v -> Hashtbl.add ctx' p (expr ctx v)) params el;
      try stmt ctx' body; Vnone 
      with Return v -> v
      )
  | Elist el -> Vlist (List.map (expr ctx) el |> Array.of_list)
  | Eget (e1, e2) -> (match expr ctx e1 with
    | Vlist l -> (match expr ctx e2 with
        | Vint n -> l.(n)
        | _ -> error "index must be integer"
    )
    | _ -> error "can not index"
  )

(* interprétation d'une instruction ; ne renvoie rien *)
and stmt ctx = function
  | Seval e ->
      ignore (expr ctx e)
  | Sprint e ->
      print_value (expr ctx e); printf "@."
  | Sblock bl ->
      block ctx bl
  | Sif (e, s1, s2) ->
      if is_true (expr ctx e) then stmt ctx s1 else stmt ctx s2
  | Sassign (id, e1) ->
      Hashtbl.add ctx id (expr ctx e1)
  | Sreturn e -> raise (Return (expr ctx e))
  | Sfor (x, e, s) ->
      (match expr ctx e with
        | Vlist l -> l
        | _ -> error "for loop variable must be a list")
      |> Array.iter (fun i -> Hashtbl.add ctx x i; stmt ctx s)
  | Sset (e1, e2, e3) ->
      assert false (* à compléter (question 5) *)

(* interprétation d'un bloc i.e. d'une séquence d'instructions *)

and block ctx = function
  | [] -> ()
  | s :: sl -> stmt ctx s; block ctx sl

(* interprétation d'un fichier
   - dl est une liste de définitions de fonction (cf Ast.def)
   - s est une instruction, qui représente les instructions globales
 *)

let file (dl, s) =
  List.iter (fun d ->
    let (id, pl, s) = d in
    Hashtbl.add functions id (pl, s)
  ) dl;
  stmt (Hashtbl.create 16) s


