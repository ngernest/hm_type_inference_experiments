(* type inference for the lambda calculus using Robinson's unification
   algorithm *)

(** Syntax of types: int, bool, unit, function types and type variables
    (indexed with natural numbers) *)
type typ =
  | TInt
  | TBool
  | TUnit
  | TFun of typ * typ
  | TVar of int (* type variable for inference *)

(** Simply-typed lambda calculus with ints, bools and null *)
type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Null
  | Lambda of string * expr
  | App of expr * expr
  | If of expr * expr * expr

type context = (string * typ) list

exception TypeError of string

(* generates fresh type variable *)
let next_var_id : int ref = ref 0

let fresh_var () : typ =
  let id = !next_var_id in
  incr next_var_id;
  TVar id

(* converts a type to string for display *)
let rec string_of_type (t : typ) : string =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TFun (t1, t2) ->
    let t1_str =
      match t1 with
      | TFun (_, _) -> "(" ^ string_of_type t1 ^ ")"
      | _ -> string_of_type t1 in
    t1_str ^ " -> " ^ string_of_type t2
  | TVar n -> "'" ^ String.make 1 (Char.chr (97 + (n mod 26)))
(* 'a, 'b, ... *)

(** checks if a type variable occurs in a type *)
let rec occurs (var_id : int) (t : typ) : bool =
  match t with
  | TVar n -> n = var_id
  | TFun (t1, t2) -> occurs var_id t1 || occurs var_id t2
  | _ -> false

(** represent substitutions as lists of bindings (pairs) btwn type variables
   (ints) and types *)
type sub = (int * typ) list

(** applies a substitution to a type *)
let rec apply_subst (subst : sub) (t : typ) : typ =
  match t with
  | TVar n -> ( try List.assoc n subst with Not_found -> t)
  | TFun (t1, t2) -> TFun (apply_subst subst t1, apply_subst subst t2)
  | _ -> t

(** composes substitutions *)
let compose_subst (s1 : sub) (s2 : sub) : sub =
  let s2' = List.map (fun (var, t) -> (var, apply_subst s1 t)) s2 in
  s1 @ List.filter (fun (var, _) -> not (List.mem_assoc var s1)) s2'

(* TODO: implement unification *)

(** Implementation of Robinson's unification algorithm. Returns a substitution 
    that most weakly unifies the constraint (t1 = t2) for types t1, t2. *)
let rec unify t1 t2 : sub = failwith "not implemented"

(** Looks-up a variable in the context, returning its type.
    Raises [TypeError] if the variable is not found. *)
let rec lookup (ctx : context) (x : string) : typ =
  match ctx with
  | [] -> raise (TypeError ("Unbound variable: " ^ x))
  | (y, t) :: rest -> if x = y then t else lookup rest x

(* TODO: implement inference *)

(** Infers the type of expression e in context ctx via unification.
 *  Returns a pair (t, s) where:
 *  - t is the inferred type of e
 *  - s is a substitution containing all the constraints that must be 
 *    satisfied for e to be of type t.
 *)
let rec infer (ctx : context) (e : expr) : typ * sub =
  failwith "not implemented"

(** Main typechecking function that returns the inferred type *)
let typecheck (ctx : context) (e : expr) : typ =
  try
    let t, s = infer ctx e in
    apply_subst s t
  with TypeError msg ->
    Printf.eprintf "Type error: %s\n" msg;
    raise (TypeError msg)

(************************************************************)
(* Some sample expressions to use and evaluate              *)
(************************************************************)

(* in utop: #use "typecheck.ml";; *)
let tru = Bool true
let fls = Bool false
let int5 = Int 5
let int42 = Int 42
let x = Var "x"
let y = Var "y"
let id = Lambda ("x", Var "x")
let if_int = If (tru, Int 42, Int 5)
let if_fun = If (tru, id, id)

let foo1 =
  Lambda ("f", Lambda ("g", Lambda ("x", App (Var "f", App (Var "g", Var "x")))))
