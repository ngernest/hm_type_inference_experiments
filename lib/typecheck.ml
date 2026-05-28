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

(** Simply-typed lambda calculus with ints, bools and null (the unit value) *)
type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Null
  | Lambda of string * expr
  | App of expr * expr
  | If of expr * expr * expr

(** A typing context is a map from variable names to types, implemented as an 
    association list *)
type context = (string * typ) list

(** Helper function: extends a context [ctx] with a new binding [x : ty].    
    If a binding for [x] already exists in [ctx], that binding is overwritten
    with the new binding [x : ty]. *)
let extend_ctx (ctx : context) (x : string) (ty : typ) : context =
  let new_ctx = List.filter (fun (x', _) -> not (String.equal x' x)) ctx in
  List.rev ((x, ty) :: new_ctx)

(** Exception to be thrown when:
    - a variable doesn't exist in a typing context
    - two types can't be unified 
    - a type can't be inferred for an expression *)
exception TypeError of string

(** Mutable variable storing the (integer) value of the next 
    fresh type variable *)
let next_var_id : int ref = ref 0

(** Generates a fresh type variable *)
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
  | TVar n ->
    (* 'a, 'b, ... *)
    "'" ^ String.make 1 (Char.chr (97 + (n mod 26)))

(** Helper function: pretty-prints an [expr] as a string *)
let rec string_of_expr (e : expr) : string =
  match e with
  | Var x -> x
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Null -> "null"
  | Lambda (x, e') -> Printf.sprintf "λ%s. %s" x (string_of_expr e')
  | App (e1, e2) ->
    let s1 = string_of_expr e1 in
    let s2 = string_of_expr e2 in
    Printf.sprintf "(%s %s)" s1 s2
  | If (e1, e2, e3) ->
    let s1 = string_of_expr e1 in
    let s2 = string_of_expr e2 in
    let s3 = string_of_expr e3 in
    Printf.sprintf "if %s then %s else %s" s1 s2 s3

(** Helper function: pretty-prints a context *)
let rec string_of_ctx (ctx : context) : string =
  match ctx with
  | [] -> "∅"
  | (x, ty) :: ctx' ->
    Printf.sprintf "%s : %s, %s" x (string_of_type ty) (string_of_ctx ctx')

(** Checks if a type variable occurs in a type *)
let rec occurs (var_id : int) (t : typ) : bool =
  match t with
  | TVar n -> n = var_id
  | TFun (t1, t2) -> occurs var_id t1 || occurs var_id t2
  | _ -> false

(** A substitution is an association list that maps type variables (ints) to 
    types *)
type sub = (int * typ) list

(** Applies a substitution to a type: [apply_subst subst t] replaces
    all type variables inside the type [t] with the result of 
    the substitution [subst] *)
let rec apply_subst (subst : sub) (t : typ) : typ =
  match t with
  | TVar n -> ( try List.assoc n subst with Not_found -> t)
  | TFun (t1, t2) -> TFun (apply_subst subst t1, apply_subst subst t2)
  | _ -> t

(** Composes two substitutions: [compose_subst s1 s2] applies 
    [s1] to every type in the image of [s2], then unions [s1, s2] together.
    - If the same type variable [var] appears in both [s1] and [s2], the binding 
    for [var] in s1 is preserved. *)
let compose_subst (s1 : sub) (s2 : sub) : sub =
  let s2' = List.map (fun (var, t) -> (var, apply_subst s1 t)) s2 in
  s1 @ List.filter (fun (var, _) -> not (List.mem_assoc var s1)) s2'

(** For brevity, we define an infix operator for [compose_subst], 
    where [s1 <.> s2] means [compose_subst s1 s2]. Note that [<.>]
    is left-associative.  *)
let ( <.> ) (s1 : sub) (s2 : sub) : sub = compose_subst s1 s2

(* TODO: implement unification *)

(** Implementation of Robinson's unification algorithm. Returns a substitution 
    that most weakly unifies the constraint (t1 = t2) for types t1, t2. *)
let rec unify (t1 : typ) (t2 : typ) : sub =
  match (t1, t2) with
  | TInt, TInt | TBool, TBool | TUnit, TUnit -> []
  | TVar x1, TVar x2 when x1 = x2 -> []
  | TVar x, tau ->
    if not (occurs x tau) then [ (x, tau) ]
    else
      raise
        (TypeError
           (Printf.sprintf "Type variable %d appears in %s\n" x
              (string_of_type tau)))
  | tau, TVar x ->
    if not (occurs x tau) then [ (x, tau) ]
    else
      raise
        (TypeError
           (Printf.sprintf "Type variable %d appears in %s\n" x
              (string_of_type tau)))
  | TFun (tau1, tau2), TFun (tau1', tau2') ->
    let s1 = unify tau1 tau1' in
    let s2 = unify (apply_subst s1 tau2) (apply_subst s1 tau2') in
    compose_subst s2 s1
  | _ ->
    let s1 = string_of_type t1 in
    let s2 = string_of_type t2 in
    raise (TypeError (Printf.sprintf "No solution for %s = %s\n" s1 s2))

(** Looks-up a variable in the context, returning its type.
    Raises [TypeError] if the variable is not found. *)
let rec lookup (ctx : context) (x : string) : typ =
  match ctx with
  | [] -> raise (TypeError ("Unbound variable: " ^ x))
  | (y, t) :: rest -> if x = y then t else lookup rest x

(** Infers the type of expression e in context ctx via unification.
 *  Returns a pair (t, s) where:
 *  - t is the inferred type of e
 *  - s is a substitution containing all the constraints that must be 
 *    satisfied for e to be of type t.
 *)
let rec infer (ctx : context) (e : expr) : typ * sub =
  match e with
  | Bool _ -> (TBool, [])
  | Int _ -> (TInt, [])
  | Null -> (TUnit, [])
  | Var x -> (lookup ctx x, [])
  | App (e0, e1) ->
    let tau0, s0 = infer ctx e0 in
    let tau1, s1 = infer ctx e1 in
    let t = fresh_var () in
    let s01 = s1 <.> s0 in
    let s2 = unify (apply_subst s01 tau0) (TFun (apply_subst s01 tau1, t)) in
    (apply_subst s2 t, s2 <.> s01)
  | Lambda (x, e) ->
    let t = fresh_var () in
    let extended_ctx = extend_ctx ctx x t in
    let tau', s = infer extended_ctx e in
    (TFun (apply_subst s t, tau'), s)
  | If (e1, e2, e3) ->
    let t1, s1 = infer ctx e1 in
    let t2, s2 = infer ctx e2 in
    let t3, s3 = infer ctx e3 in
    let s123 = s3 <.> s2 <.> s1 in
    let s4 = unify (apply_subst s123 t1) TBool in
    let s1234 = s4 <.> s123 in
    let s5 = unify (apply_subst s1234 t2) (apply_subst s1234 t3) in
    let final_subst = s5 <.> s1234 in
    (apply_subst final_subst t2, final_subst)

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
let null = Null
let int5 = Int 5
let int42 = Int 42
let x = Var "x"
let y = Var "y"
let id = Lambda ("x", Var "x")
let if_int = If (tru, Int 42, Int 5)

(** This term is [if_fun = if true then λx. x else λx. x]. 
    Our code infers the type ['b -> 'b] for this term. 
    This is equivalent to the type that OCaml infers for [if_fun_ocaml], 
    which is ['a -> 'a]. *)
let if_fun = If (tru, id, id)

let id_ocaml x = x
let if_fun_ocaml = if true then id_ocaml else id_ocaml

(** This term is [foo1 = λf. λg. λx. f (g x)].         
    The inferred type is [('d -> 'e) -> ('c -> 'd) -> 'c -> 'e], 
    which is equivalent to the type that OCaml infers 
    for the term [foo1_ocaml] below, that is 
    [('a -> 'b) -> ('c -> 'a) -> 'c -> 'b]. *)
let foo1 : expr =
  Lambda ("f", Lambda ("g", Lambda ("x", App (Var "f", App (Var "g", Var "x")))))

let foo1_ocaml f g x = f (g x)

(** Helper function for running the unit tests below:
    - Resets [next_var_id := 0] when typechecking a new term
    - If the term typechecks succesfully, prints out [ctx ⊢ e : ty], 
      using the dedicated pretty-printing functions for 
      [expr], [context] and [typ] *)
let top_level_typechecker (ctx : context) (e : expr) : unit =
  next_var_id := 0;
  let ty = typecheck ctx e in
  Printf.eprintf "%s ⊢ %s : %s\n" (string_of_ctx ctx) (string_of_expr e)
    (string_of_type ty)

(** For each of the example terms above, infer a type for them in the empty 
    context and typechecks the term, printing out the final type. 
    - Note: [x] and [y] are omitted since they can't be typechecked in the
      in the empty context  *)
let run_tests () =
  List.iter (top_level_typechecker [])
    [ tru; fls; null; int5; int42; id; if_int; if_fun; foo1 ]
