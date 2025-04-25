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
let (<.>) (s1 : sub) (s2 : sub) : sub = compose_subst s1 s2  

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
  | TFun (tau1, tau2), TFun (tau1', tau2') ->
    (* TODO: need to check this case *)
    compose_subst (unify tau1 tau1') (unify tau2 tau2')
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

(* TODO: implement inference *)

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
    (* Generate a fresh variable [T] *)
    let t = fresh_var () in
    (* Unify the equation [τ₀ = τ₁ -> T] *)
    let s2 = unify tau0 (TFun (tau1, t)) in
    (* TODO: not sure if combining all the substs in this way is right *)
    let final_subst = compose_subst s2 (compose_subst s1 s0) in 
    (t, final_subst)
  | Lambda (x, e) ->
    (* Generate a fresh type variable [T], then infer a type for the body [e] in
       the extended context [ctx, x : T] *)
    let t = fresh_var () in
    let extended_ctx = extend_ctx ctx x t in
    let tau', s = infer extended_ctx e in
    (TFun (t, tau'), s)
  | If (e1, e2, e3) ->
    let (t1, s1) = infer ctx e1 in 
    let (t2, s2) = infer ctx e2 in 
    let (t3, s3) = infer ctx e3 in 
    let s4 = unify t1 TBool in 
    let s5 = unify t2 t3 in 
    let final_subst = s5 <.> s4 <.> s3 <.> s2 <.> s1 in 
    (t2, final_subst)

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
let _tru = Bool true
let _fls = Bool false
let _int5 = Int 5
let _int42 = Int 42
let _x = Var "x"
let _y = Var "y"
let _id = Lambda ("x", Var "x")
let _if_int = If (_tru, Int 42, Int 5)
let _if_fun = If (_tru, _id, _id)

let _foo1 =
  Lambda ("f", Lambda ("g", Lambda ("x", App (Var "f", App (Var "g", Var "x")))))
