type typ =
  | TInt
  | TBool
  | TUnit
  | TFun of typ * typ
  | TVar of int

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

(** Generates a fresh type variable *)
val fresh_var : unit -> typ

(** converts a type to string for display *)
val string_of_type : typ -> string

(** checks if a type variable occurs in a type *)
val occurs : int -> typ -> bool

type sub = (int * typ) list

(** applies a substitution to a type *)
val apply_subst : sub -> typ -> typ

(* composes two substitutions *)
val compose_subst : sub -> sub -> sub

(** Implementation of Robinson's unification algorithm. Returns a substitution 
    that most weakly unifies the constraint (t1 = t2) for types t1, t2. *)
val unify : 'a -> 'b -> sub

(** Looks-up a variable in the context, returning its type.
    Raises [TypeError] if the variable is not found. *)
val lookup : context -> string -> typ

(** Infers the type of expression e in context ctx via unification.
 *  Returns a pair (t, s) where:
 *  - t is the inferred type of e
 *  - s is a substitution containing all the constraints that must be 
 *    satisfied for e to be of type t.
 *)
val infer : context -> expr -> typ * sub

(** Main typechecking function that returns the inferred type *)
val typecheck : context -> expr -> typ
