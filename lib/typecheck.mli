(** Syntax of types: int, bool, unit, function types and type variables
    (indexed with natural numbers) *)
type typ =
  | TInt
  | TBool
  | TUnit
  | TFun of typ * typ
  | TVar of int

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
val extend_ctx : context -> string -> typ -> context

(** Exception to be thrown when:
    - a variable doesn't exist in a typing context
    - two types can't be unified 
    - a type can't be inferred for an expression *)
exception TypeError of string

(** Mutable variable storing the (integer) value of the next 
    fresh type variable *)
val fresh_var : unit -> typ

(** converts a type to string for display *)
val string_of_type : typ -> string

(** Checks if a type variable occurs in a type *)
val occurs : int -> typ -> bool

(** A substitution is an association list that maps type variables (ints) to 
    types *)
type sub = (int * typ) list

(** Applies a substitution to a type: [apply_subst subst t] replaces
    all type variables inside the type [t] with the result of 
    the substitution [subst] *)
val apply_subst : sub -> typ -> typ

(** Composes two substitutions: [compose_subst s1 s2] applies 
    [s1] to every type in the image of [s2], then unions [s1, s2] together.
    - If the same type variable [var] appears in both [s1] and [s2], the binding 
    for [var] in s1 is preserved. *)
val compose_subst : sub -> sub -> sub

(** Implementation of Robinson's unification algorithm. Returns a substitution 
    that most weakly unifies the constraint (t1 = t2) for types t1, t2. *)
val unify : typ -> typ -> sub

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
