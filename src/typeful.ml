(****************************************************************)
(* Simple types, annotated by OCaml types                       *)
(****************************************************************)

(* *Deep* embedding of types:

<<<
    τ, σ ::= Ω         (* base type *)
           | τ → σ     (* arrrow type *)
>>>
 *)

(* Use a GADT to relate the syntactic [tp] with OCaml's types *)

(* sujet
type 'a tp = (* NYI *)
  |
   /sujet *)
(* corrige *)
type 'a tp =
  | Base : base tp
  | Arr : 'a tp * 'b tp -> ('a -> 'b) tp

(* /corrige *)
and base = Atom of base at

(* = Ω *)

(* In particular, we want to be ensure that an arrow in [tp] will
   correspond to an OCaml function during reflection. *)

(****************************************************************)
(* Typed source language                                        *)
(****************************************************************)

(* *Deep* embedding of terms using weak/parametric higher-order
   abstract syntax (HOAS):

<<<
    t, u ::= x
           | λ x. t
           | t u
>>>
 *)

(* Use a GADT to make sure that we can only write well-typed terms. *)

(* sujet
and 'a tm = (* NYI *)
  |
   /sujet *)

(* corrige *)
and 'a tm =
  | Var : 'a var -> 'a tm
  | Lam : ('a var -> 'b tm) -> ('a -> 'b) tm
  | App : ('a -> 'b) tm * 'a tm -> 'b tm

(* /corrige *)

(****************************************************************)
(* Intermediate language of values, typed                       *)
(****************************************************************)

(* *Shallow* embedding of values in weak-head normal form:

<<<
    v ::= λ x. v
        | base

>>>
 *)

(* Use a GADT to make sure that we can only write well-typed values. *)

(* sujet
and 'a vl = VFun : (* NYI *) -> 'nyi vl
          | VBase : base -> base vl
/sujet *)

(* corrige *)
and 'a vl =
  | VFun : ('a vl -> 'b vl) -> ('a -> 'b) vl
  | VBase : base -> base vl

(* /corrige *)

(****************************************************************)
(* Typed target language: β-normal, η-long λ-terms              *)
(****************************************************************)

(* Use a GADT to make sure that we can only write β-normal, η-long
   terms. *)

(* sujet
and 'a nf = (* NYI *)
  |
and 'a at = (* NYI *)
  |
/sujet *)

(* corrige *)
and 'a nf =
  | NLam : ('a y -> 'b nf) -> ('a -> 'b) nf
  | NAt : base at -> base nf

and 'a at =
  | AApp : ('a -> 'b) at * 'a nf -> 'b at
  | AVar : 'a y -> 'a at

(* /corrige *)
and 'a y

and 'a var = 'a vl

(****************************************************************)
(* Examples                                                     *)
(****************************************************************)

(* Define [typ1] as [Ω → Ω] *)

(* sujet
let typ1 () = failwith "NYI"
   /sujet *)

(* corrige *)
let typ1 () = Arr (Base, Base)

(* /corrige *)

(* Define [typ2] as [(Ω → Ω) → Ω → Ω] *)

(* sujet
let typ2 () = failwith "NYI"
   /sujet *)

(* corrige *)
let typ2 () = Arr (Arr (Base, Base), Arr (Base, Base))

(* /corrige *)

(* Define [tm1] as [λ x. x] *)

(* sujet
let tm1 () = failwith "NYI"
   /sujet *)

let tm1 () = Lam (fun x -> Var x)

(* Define [tm2] as [λ f. λ x. f x] *)

(* sujet
let tm2 () = failwith "NYI"
   /sujet *)

let tm2 () = Lam (fun f -> Lam (fun x -> App (Var f, Var x)))

(* Define [tm3] as [λ x. (λ y. y) x] *)
(* sujet
let tm3 () = failwith "NYI"
   /sujet *)

let tm3 () = Lam (fun x -> App (Lam (fun y -> Var y), Var x))

(* Define [vl1] as [λ x. x] *)

(* sujet
let vl1 () = failwith "NYI"
   /sujet *)

(* corrige *)
let vl1 () = VFun (fun x -> x)

(* /corrige *)

(* Define [vl2] as [λ f. λ x. f x] *)

(* sujet
let vl2 () = failwith "NYI"
   /sujet *)

(* corrige *)
let vl2 () =
  VFun
    (fun f ->
      let (VFun f) = f in
      VFun (fun x -> f x))


(* /corrige *)

(* Define [vl3] as [λ x. (λ y. y) x] *)
(* sujet
let vl3 () = failwith "NYI"
   /sujet *)

(* corrige *)
let vl3 () = VFun (fun x -> (fun y -> y) x)

(* /corrige *)

(* Define [nf1] as [λ x. x] *)

(* sujet
let nf1 () = failwith "NYI"
   /sujet *)

(* corrige *)
let nf1 () = NLam (fun x -> NAt (AVar x))

(* /corrige *)

(* Define [nf2] as [λ f. λ x. f x] *)

(* sujet
let nf2 () = failwith "NYI"
   /sujet *)

(* corrige *)
let nf2 () = NLam (fun f -> NLam (fun x -> NAt (AApp (AVar f, NAt (AVar x)))))

(* /corrige *)

(* Define [nf3] as [λ x. (λ y. y) x] *)
(* sujet
let nf3 () = failwith "NYI"
   /sujet *)

(* corrige *)
let nf3 () = NLam (fun x -> NAt (AVar x))

(* /corrige *)

(****************************************************************)
(* Evaluation function: from source to intermediate             *)
(****************************************************************)

(* sujet
let rec eval : type a. a tm -> a vl =
  fun _ -> failwith "NYI"
   /sujet *)

(* corrige *)
let rec eval : type a. a tm -> a vl = function
  | Var x ->
      x
  | Lam f ->
      VFun (fun x -> eval (f x))
  | App (m, n) ->
      let (VFun f) = eval m in
      f (eval n)


(* /corrige *)

(****************************************************************)
(* reify and reflect: from intermediate to target               *)
(****************************************************************)

(* sujet
 let rec reify : type a. a tp -> a vl -> a nf =
   fun a v -> failwith "NYI"
and reflect : type a. a tp -> a at -> a vl = fun a r ->
   fun a r -> failwith "NYI"
  /sujet *)

(* corrige *)
let rec reify : type a. a tp -> a vl -> a nf =
 fun a v ->
  match (a, v) with
  | Arr (a, b), VFun f ->
      NLam (fun x -> reify b (f (reflect a (AVar x))))
  | Base, VBase v ->
      let (Atom r) = v in
      NAt r


and reflect : type a. a tp -> a at -> a vl =
 fun a r ->
  match a with
  | Arr (a, b) ->
      VFun (fun x -> reflect b (AApp (r, reify a x)))
  | Base ->
      VBase (Atom r)


(* /corrige *)

(****************************************************************)
(* Normalization: from term to normal form                      *)
(****************************************************************)

(* sujet
let nbe : type a. a tp -> a tm -> a nf =
  fun a m -> failwith "NYI"
   /sujet *)

(* corrige *)
let nbe : type a. a tp -> a tm -> a nf = fun a m -> reify a (eval m)

(* /corrige *)

let%test_unit _ =
  let typ1 = typ1 () in
  let tm1 = tm1 () in
  ignore (nbe typ1 tm1)

let%test_unit _ =
  let typ2 = typ2 () in
  let tm2 = tm2 () in
  ignore (nbe typ2 tm2)

let%test_unit _ =
  let typ2 = typ2 () in
  let tm1 = tm1 () in
  ignore (nbe typ2 tm1)

let%test_unit _ =
  let typ1 = typ1 () in
  let tm3 = tm3 () in
  ignore (nbe typ1 tm3)

let%test_unit _ =
  let typ2 = typ2 () in
  let tm3 = tm3 () in
  ignore (nbe typ2 tm3)

(****************************************************************)
(* Outro                                                        *)
(****************************************************************)

(* Puzzle: did you notice that I didn't try to write
   pretty-printers or equality tests, what am I hiding up my
   sleeve? *)

(* Puzzle: [untyped.ml] unties the self-reference through polymorphic variables.
   Can you do the same here? *)

(* Hungry for more?
   -> "Typeful Normalization by Evaluation", Danvy, Keller & Puech
      [https://hal.inria.fr/hal-01397929/document] *)
