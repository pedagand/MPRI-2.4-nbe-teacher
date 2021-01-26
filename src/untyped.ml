(* sujet
(* Once you are done writing the code, remove this directive,
   whose purpose is to disable several warnings. *)
[@@@warning "-27"]
  /sujet *)

(****************************************************************)
(* Simple types                                                 *)
(****************************************************************)

(* *Deep* embedding of types:

<<<
    τ, σ ::= Ω
           | τ → σ
>>>
 *)

type tp =
  | Base
  | Arr of tp * tp
  [@@deriving show]

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

let _ =
  Format.printf "tp1 = %a\n" pp_tp (typ1 ()) ;
  Format.printf "tp2 = %a\n" pp_tp (typ2 ())

(****************************************************************)
(* Source language: λ-terms                                     *)
(****************************************************************)

(* *Deep* embedding of terms using weak/parametric higher-order
   abstract syntax (HOAS):

<<<
    t, u ::= x
           | λ x. t
           | t u
>>>
 *)

(* sujet
   type 'a tm = (* NYI *)
   (* Hint: ['a] ranges over the set of variables,
            piggy-back on OCaml for managing binders! *)
   |

   let pp_tm gensym pp_var = failwith "NYI"
 *)

(* corrige *)
type 'a tm =
  | Var of 'a
  | Lam of ('a -> 'a tm)
  | App of 'a tm * 'a tm

let pp_tm gensym pp_var =
  let rec aux oc = function
    | Var x ->
        pp_var oc x
    | Lam f ->
        let x = gensym () in
        Format.fprintf oc "(lambda (%a) %a)" pp_var x aux (f x)
    | App (t, u) ->
        Format.fprintf oc "(%a %a)" aux t aux u
  in
  aux


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

let _ =
  let gensym =
    let x = ref 0 in
    fun () ->
      incr x ;
      "x" ^ string_of_int !x
  in
  let pp_var oc s = Format.fprintf oc "%s" s in
  let pp_tm_str = pp_tm gensym pp_var in
  Format.printf "tm1 = %a\n" pp_tm_str (tm1 ()) ;
  Format.printf "tm2 = %a\n" pp_tm_str (tm2 ()) ;
  Format.printf "tm3 = %a\n" pp_tm_str (tm3 ())


(****************************************************************)
(* Intermediate language of values                              *)
(****************************************************************)

(* *Shallow* embedding of values in weak-head normal form:
   [https://en.wikipedia.org/wiki/Lambda_calculus_definition#Weak_head_normal_form]

<<<
    v ::= λ x. v
        | base

>>>
 *)

(* We are polymorphic in the representation ['b] of base values for now *)

type 'b vl =
  | VFun of ('b vl -> 'b vl)
  | VBase of 'b

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
let vl2 () = VFun (function VFun f -> VFun (fun x -> f x) | _ -> assert false)

(* /corrige *)

(* Define [vl3] as [λ x. (λ y. y) x] *)
(* sujet
let vl3 () = failwith "NYI"
   /sujet *)

(* corrige *)
let vl3 () = VFun (fun x -> (fun y -> y) x)

(* /corrige *)

(****************************************************************)
(* Target language: β-normal λ-terms                            *)
(****************************************************************)

(* *Deep* embedding of β-normal terms using weak/parametric
   higher-order abstract syntax (HOAS):
   [https://en.wikipedia.org/wiki/Beta_normal_form]

<<<
    nf ::= at
         | λ x. nf

    at ::= at nf
         | x
>>>
*)

(* Hint: ['a] ranges over the set of variables,
   piggy-back on OCaml for managing binders! *)

(* sujet
type 'a nf = (* NYI *)
  |
and 'a at = (* NYI *)
  |
  /sujet *)

(* corrige *)
type 'a nf =
  | NLam of ('a -> 'a nf)
  | NAt of 'a at

and 'a at =
  | AApp of 'a at * 'a nf
  | AVar of 'a

let rec pp_nf gensym pp_var oc = function
  | NLam f ->
      let x = gensym () in
      Format.fprintf oc "(lambda (%a) %a)" pp_var x (pp_nf gensym pp_var) (f x)
  | NAt at ->
      pp_at gensym pp_var oc at


and pp_at gensym pp_var oc = function
  | AApp (at, n) ->
      Format.fprintf
        oc
        "(%a %a)"
        (pp_at gensym pp_var)
        at
        (pp_nf gensym pp_var)
        n
  | AVar v ->
      pp_var oc v


let rec equal_nf gensym equal_var nf1 nf2 =
  match (nf1, nf2) with
  | NLam f1, NLam f2 ->
      let x = gensym () in
      equal_nf gensym equal_var (f1 x) (f2 x)
  | NAt at1, NAt at2 ->
      equal_at gensym equal_var at1 at2
  | _, _ ->
      false


and equal_at gensym equal_var at1 at2 =
  match (at1, at2) with
  | AApp (at1, n1), AApp (at2, n2) ->
      equal_at gensym equal_var at1 at2 && equal_nf gensym equal_var n1 n2
  | AVar v1, AVar v2 ->
      equal_var v1 v2
  | _, _ ->
      false


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

let _ =
  let gensym =
    let x = ref 0 in
    fun () ->
      incr x ;
      "x" ^ string_of_int !x
  in
  let pp_var oc s = Format.fprintf oc "%s" s in
  let pp_nf_str = pp_nf gensym pp_var in
  Format.printf "nf1 = %a\n" pp_nf_str (nf1 ()) ;
  Format.printf "nf2 = %a\n" pp_nf_str (nf2 ()) ;
  Format.printf "nf3 = %a\n" pp_nf_str (nf3 ())


let%test _ =
  let gensym =
    let x = ref 0 in
    fun () ->
      incr x ;
      !x
  in
  let nf1 = nf1 () in
  let nf2 = nf2 () in
  not (equal_nf gensym ( = ) nf1 nf2)

(****************************************************************)
(* Evaluation function: from source to intermediate             *)
(****************************************************************)

(* sujet
let rec eval : type a. (a vl) tm -> a vl =
  fun _ -> failwith "NYI"
 /sujet *)

(* corrige *)
let rec eval : type a. a vl tm -> a vl = function
  | Var x ->
      x
  | Lam f ->
      VFun (fun x -> eval (f x))
  | App (m, n) ->
    ( match eval m with
    | VFun f ->
        f (eval n)
    | VBase _ ->
        failwith "Unidentified Functional Object" )


(* /corrige *)

(****************************************************************)
(* reify and reflect: from intermediate to target               *)
(****************************************************************)

(* sujet
let rec reify : type a. tp -> (a at) vl -> a nf =
 fun a v -> failwith "NYI"
and reflect : type a. tp -> a at -> (a at) vl =
 fun a r -> failwith "NYI"
   /sujet *)

(* corrige *)
let rec reify : type a. tp -> a at vl -> a nf =
 fun a v ->
  match (a, v) with
  | Arr (a, b), VFun f ->
      NLam (fun x -> reify b (f (reflect a (AVar x))))
  | Base, VBase v ->
      NAt v
  | _ ->
      failwith "type mismatch"


and reflect : type a. tp -> a at -> a at vl =
 fun a r ->
  match a with
  | Arr (a, b) ->
      VFun (fun x -> reflect b (AApp (r, reify a x)))
  | Base ->
      VBase r


(* /corrige *)

let%test _ =
  let gensym =
    let x = ref 0 in
    fun () ->
      incr x ;
      !x
  in
  let n1 =
    reify
      (Arr (Base, Base))
      (VFun (fun x -> x))
  in
  let n2 = nf1 () in
  equal_nf gensym ( = ) n1 n2

let%test _ =
  let gensym =
    let x = ref 0 in
    fun () ->
      incr x ;
      !x
  in
  let n1 =
    reify
      (Arr (Arr (Base, Base), Arr (Base, Base)))
      (VFun (fun x -> x))
  in
  let n2 = nf2 () in
  equal_nf gensym ( = ) n1 n2

(****************************************************************)
(* Normalization: from term to normal form                      *)
(****************************************************************)

(* sujet
let nbe : type a. tp -> a at vl tm  -> a nf =
  fun a m -> failwith "NYI"
  /sujet *)

(* corrige *)
let nbe : type a. tp -> a at vl tm -> a nf = fun a m -> reify a (eval m)

(* /corrige *)

let gensym =
  let x = ref 0 in
  fun () ->
    incr x ;
    !x


let%test _ =
  let typ1 = typ1 () in
  let tm1 = tm1 () in
  let n1 = nbe typ1 tm1 in
  let n2 = nf1 () in
  equal_nf gensym ( = ) n1 n2

let%test _ =
  let typ2 = typ2 () in
  let tm2 = tm2 () in
  let n1 = nbe typ2 tm2 in
  let n2 = nf2 () in
  equal_nf gensym ( = ) n1 n2

let%test _ =
  let typ2 = typ2 () in
  let tm1 = tm1 () in
  let n1 = nbe typ2 tm1 in
  let n2 = nf2 () in
  equal_nf gensym ( = ) n1 n2

let%test _ =
  let typ1 = typ1 () in
  let tm3 = tm3 () in
  let n1 = nbe typ1 tm3 in
  let n2 = nf1 () in
  equal_nf gensym ( = ) n1 n2

let%test _ =
  let typ2 = typ2 () in
  let tm3 = tm3 () in
  let n1 = nbe typ2 tm3 in
  let n2 = nf2 () in
  equal_nf gensym ( = ) n1 n2

(****************************************************************)
(* Public API                                                   *)
(****************************************************************)

type vars

type x = vars at vl

type term = x tm

type normal = vars nf

let norm : tp -> term -> normal = nbe
