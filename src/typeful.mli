(****************************************************************)
(* Simple types                                                 *)
(****************************************************************)

type 'a tp

type base

val typ1 : unit -> (base -> base) tp

val typ2 : unit -> ((base -> base) -> base -> base) tp

(****************************************************************)
(* Source language: λ-terms                                     *)
(****************************************************************)

type 'a tm

val tm1 : unit -> ('a -> 'a) tm

val tm2 : unit -> (('a -> 'b) -> 'a -> 'b) tm

val tm3 : unit -> ('a -> 'a) tm

(****************************************************************)
(* Intermediate language of values                              *)
(****************************************************************)

type 'b vl

val vl1 : unit -> ('a -> 'a) vl

val vl2 : unit -> (('a -> 'b) -> 'a -> 'b) vl

val vl3 : unit -> ('a -> 'a) vl

(****************************************************************)
(* Target language: β-normal λ-terms                            *)
(****************************************************************)

type 'a nf

type 'a at

val nf1 : unit -> (base -> base) nf

val nf2 : unit -> ((base -> base) -> base -> base) nf

val nf3 : unit -> (base -> base) nf

(****************************************************************)
(* Evaluation function: from source to intermediate             *)
(****************************************************************)

val eval : 'a tm -> 'a vl

(****************************************************************)
(* reify and reflect: from intermediate to target               *)
(****************************************************************)

val reify : 'a tp -> 'a vl -> 'a nf

val reflect : 'a tp -> 'a at -> 'a vl

(****************************************************************)
(* Normalization: from term to normal form                      *)
(****************************************************************)

val nbe : 'a tp -> 'a tm -> 'a nf
