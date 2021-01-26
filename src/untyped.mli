(****************************************************************)
(* Simple types                                                 *)
(****************************************************************)

type tp

val typ1 : unit -> tp

val typ2 : unit -> tp

(****************************************************************)
(* Source language: λ-terms                                     *)
(****************************************************************)

type 'a tm

val pp_tm :
     (unit -> 'a)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a tm
  -> unit

val tm1 : unit -> 'a tm

val tm2 : unit -> 'a tm

val tm3 : unit -> 'a tm

(****************************************************************)
(* Intermediate language of values                              *)
(****************************************************************)

type 'b vl

val vl1 : unit -> 'b vl

val vl2 : unit -> 'b vl

val vl3 : unit -> 'b vl

(****************************************************************)
(* Target language: β-normal λ-terms                            *)
(****************************************************************)

type 'a nf

type 'a at

val equal_nf : (unit -> 'a) -> ('a -> 'a -> bool) -> 'a nf -> 'a nf -> bool

val equal_at : (unit -> 'a) -> ('a -> 'a -> bool) -> 'a at -> 'a at -> bool

val pp_nf :
     (unit -> 'a)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a nf
  -> unit

val pp_at :
     (unit -> 'a)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a at
  -> unit

val nf1 : unit -> 'a nf

val nf2 : unit -> 'a nf

val nf3 : unit -> 'a nf

(****************************************************************)
(* Evaluation function: from source to intermediate             *)
(****************************************************************)

val eval : 'a vl tm -> 'a vl

(****************************************************************)
(* reify and reflect: from intermediate to target               *)
(****************************************************************)

val reify : tp -> 'a at vl -> 'a nf

val reflect : tp -> 'a at -> 'a at vl

(****************************************************************)
(* Normalization: from term to normal form                      *)
(****************************************************************)

val nbe : tp -> 'a at vl tm -> 'a nf

(****************************************************************)
(* Public API                                                   *)
(****************************************************************)

type vars

type x = vars at vl

type term = x tm

type normal = vars nf

val norm : tp -> term -> normal
