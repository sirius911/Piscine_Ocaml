(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex04.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/07 20:12:07 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/07 22:07:21 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type VAL = 
sig
  type t
  val add : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
end

module type EVALEXPR = 
sig
  type t
  type expr = Add of expr * expr | Mul of expr * expr | Div of expr * expr | Value of t
  val eval : expr -> t
end

module type MAKEEVALEXPR = 
  functor (EvalExpr : VAL) -> 
    EVALEXPR with type t = EvalExpr.t

module MakeEvalExpr : MAKEEVALEXPR = 
  functor (EvalExpr : VAL) ->
    struct
      type t = EvalExpr.t
      type expr = Add of expr * expr | Mul of expr * expr | Div of expr * expr | Value of t
      let rec eval = function
        |Add(x,y) -> EvalExpr.add (eval x) (eval y)
        |Mul(x,y) -> EvalExpr.mul (eval x) (eval y)
        |Div(x,y) -> EvalExpr.div (eval x) (eval y)
        |Value (x) -> x
  end
  
(* subject*)
module IntVal : (VAL with type t = int) =
  struct
    type t = int
    let add = ( + )
    let mul = ( * )
    let div = ( / )
  end

module FloatVal : (VAL with type t = float) =
  struct
    type t = float
    let add = ( +. )
    let mul = ( *. )
    let div = ( /. )
  end

module StringVal : (VAL with type t = string) =
  struct
    type t = string
    let add s1 s2 = if (String.length s1) > (String.length s2) then s1 else s2
    let mul = ( ^ )
    let div = ( ^ )
  end

module IntEvalExpr : (EVALEXPR with type t := IntVal.t) = MakeEvalExpr (IntVal)
module FloatEvalExpr : (EVALEXPR with type t := FloatVal.t) = MakeEvalExpr (FloatVal)
module StringEvalExpr : (EVALEXPR with type t := StringVal.t) = MakeEvalExpr (StringVal)

let ie = IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2)
let fe = FloatEvalExpr.Add (FloatEvalExpr.Value 41.5, FloatEvalExpr.Value 0.92)
let se = StringEvalExpr.Mul (StringEvalExpr.Value "very ",
(StringEvalExpr.Add (StringEvalExpr.Value "very long",
StringEvalExpr.Value "short")))
let divers = FloatEvalExpr.Div(FloatEvalExpr.Mul(FloatEvalExpr.Value 6., FloatEvalExpr.Value 7.), FloatEvalExpr.Mul(FloatEvalExpr.Value 3., FloatEvalExpr.Value 5.))
(* let ia = FloatEvalExpr.Div(FloatEvalExpr.Value 1., FloatEvalExpr.Value 3.) *)
let () = Printf.printf "Res = %d\n" (IntEvalExpr.eval ie)
let () = Printf.printf "Res = %f\n" (FloatEvalExpr.eval fe)
let () = Printf.printf "Res = %s\n" (StringEvalExpr.eval se)
let () = Printf.printf "Res = %f\n" (FloatEvalExpr.eval divers)
