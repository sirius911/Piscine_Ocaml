(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Value.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/03 12:49:39 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/03 12:49:49 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

let all = [ T2;  T3;  T4;  T5;  T6;  T7;  T8;  T9;  T10;  Jack;  Queen;  King;  As]

let toInt (card : t) : int = match card with
  | T2 -> 1
  | T3 -> 2
  | T4 -> 3
  | T5 -> 4
  | T6 -> 5
  | T7 -> 6
  | T8 -> 7
  | T9 -> 8
  | T10 -> 9
  | Jack -> 10
  | Queen -> 11
  | King -> 12
  | As -> 13

let fromInt (i : int) : t  = match i with
  | 1 ->  T2
  | 2 ->  T3
  | 3 ->  T4
  | 4 ->  T5
  | 5 ->  T6
  | 6 ->  T7
  | 7 ->  T8
  | 8 ->  T9
  | 9 ->  T10
  | 10 ->  Jack
  | 11 ->  Queen
  | 12 ->  King
  | 13 ->  As
  | _ -> invalid_arg "invalid_arg in fromInt"

let toString (card : t ) : string = match card with
  | T2 -> "2"
  | T3 -> "3"
  | T4 -> "4"
  | T5 -> "5"
  | T6 -> "6"
  | T7 -> "7"
  | T8 -> "8"
  | T9 -> "9"
  | T10 -> "10"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | As -> "A"

let toStringVerbose (card : t ) : string = match card with
  | T2 -> "2"
  | T3 -> "3"
  | T4 -> "4"
  | T5 -> "5"
  | T6 -> "6"
  | T7 -> "7"
  | T8 -> "8"
  | T9 -> "9"
  | T10 -> "10"
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"
  | As -> "As"

let next (card : t) : t  = 
  if card = As then
      invalid_arg "next card after As does'nt exist"
  else
    fromInt ((toInt card) + 1)

let previous (card : t) : t  = 
  if card = T2 then
      invalid_arg "previus card before T2 does'nt exist"
  else
    fromInt ((toInt card) - 1)