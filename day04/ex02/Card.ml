(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Card.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/03 12:50:30 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/04 09:11:37 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Color = 
  struct
    type t = Spade | Heart | Diamond | Club

    let all = [Spade; Heart; Diamond; Club]
    
    let toString (color : t) : string = 
      match color with
      | Spade -> "S"
      | Heart -> "H"
      | Diamond -> "D"
      | Club -> "C"
    
    let toStringVerbose (color : t) : string = 
      match color with
      | Spade -> "Spade"
      | Heart -> "Heart"
      | Diamond -> "Diamond"
      | Club -> "Club"  
  end

module Value =
  struct
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
  end

type t = (Value.t * Color.t )

(* Constructor*)
let newCard (value : Value.t) (color : Color.t) : t = 
  (value, color)

let getValue (card : t) : Value.t = match card with
  | (v,_) -> v

let getColor (card : t) : Color.t = match card with
  | (_,c) -> c

let toString (card : t) : string = match card with
  | (v, c) -> Value.toString v ^ Color.toString c

let toStringVerbose (card : t) : string = match card with
  | (v, c) -> "Card(" ^ Value.toStringVerbose v ^ ", " ^ Color.toStringVerbose c ^ ")"

let compare (card1 : t) (card2 : t) : int = 
  let comparaison = (Value.toInt(getValue card1)) - (Value.toInt(getValue card2)) in
  if comparaison < 0 then -1
  else if comparaison > 0 then 1
  else 0

let max (card1 : t) (card2 : t) : t =
  if compare card1 card2 >= 0 then card1
  else card2

let min (card1: t) (card2 : t) : t = 
  if compare card1 card2 <= 0 then card1
  else card2

let all = List.rev (List.fold_left (fun acc1 color ->
  List.fold_left (fun acc2 value ->
    (value, color) :: acc2
    ) acc1 Value.all) [] Color.all)

let allSpades = List.filter (fun (_,color) -> color = Color.Spade) all
let allHearts = List.filter (fun (_,color) -> color = Color.Heart) all
let allDiamonds = List.filter (fun (_,color) -> color = Color.Diamond) all
let allClubs = List.filter (fun (_,color) -> color = Color.Club) all 

let best (l : t list) : t = match l with
    | head::queue -> List.fold_left (fun a b -> max a b) head queue
    | _ -> invalid_arg "empty list"

let isOf (card : t) (color : Color.t) : bool = 
  getColor card = color

let isSpade (card : t) : bool =
  isOf card Color.Spade

let isHeart (card : t) : bool =
  isOf card Color.Heart

let isDiamond (card : t) : bool = 
  isOf card Color.Diamond

let isClub (card : t) : bool = 
  isOf card Color.Club