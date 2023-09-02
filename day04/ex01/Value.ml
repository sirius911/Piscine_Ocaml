type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

let all = [Some T2; Some T3; Some T4; Some T5; Some T6; Some T7; Some T8; Some T9; Some T10; Some Jack; Some Queen; Some King; Some As]

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

let fromInt (i : int) : t option = match i with
  | 1 -> Some T2
  | 2 -> Some T3
  | 3 -> Some T4
  | 4 -> Some T5
  | 5 -> Some T6
  | 6 -> Some T7
  | 7 -> Some T8
  | 8 -> Some T9
  | 9 -> Some T10
  | 10 -> Some Jack
  | 11 -> Some Queen
  | 12 -> Some King
  | 13 -> Some As
  | _ -> None

let toString (card : t option) : string = match card with
  | Some T2 -> "2"
  | Some T3 -> "3"
  | Some T4 -> "4"
  | Some T5 -> "5"
  | Some T6 -> "6"
  | Some T7 -> "7"
  | Some T8 -> "8"
  | Some T9 -> "9"
  | Some T10 -> "10"
  | Some Jack -> "J"
  | Some Queen -> "Q"
  | Some King -> "K"
  | Some As -> "A"
  | None -> ""

let toStringVerbose (card : t option) : string = match card with
  | Some T2 -> "2"
  | Some T3 -> "3"
  | Some T4 -> "4"
  | Some T5 -> "5"
  | Some T6 -> "6"
  | Some T7 -> "7"
  | Some T8 -> "8"
  | Some T9 -> "9"
  | Some T10 -> "10"
  | Some Jack -> "Jack"
  | Some Queen -> "Queen"
  | Some King -> "King"
  | Some As -> "As"
  | None -> "None"

let next (card : t) : t option = 
  if card = As then
      invalid_arg "invalid_arg if argument is As";
  fromInt ((toInt card) + 1)

let previous (card : t) : t option = 
  if card = T2 then
      invalid_arg "invalid_arg if argument is T2";
  fromInt ((toInt card) - 1)