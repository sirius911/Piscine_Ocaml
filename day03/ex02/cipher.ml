(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   cipher.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/28 12:23:23 by clorin            #+#    #+#             *)
(*   Updated: 2023/08/30 21:26:55 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let caesar (str : string) (n : int) : string = 
  let sens = 
    if n >= 0 then 1
    else
      (-1)
  in

  let is_alpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') in

  let rotate c i = 
    let tmp = char_of_int(int_of_char(c) + i) in
    match tmp with
    | '[' -> 'A'
    | '{' -> 'a'
    | '@' -> 'Z'
    | '`' -> 'z'
    | _ -> tmp
  in 

  let rotation_horaire c =      
      if is_alpha c then
          rotate c 1
      else
          c
  in
  
  let rotation_anti_horaire c =
    if is_alpha c then
        rotate c (-1)
    else
        c
    in

  let rec loop str i index =
      if i = 0 then
          str
      else
        begin 
          if index > 0 then
            String.map rotation_horaire (loop str (i - 1) index)
          else
            String.map rotation_anti_horaire (loop str (i - 1) index)
        end
  in
  if n >= 0 then
    loop str n sens
  else
    loop str (-n) sens

let rot42 str = 
  caesar str 42

let xor (str:string) (key:int) : string = 
  String.map (fun c-> char_of_int ((int_of_char c) lxor key )) str

let rec ft_crypt (str:string) (f: (string -> string) list) : string =
  match f with
  | head::queue -> ft_crypt (head str) queue
  | _ -> str