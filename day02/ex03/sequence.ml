(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sequence.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/21 21:52:06 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/22 09:51:25 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* let first lst = match lst with
  | [] -> None
  | head::queue -> Some head

let count l =
  let rec count_letter l count = match l with
    | first_letter::second_letter::queue ->
      if (first_letter = second_letter) then
        count_letter (second_letter::queue) (count + 1)
      else
        count
    | letter::queue -> count
    | [] -> count
  in 
  count_letter l 1 *)

(* let encode lst =
  let rec count_consecutive acc count = function
    | x :: (y :: _ as rest) ->
      if x = y then count_consecutive acc (count + 1) rest
      else count_consecutive (acc @ [count + 1; x]) 0 rest
    | [x] -> acc @ [count + 1; x]
    | [] -> acc
  in
  count_consecutive [] 0 lst *)

(* let list_to_str lst = 
  let rec loop lst result = match lst with
    | [] -> result
    | head::queue -> result ^ string_of_int (head) ^ loop queue result
  in
  loop lst "" *)

let sequence n =
  let rec encode acc count = function
    | x :: (y :: _ as rest) ->
      if x = y then encode acc (count + 1) rest
      else encode (acc @ [count + 1; x]) 0 rest
    | [x] -> acc @ [count + 1; x]
    | [] -> acc
  in
  let rec list_to_str lst result = match lst with
    | [] -> result
    | head::queue -> result ^ string_of_int (head) ^ list_to_str queue result
  in
  let rec loop i lst = 
    if i = n then 
      list_to_str lst ""
    else
      loop (i + 1) (encode [] 0 lst)
  in
  if n <= 0 then 
    ""
  else
    loop 1 [1]
  
let main () =
  let rec loop n = match n with
    | 0 -> ()
    | _ -> loop (n - 1); print_endline (sequence n)
  in
  loop 8;
  print_endline ("\"" ^ (sequence 0) ^"\"");
  print_endline ("\"" ^ (sequence (-10)) ^"\"")
let () = main ()