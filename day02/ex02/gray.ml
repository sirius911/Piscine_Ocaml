(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gray.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/21 21:02:44 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/21 21:45:40 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* Fonction pour inverser une liste *)
let rec reverse_list lst =
  let rec loop lst acc =
    match lst with
    | [] -> acc
    | head :: queue -> loop queue (head :: acc)
  in
  loop lst []

(* Fonction pour ajouter un préfixe à chaque élément d'une liste *)
let rec add_prefix_to_list prefix lst = match lst with
  | [] -> []
  | head :: queue -> (prefix ^ head) :: (add_prefix_to_list prefix queue)

(* Fonction pour ajouter un élément à la fin d'une liste *)
let rec append_element lst elem = match lst with
  | [] -> [elem]
  | head :: queue -> head :: (append_element queue elem)

let rec concatenate_lists lst1 lst2 = match lst1 with
  | [] -> lst2
  | head :: queue -> head :: concatenate_lists queue lst2

(* Fonction pour générer la séquence de Gray de taille n *)
let rec generate_gray n =
  if n = 1 then
    ["0"; "1"]
  else
    let prev_gray_sequence = generate_gray (n - 1) in
    let first_half = add_prefix_to_list "0" prev_gray_sequence in
    let second_half = add_prefix_to_list "1" (reverse_list prev_gray_sequence) in
    concatenate_lists first_half second_half

let gray n =
  let gray_sequence = generate_gray n in
  let rec loop_print = function
    | [] -> ()
    | head :: queue -> print_string head; print_char ' ';loop_print queue
  in
  loop_print gray_sequence;
  print_endline("")

let main () =
  gray 1;
  gray 2;
  gray 3;
  gray 4

let () = main ()