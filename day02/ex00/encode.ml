(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   encode.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/20 09:03:46 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/20 14:27:36 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec len_list l = match l with
  | [] -> 0
  | _::queue -> 1 + len_list queue

let affiche_tuples tuple = match tuple with
  | (x,y) -> print_char '(';print_int x;print_char ',';print_char y;print_char ')'

let rec afficher_liste = function
| [] -> ()
| head :: queue ->
  print_char head;
  print_char ';';
  afficher_liste queue

let encode l =
  let rec count_letter l count result = match l with
    | first_letter::second_letter::queue ->
      if (first_letter = second_letter) then
        count_letter (second_letter::queue) (count + 1) result
      else
        count_letter (second_letter::queue) 0 (result @ [(count + 1), first_letter])
    | letter::queue -> result @ [(count + 1), letter]
    | [] -> result
  in 
  count_letter l 0 []

let rec affiche_liste_tupple liste = match liste with
    | [] -> ()
    | head::queue ->
        affiche_tuples head;
        print_char '\n';
        affiche_liste_tupple queue

let main () = 
  let ma_liste = ['a';'a';'a';'b';'b';'c';'a'] in
  print_string "liste = [";
  afficher_liste ma_liste;
  print_endline "]";
  affiche_liste_tupple(encode ma_liste)
  
let () = main ()