(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   encode.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/20 09:03:46 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/20 11:00:15 by clorin           ###   ########.fr       *)
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

let rec encode l = 
    let rec is_value_present value lst =
      match lst with
      | [] -> false
      | (_, x) :: queue -> x = value || is_value_present value queue
    in
    let rec compte l c = match l with
      | [] -> 0
      | head::queue when head = c -> 1 + compte queue c
      | _::queue -> 0
    in
    let rec process l result = match l with
      | [] ->  List.rev result
      | head::queue -> 
        if is_value_present head result then
          process queue result
        else
          process queue (((compte l head), head):: result)
    in
    process l []
    
let rec affiche_liste_tupple liste = match liste with
    | [] -> ()
    | head::queue ->
        affiche_tuples head;
        print_char '\n';
        affiche_liste_tupple queue

let main () = 
  let ma_liste = ['a';'a';'a';'b';'b';'c'] in
  print_string "liste = [";
  afficher_liste ma_liste;
  print_endline "]";
  affiche_liste_tupple(encode ma_liste)
  
let () = main ()