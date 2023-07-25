(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   crossover.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/21 18:50:52 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/21 20:35:25 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let afficher_liste lst = 
  print_char '[';
  let rec loop =   function
    | [] -> print_endline "]"
    | [x] -> print_char x; loop []
    | head :: queue -> print_char head;
      print_char ';';
      loop queue
  in
  loop lst

let crossover lst1 lst2 = 
  let rec is_present lst el = match lst with
    | [] -> false
    | head::queue  -> (head = el) || is_present queue el
  in
  if lst1 = [] || lst2 = [] then []
  else
    let rec loop lst1 lst2 result = match lst1 with
      | [] -> result
      | head::queue ->
          if (is_present lst2 head)  && not (is_present result head) then
            loop queue lst2 (head::result)
          else
            loop queue lst2 result
    in
    loop lst1 lst2 [] 

let main () =
  let lst1 = ['a';'a';'a';'b';'b';'c';'a'] in
  let lst2 = ['d';'a';'c'] in
  afficher_liste(crossover lst2 lst1);
  afficher_liste(crossover ['1';'2';'3'] ['3';'4';'5']);
  afficher_liste(crossover lst1 [])
let () = main ()