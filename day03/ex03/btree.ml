(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   btree.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/08/30 21:48:41 by clorin            #+#    #+#             *)
(*   Updated: 2023/08/30 22:50:21 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let is_bst (t : 'a tree) : bool = 
  let rec loop (t : 'a tree) (greater_value : 'a option) (smaller_value : 'a option) =
    match t with
    | Node(value,left,right) ->
      if greater_value != None && Some value > greater_value then false
      else if smaller_value != None && Some value < smaller_value then false
      else loop left (Some value) smaller_value && loop right greater_value (Some value) 
    | Nil -> true
  in 
  loop t None None

let is_perfect (t : 'a tree) : bool =
  let rec loop t d lvl =
    match t with
    | Node (a,left,right) -> if left = Nil && right = Nil then (d = (lvl + 1))
                      else if left = Nil || right = Nil then false
                      else (loop left d (lvl + 1) && loop right d (lvl + 1))
    | Nil -> true
  in
  let rec getdepth t d =
    match t with
      Node (a,b,_) -> getdepth b (d + 1)
    | Nil -> d
  in
  loop t (getdepth t 0) 0

let main () = 
  let t = Node (10, Node (7, Nil, Nil), Node (11, Nil, Node (12, Node (50, Nil, Nil), Nil))) in (* false *)
  
  let rec print_bst t =
    match t with
      Node (a,b,c) -> Printf.printf "(%d " a; print_bst b; print_bst c; Printf.printf ")"
    | Nil -> Printf.printf "Nil ";
  in

  print_bst t;
  Printf.printf "\n%b\n" (is_bst t);

  let t = Node (10, Node (7, Nil, Node (9, Nil, Nil)), Node (11, Nil, Node (12, Nil, Nil))) in (* true *)
  print_bst t;
  Printf.printf "\n%b\n" (is_bst t);
  Printf.printf "%b\n" (is_perfect t);

  let t = Node (10, Node (1, Nil, Nil), Node (20, Nil, Nil)) in (* perfect *)
  print_bst t;
  Printf.printf "\n%b\n" (is_bst t);
  Printf.printf "%b\n" (is_perfect t)

let () = main ()