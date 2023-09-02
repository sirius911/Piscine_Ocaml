(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   btree.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/08/30 21:48:41 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/02 11:20:51 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree
  
let rec height (t: 'a tree) : int = match t with
  | Nil -> 0
  | Node (_, Nil, Nil) -> 0
  | Node (_, left, right) -> 1 + max (height left) (height right)

let is_bst (t : 'a tree) : bool = 
   (*The trick I use is to transform the tree into a list through an infix path, and then check if the list is sorted.*)
  let rec is_sorted (l:'a list) : bool = 
    match l with
    | [] | [_] -> true
    | x::y::rest -> if x > y then false else is_sorted (y::rest)
  in
  let rec get_list_of_tree (t : ('a tree)) : 'a list = 
    match t with
    | Nil -> []
    | Node(a, left, Nil) -> get_list_of_tree left @ [a]
    | Node(a, Nil, right) -> [a] @ get_list_of_tree right
    | Node(a, left, right) ->
        (get_list_of_tree (left)) @ [a] @ (get_list_of_tree (right))
  in
  is_sorted (get_list_of_tree t)
   
let is_perfect (t : 'a tree) : bool =
  let h = height t in
  let rec loop (t: 'a tree) (depth : int) (current_depth : int) : bool =
    match t with
    | Nil -> current_depth = depth
    | Node (_, left, right) ->
      loop left depth (current_depth + 1) &&
      loop right depth (current_depth + 1)
  in
  loop t h 0

let rec is_balanced (t : 'a tree) : bool = match t with
  | Nil -> true
  | Node(a, Nil, Nil) -> true
  | Node(a, Nil, b) | Node(a, b, Nil) -> 
      if ((height b) > 1) then false else true
  | Node(a, left, right) -> 
      if ((abs ((height left) - (height right))) > 1) then false
      else is_balanced left && is_balanced right

let search_bst (t : 'a tree) (value : 'a) : bool =
  let rec search (t : 'a tree) (value : 'a) : bool = 
    match t with
    | Node(a, left, right) -> if a = value then true
      else search left value || search right value
    | Nil -> false
    in
  if is_bst t then
    search t value
  else
    begin
      print_endline("the tree must be a BST !");
      false
    end

let add_bst (t : 'a tree) (value : 'a) : ('a tree) =
  let rec add (t : 'a tree) (value : 'a) : ('a tree) = 
    match t with
    | Nil -> Node(value, Nil, Nil)
    | Node(a, left, right) ->
      if value < a then Node(a, add left value, right)
      else if value > a then Node(a, left, add right value)
      else t
  in
  if is_bst t then
    add t value
  else  
    begin
      print_endline("the tree must be a BST !");
      t
    end

let delete_bst (t : 'a tree) (value : 'a) : ('a tree) = 
  if is_bst t then
    t
  else  
    begin
      print_endline("the tree must be a BST !");
      t
    end

let main () = 
  let t = Node (10, Node (7, Nil, Node(13, Nil, Nil)), Node (11, Nil, Node (12, Node (50, Nil, Nil), Nil))) in (* false *)
  
  let rec print_bst t =
    match t with
      Node (a,b,c) -> Printf.printf "(%d " a; print_bst b; print_bst c; Printf.printf ")"
    | Nil -> Printf.printf "Nil ";
  in

  print_bst t;
  Printf.printf " -> %b => " (is_bst t);
  Printf.printf "%b **> " (is_perfect t);
  Printf.printf "%b\n" (is_balanced t);
  let value_search = 2 in 
  Printf.printf "%d in tree = %b\n" value_search (search_bst t value_search);
  let value_search = 50 in 
  Printf.printf "%d in tree = %b\n" value_search (search_bst t value_search);

  let t = Node (10, Node (7, Nil, Node (9, Nil, Nil)), Node (11, Nil, Node (12, Nil, Nil))) in 
  print_bst t;
  Printf.printf " -> %b => " (is_bst t);
  Printf.printf "%b **> " (is_perfect t);
  Printf.printf "%b\n" (is_balanced t);

  let t = Node (10, Node (1, Nil, Nil), Node (20, Nil, Nil)) in 
  print_bst t;
  Printf.printf " -> %b => " (is_bst t);
  Printf.printf "%b **> " (is_perfect t);
  Printf.printf "%b\n" (is_balanced t);

  let t = Node (10, Node (5, Node(1, Node(0, Nil, Nil), Node(3, Nil, Nil)), Node(7, Nil, Nil)), Node (20, Node(15, Nil, Nil), Node(25, Nil, Nil))) in
  print_bst t;
  Printf.printf " -> %b => " (is_bst t);
  Printf.printf "%b **> " (is_perfect t);
  Printf.printf "%b\n" (is_balanced t);

  let t = Node (10, Node (5, Node(1, Node(0, Nil, Nil), Node(3, Nil, Nil)), Nil), Node (20, Node(15, Nil, Nil), Node(25, Nil, Nil))) in 
  print_bst t;
  Printf.printf " -> %b => " (is_bst t);
  Printf.printf "%b **> " (is_perfect t);
  Printf.printf "%b\n" (is_balanced t);

  let t = Node (4,Node(3, Node(2, Node(1, Nil, Nil), Nil), Nil), Nil) in
  print_bst t;
  Printf.printf " -> %b => " (is_bst t);
  Printf.printf "%b **> " (is_perfect t);
  Printf.printf "%b\n" (is_balanced t);
  let value_search = 2 in 
  Printf.printf "%d in tree = %b\n" value_search (search_bst t value_search);
  let value_search = 42 in 
  Printf.printf "%d in tree = %b\n" value_search (search_bst t value_search);
  let t = add_bst t 5 in
  print_endline "-------";
  print_bst t;
  print_endline "-------";
  
  let balanced_tree =
    Node(4,
      Node(2,
        Node(1, Nil, Nil),
        Node(3, Nil, Nil)
      ),
      Node(6,
        Node(5, Nil, Nil),
        Node(7, Nil, Nil)
      )
    ) in
  print_bst balanced_tree;
  Printf.printf " -> %b => " (is_bst balanced_tree);
  Printf.printf "%b **> " (is_perfect balanced_tree);
  Printf.printf "%b\n" (is_balanced balanced_tree);
  let value_search = 4 in 
  Printf.printf "%d in tree = %b\n" value_search (search_bst balanced_tree value_search);
  let value_search = 42 in 
  Printf.printf "%d in tree = %b\n" value_search (search_bst balanced_tree value_search);
  let t = add_bst balanced_tree 0 in
  print_bst t
  
let () = main ()