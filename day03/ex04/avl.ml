(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   avl.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/08/30 21:48:41 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/02 11:20:51 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

(* Helper function to get the height of a tree *)
let rec height = function
  | Nil -> 0
  | Node (_, left, right) -> 1 + max (height left) (height right)

(* Perform a right rotation *)
let rotate_right = function
  | Node (x, Node (y, a, b), c) -> Node (y, a, Node (x, b, c))
  | _ -> failwith "Invalid rotation"

(* Perform a left rotation *)
let rotate_left = function
  | Node (x, a, Node (y, b, c)) -> Node (y, Node (x, a, b), c)
  | _ -> failwith "Invalid rotation"

(* Perform a double left-right rotation *)
let double_rotate_left_right = function
  | Node (x, a, Node (y, b, c)) -> rotate_left (Node (x, a, rotate_right (Node (y, b, c))))
  | _ -> failwith "Invalid rotation"

(* Perform a double right-left rotation *)
let double_rotate_right_left = function
  | Node (x, Node (y, a, b), c) -> rotate_right (Node (x, rotate_left (Node (y, a, b)), c))
  | _ -> failwith "Invalid rotation"

(* Function to insert a value into an AVL tree *)
let rec insert_avl value = function
  | Nil -> Node (value, Nil, Nil)
  | Node (x, left, right) ->
    if value < x then
      let new_left = insert_avl value left in
      if height new_left - height right > 1 then
        if value < match left with Nil -> 0 | Node (y, _, _) -> y then
          rotate_right (Node (x, new_left, right))
        else
          double_rotate_left_right (Node (x, new_left, right))
      else
        Node (x, new_left, right)
    else if value > x then
      let new_right = insert_avl value right in
      if height new_right - height left > 1 then
        if value > match right with Nil -> 0 | Node (y, _, _) -> y then
          rotate_left (Node (x, left, new_right))
        else
          double_rotate_right_left (Node (x, left, new_right))
      else
        Node (x, left, new_right)
    else
      (* Value already exists, no duplicate values in AVL tree *)
      Node (x, left, right)

(* Function to delete a value from an AVL tree *)
let rec delete_avl value = function
  | Nil -> Nil
  | Node (x, left, right) ->
    if value < x then
      let new_left = delete_avl value left in
      (* Check and balance the tree *)
      if height new_left - height right > 1 then
        if value < match left with Nil -> 0 | Node (y, _, _) -> y then
          rotate_right (Node (x, new_left, right))
        else
          double_rotate_left_right (Node (x, new_left, right))
      else
        Node (x, new_left, right)
    else if value > x then
      let new_right = delete_avl value right in
      (* Check and balance the tree *)
      if height new_right - height left > 1 then
        if value > match right with Nil -> 0 | Node (y, _, _) -> y then
          rotate_left (Node (x, left, new_right))
        else
          double_rotate_right_left (Node (x, left, new_right))
      else
        Node (x, left, new_right)
    else
      (* Node with the value to be deleted found *)
      if left = Nil then right
      else if right = Nil then left
      else
        (* Node with two children, replace with the minimum value from the right subtree *)
        let rec find_min = function
          | Nil -> failwith "Invalid tree"
          | Node (v, Nil, _) -> v
          | Node (_, left, _) -> find_min left
        in
        let min_value = find_min right in
        let new_right = delete_avl min_value right in
        Node (min_value, left, new_right)

        
        
let rec inorder_traversal = function
  | Nil -> []
  | Node (value, left, right) ->
    (inorder_traversal left) @ [value] @ (inorder_traversal right)

let rec is_avl = function
  | Nil -> true
  | Node (_, left, right) ->
    let balance_factor = abs (height left - height right) in
    balance_factor <= 1 && is_avl left && is_avl right

(* Test the AVL tree functions *)
let () =
  let print_bst t =
    let rec print (t : 'a tree) =
      match t with
        Node (a,left,right) -> Printf.printf "(%d " a; print left; print right; Printf.printf ")"
      | Nil -> Printf.printf "Nil ";
    in
  print t;
  print_endline "\n"
  in

  let avl_tree = ref Nil in
  avl_tree := insert_avl 10 !avl_tree;
  avl_tree := insert_avl 20 !avl_tree;
  avl_tree := insert_avl 30 !avl_tree;
  avl_tree := insert_avl 40 !avl_tree;
  avl_tree := insert_avl 50 !avl_tree;
  print_bst !avl_tree;

  Printf.printf "Inorder traversal after insertions: %s\n" (String.concat ", " (List.map string_of_int (inorder_traversal !avl_tree)));
  Printf.printf "Is AVL tree after insertions: %b\n" (is_avl !avl_tree);

  avl_tree := delete_avl 30 !avl_tree;
  Printf.printf "Inorder traversal after deletion of 30: %s\n" (String.concat ", " (List.map string_of_int (inorder_traversal !avl_tree)));
  Printf.printf "Is AVL tree after deletion of 30: %b\n" (is_avl !avl_tree);
  print_bst !avl_tree;

  avl_tree := delete_avl 20 !avl_tree;
  Printf.printf "Inorder traversal after deletion of 20: %s\n" (String.concat ", " (List.map string_of_int (inorder_traversal !avl_tree)));
  Printf.printf "Is AVL tree after deletion of 20: %b\n" (is_avl !avl_tree);
  print_bst !avl_tree;
