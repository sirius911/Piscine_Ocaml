(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gardening.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/28 08:41:56 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/28 09:37:09 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec size (t:'a tree) : int = match t with
  | Nil -> 0
  | Node (_, Nil, Nil) -> 0
  | Node (_, left, right) -> 1 + size left + size right

let rec height (t: 'a tree) : int = match t with
  | Nil -> 0
  | Node (_, Nil, Nil) -> 0
  | Node (_, left, right) -> 1 + max (height left) (height right)

let draw_square x y size =
  if size < 1 then
    ()
  else
  begin
    Graphics.moveto (x - (size / 2)) (y - (size / 2));
    Graphics.lineto (x - (size / 2)) (y + (size / 2));
    Graphics.lineto (x + (size / 2)) (y + (size / 2));
    Graphics.lineto (x + (size / 2)) (y - (size / 2));
    Graphics.lineto (x - (size / 2)) (y - (size / 2));
  end

let draw_tree_node (t : 'a tree) : unit =
  let rec loop x y size div t = match t with
      Node (a,b,c) -> Graphics.moveto ((x-size/2) + 5) y; Graphics.draw_string a; draw_square x y size;
                      let d = (1600 / div) in
                      Graphics.moveto (x - d) (y - size + size/2 - 10);
                      Graphics.lineto x (y - size/2);
                      loop (x - d) (y - size - 10) size (div*2) b;
                      Graphics.moveto (x + d) (y - size + size/2 - 10);
                      Graphics.lineto x (y - size/2);
                      loop (x + d) (y - size - 10) size (div*2) c;
    | Nil -> Graphics.moveto ((x-size/2) + 10) y; Graphics.draw_string "Nil"; draw_square x y size
  in
  loop 800 750 40 4 t

let main () =
  let node = Node("42", Node("40", Nil, Node("41", Nil, Nil)), Node("56", Nil, Node("62", Node("60",Node("59", Nil, Nil),Nil), Nil))) in
  print_string "Size of Node = ";
  print_int (size node);
  print_string "\nHeight of Node = ";
  print_int (height node);
  print_char '\n';
  Graphics.open_graph " 1600x800" ;
  draw_tree_node node;
	ignore ( Graphics.read_key () ) ;
	Graphics.close_graph ()

let () = main ()
