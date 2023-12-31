(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_graphics.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/25 14:36:12 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/28 09:33:29 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

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

let draw_string x y str = 
  Graphics.moveto x y;
  Graphics.draw_string str

let draw_node x y value = 
  let size_l = 50 in
  let size_h = 20 in
  Graphics.moveto (x - (size_l / 2)) (y - (size_h / 2));
  Graphics.lineto (x - (size_l / 2)) (y + (size_h / 2));
  Graphics.lineto (x + (size_l / 2)) (y + (size_h / 2));
  Graphics.lineto (x + (size_l / 2)) (y - (size_h / 2));
  Graphics.lineto (x - (size_l / 2)) (y - (size_h / 2));
  draw_string (x-20) (y-5) value

let draw_tree_node (t : 'a tree) : unit =
  let rec loop x y size div t = match t with
      Node (a,b,c) -> Graphics.moveto ((x-size/2) + 5) y; Graphics.draw_string (string_of_int a); draw_square x y size;
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
	Graphics.open_graph " 1600x800" ;
	draw_square 50 50 50 ;
  draw_string 50 50 "Titi";

  let node = Node(42, Node(40, Nil, Node(41, Nil, Nil)), Node(56, Nil, Node(62, Node(60,Node(58, Nil, Nil),Nil), Nil))) in

  draw_tree_node node;
	ignore ( Graphics.read_key () ) ;
	Graphics.close_graph ()

let () = ignore(main ())
