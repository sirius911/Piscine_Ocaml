(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_graphics.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/25 14:36:12 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/25 15:07:12 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let main () = 
  Graphics.open_graph " 800x600";
  Graphics.moveto 400 300;
  Graphics.draw_string "42";
  Graphics.read_key ();;

let () = main ();;