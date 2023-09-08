(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   army.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/07 22:14:21 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/07 22:39:40 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class['a] army = 

  object(self)
    val mutable _member:'a list = []
    initializer print_endline "An army is created"
    method die = print_endline "The Army is destroyed"
    method add member = 
      print_endline "a member is comming in an Army";
      _member <- _member @ [member]
    method get_army:'a list = _member
    method delete:unit = match _member with
      | [] -> self#die
      |head::tail -> _member <- tail
end