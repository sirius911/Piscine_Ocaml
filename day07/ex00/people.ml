(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   people.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/07 22:14:21 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/07 22:39:40 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class people (name : string) = 
  object
      val _name : string = name
      val _hp : int = 100
      initializer print_endline "A people is created"
      method to_string = "Name : " ^ _name ^ " hp = " ^ string_of_int (_hp)
      method talk = Printf.printf "I'm %s ! Do you know the Doctor ?\n" _name
      method die = Printf.printf "Aaaarghh!"
end
