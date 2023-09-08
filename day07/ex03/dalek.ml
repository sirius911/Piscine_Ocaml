(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   dalek .ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/07 22:14:21 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/07 22:39:40 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class dalek = 
  object
    initializer Random.self_init()
    val _name : string =
      let random_chars = String.init 3 (fun _ ->
        let random_char = char_of_int (97 + Random.int 26) in 
        random_char
      ) in
      "Dalek" ^ random_chars
    val mutable _hp : int = 100
    val mutable _shield : bool = true
    method to_string:string = 
      _name^" hp = "^string_of_int(_hp)^" shield = "^string_of_bool(_shield)
    method talk:unit = 
      let phrases =  [
        "Explain! Explain!";
        "Exterminate! Exterminate!";
        "I obey!";
        "You are the Doctor! You are the enemy of the Daleks!"
      ] in
      Printf.printf "%s\n" (List.nth phrases (Random.int (List.length phrases)))
    method die = print_endline "Emergency Temporal Shift!"
    method exterminate (target:People.people):unit = 
      _shield <- not _shield;
      target#die;
end