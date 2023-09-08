(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   doctor.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/07 22:14:21 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/07 22:39:40 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class doctor (name : string) (age : int) (people : People.people)= 
  object(self)
    val _name : string = name
    val mutable _age : int = age
    val _sidekick : People.people = people
    val mutable _hp : int = 100
    initializer print_endline "A doctor is created"
    method talk = Printf.printf "Hi! I'm the Doctor!\n"
    method use_sonic_screwdriver:unit = 
      print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
    method to_string:string = 
      "Doctor "^_name^" hp = "^string_of_int (_hp)^", age = "^string_of_int(_age)^", Sidekick = ["^_sidekick#to_string^"]\n"
    method travel_in_time (start : int) (arrival : int) : unit = 
    print_endline "
                _.--._
                _|__|_
    _____________|__|_____________
 .-'______________________________'-.
 | |________POLICE___BOX__________| |
 |  |============================|  |
 |  | .-----------..-----------. |  |
 |  | |  _  _  _  ||  _  _  _  | |  |
 |  | | | || || | || | || || | | |  |
 |  | | |_||_||_| || |_||_||_| | |  |
 |  | | | || || | || | || || | | |  |
 |  | | |_||_||_| || |_||_||_| | |  |
 |  | |  _______  ||  _______  | |  |
 |  | | |       | || |       | | |  |
 |  | | |       | || |       | | |  |
 |  | | |       | || |       | | |  |
 |  | | |_______| || |_______| | |  |
 |  | |  _______ @||@ _______  | |  |
 |  | | |       | || |       | | |  |
 |  | | |       | || |       | | |  |
 |  | | |       | || |       | | |  |
 |  | | |_______| || |_______| | |  |
 |  | |  _______  ||  _______  | |  |
 |  | | |       | || |       | | |  |
 |  | | |       | || |       | | |  |
 |  | | |       | || |       | | |  |
 |  | | |_______| || |_______| | |  |
 |  | '-----------''-----------' |  |
_|__|/__________________________\\|__|_ 
'----'----------------------------'----'"
      method set_hp (new_hp : int) : unit = 
        if new_hp <= 100 then
          _hp <- new_hp
        else  (*no sens but to test regenerate*)
          self#regenerate
      method private regenerate : unit = 
        _hp <- 100
end
