(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/07 22:14:21 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/07 22:39:40 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let doctor = new Doctor.doctor ("Who") 42 (new People.people ("Cyrille")) in
  doctor#talk;
  doctor#travel_in_time 2018 1995;
  doctor#use_sonic_screwdriver;
  doctor#set_hp 90;
  print_endline doctor#to_string;
  doctor#set_hp 200; (* to test regenerate*)
  print_endline doctor#to_string;

