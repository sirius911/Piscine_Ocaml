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
  let dalek = new Dalek.dalek in
  let people = new People.people ("Max") in
  print_endline dalek#to_string;
  dalek#talk;
  dalek#exterminate people;
  print_endline dalek#to_string;
  doctor#use_sonic_screwdriver;
  dalek#die;
