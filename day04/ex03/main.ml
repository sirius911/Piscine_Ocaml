(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/04 12:37:33 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/04 14:36:07 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec bataille deck1 deck2 =
  let sc1 = ref 0 in
  let sc2 = ref 0 in

  let rec loop d1 d2 =  
    let (card1, d1) = Deck.drawCard d1 in
    let (card2, d2) = Deck.drawCard d2 in
    print_string (Deck.Card.toString card1);
    print_string " vs ";
    print_string (Deck.Card.toString card2);
    print_string " --> ";
    let score = Deck.Card.compare card1 card2 in
    if score = 0 then
      print_endline "Bataille !"
    else if score = 1 then
      (let () = sc1 := !sc1 + 1 in
      print_endline "1")
    else
      (let () = sc2 := !sc2 + 1 in
      print_endline "2");
    if (Deck.isEmpty d1 = true || Deck.isEmpty d2 = true) then
        begin
          print_endline "Fin";
          print_string "score joueur1 = ";
          print_int !sc1;
          print_string "\nscore joueur2 = ";
          print_int !sc2;
          print_endline ""
        end
    else
      loop d1 d2
  in
  loop deck1 deck2
  
let () = 
  let myDeck = Deck.newDeck () in
  print_endline "*** New Deck ***";
  List.iter (Printf.printf "%s ") (Deck.toStringList (myDeck));
  print_endline "";
  List.iter (Printf.printf "%s ") (Deck.toStringListVerbose (myDeck));

  let yourDeck = Deck.newDeck () in
  print_endline "\n*** Bataille ***";
  bataille myDeck yourDeck