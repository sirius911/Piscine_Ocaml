(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   nucleotides.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/22 09:57:55 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/22 10:52:19 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None

type nucleotide = {
  phosphate : phosphate;
  deoxyribose : deoxyribose;
  nucleobase : nucleobase;
}

let generate_nucleotide c = 
  let get_nucleobase c = match c with
    | 'A' -> A
    | 'T' -> T
    | 'C' -> C
    | 'G' -> G
    |  _  -> None
  in
  {
    phosphate = "phosphate";
    deoxyribose = "deoxyribose";
    nucleobase = (get_nucleobase c)
  }

  let print_nucleotide = 
    let nucleobase_to_str = function
      | A -> "A"
      | T -> "T"
      | C -> "C"
      | G -> "G"
      | None -> "None"
    in
    function
	  | {phosphate: _ ; deoxyribose: _; nucleobase: _} -> 
      print_endline ("phosphate = " ^ phosphate);
      print_endline ("deoxyribose = " ^ deoxyribose);
      print_endline ("nucleobase = " ^ (nucleobase_to_str nucleobase));
      print_char '\n'

  let main () =
    let a = generate_nucleotide 'A' in
    let t = generate_nucleotide 'T' in
    let c = generate_nucleotide 'C' in
    let g = generate_nucleotide 'G' in
    let none = generate_nucleotide 'N' in
  
    print_nucleotide a;
    print_nucleotide t; 
    print_nucleotide c;
    print_nucleotide g;
    print_nucleotide none
  
  let () = main ()