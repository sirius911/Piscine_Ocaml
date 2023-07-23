(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   helix.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/22 11:16:41 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/23 10:43:11 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* ex04 *)

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

let nucleobase_to_str = function
| A -> "A"
| T -> "T"
| C -> "C"
| G -> "G"
| None -> "None"

let print_nucleotide = 
  
  function
  | {phosphate: _ ; deoxyribose: _; nucleobase: _} -> 
    print_endline ("phosphate = " ^ phosphate);
    print_endline ("deoxyribose = " ^ deoxyribose);
    print_endline ("nucleobase = " ^ (nucleobase_to_str nucleobase));
    print_char '\n'

(* ex05 *)
type helix = nucleotide list

let generate_helix (n:int) = 
  let random_nucleobase () =
		let rnd = Random.int 4 in
		match rnd with
		| 0 -> 'A'
		| 1 -> 'T'
		| 2 -> 'C'
		| 3 -> 'G'
		| _ -> 'X'
	in
  let rec loop (n:int) (result:helix) = 
    if n = 0 then
      result
    else
      loop (n - 1) ((generate_nucleotide (random_nucleobase ()))::result)
  in
  if n <= 0 then 
    []
  else
    loop (n:int) ([]:helix)

let helix_to_string (h : helix) =
  let rec loop h result = match h with
    | [] -> result
    | head::queue -> match head with 
        | {phosphate: _ ; deoxyribose: _; nucleobase: _} ->
          loop queue (result ^ (nucleobase_to_str nucleobase))
  in
  loop (h:helix) ""

let complementary_helix (h : helix) = 
  let complement_base (base:nucleobase) = match base with
    | A -> 'T'
    | T -> 'A' 
    | C -> 'G'
    | G -> 'C'
    | _ -> 'X'
  in
  let rec loop h result = match h with
    | [] -> result
    | head::queue ->
        let complement_nucleotide = generate_nucleotide ((complement_base(head.nucleobase))) in
        loop queue (result @ [complement_nucleotide])
  in     
  loop h []

let main () =
  Random.self_init() ;
  let h1 = generate_helix 3 in
  let h2 = complementary_helix h1 in
  let h3 = generate_helix (-42) in
  let h4 = generate_helix 10 in
  let h5 = complementary_helix h4 in
  
  print_string "Helix h1 = ";
  print_endline (helix_to_string h1);
  print_string "complementary of h1 = ";
  print_endline (helix_to_string h2);
  print_string "generate_helix (-42) = ";
  print_endline (helix_to_string h3) ;
  print_string (helix_to_string h4);
  print_string " -> ";
  print_endline (helix_to_string h5)

let () = main ()