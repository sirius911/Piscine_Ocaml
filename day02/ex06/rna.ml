(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   rna.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/23 10:44:01 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/23 11:28:45 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


(* ex04 *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None

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
    | 'U' -> U
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
| U -> "U"
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

(* ex06 *)
type rna = nucleobase list

let generate_rna (h:helix) = 
  let compl_base_rna (base:nucleobase) = match base with
  | A -> U 
  | T -> A
  | C -> G 
  | G -> C 
  | U -> U 
  | _ -> None
  in
  let rec loop (h:helix) (result:rna) = match h with
  | [] -> result
  | head::queue ->
    loop queue (result @ [compl_base_rna head.nucleobase])
  in
  loop h []

let rec print_rna = function
  | [] -> ()
  | head::queue -> 
      print_string(nucleobase_to_str (head));
      print_rna queue

let main () =
  Random.self_init() ;
  let h1 = generate_helix 5 in	
  let rna = generate_rna h1 in	
  print_endline (helix_to_string h1) ;
  print_rna rna ; print_char '\n'

let () = main ()