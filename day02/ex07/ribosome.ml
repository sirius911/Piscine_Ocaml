(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ribosome.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/07/23 11:30:27 by clorin            #+#    #+#             *)
(*   Updated: 2023/07/23 14:01:51 by clorin           ###   ########.fr       *)
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

let print_nucleobase (nucleo:nucleotide) : unit = 
  Printf.printf "%s " (nucleobase_to_str(nucleo.nucleobase))

let generate_rna (h:helix) : rna = 
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
      print_string(nucleobase_to_str (head));print_char ' ';
      print_rna queue

(* ex07 *)
let rec generate_bases_triplets (r:rna) = match r with
    | a::b::c::queue -> (a,b,c)::generate_bases_triplets queue
    | _ -> []

let print_triplet (a,b,c) =
  Printf.printf "(%s-%s-%s) " (nucleobase_to_str a) (nucleobase_to_str b) (nucleobase_to_str c)

type aminoacid =
  | Stop (* UAA, UAG, UGA               : End of translation *)
  | Ala (* GCA, GCC, GCG, GCU           : Alanine *)
  | Arg (* AGA, AGG, CGA, CGC, CGG, CGU : Arginine *)
  | Asn (* AAC, AAU                     : Asparagine *)
  | Asp (* GAC, GAU                     : Aspartique *)
  | Cys (* UGC, UGU                     : Cysteine *)
  | Gln (* CAA, CAG                     : Glutamine *)
  | Glu (* GAA, GAG                     : Glutamique *)
  | Gly (* GGA, GGC, GGG, GGU           : Glycine *)
  | His (* CAC, CAU                     : Histidine *)
  | Ile (* AUA, AUC, AUU                : Isoleucine *)
  | Leu (* CUA, CUC, CUG, CUU, UUA, UUG : Leucine *)
  | Lys (* AAA, AAG                     : Lysine *)
  | Met (* AUG                          : Methionine *)
  | Phe (* UUC, UUU                     : Phenylalanine *)
  | Pro (* CCC, CCA, CCG, CCU           : Proline *)
  | Ser (* UCA, UCC, UCG, UCU           : Serine *)
  | Thr (* ACA, ACC, ACG, ACU           : Threonine *)
  | Trp (* UGG                          : Tryptophane *)
  | Tyr (* UAC, UAU                     : Tyrosine *)
  | Val (* GUA, GUC, GUG, GUU           : Valine *)
  | None

type protein = aminoacid list

let string_of_protein (p : protein) =
  let rec loop p result = match p with
         | Stop::queue -> loop queue (result ^ "EOT ")
         | Ala::queue -> loop queue  (result ^ "Alanine ")
         | Arg::queue -> loop queue  (result ^ "Arginine ")
         | Asn::queue -> loop queue  (result ^ "Asparagine ")
         | Asp::queue -> loop queue  (result ^ "Aspartique ")
         | Cys::queue -> loop queue  (result ^ "Cysteine ")
         | Gln::queue -> loop queue  (result ^ "Glutamine ")
         | Glu::queue -> loop queue  (result ^ "Glutamique ")
         | Gly::queue -> loop queue  (result ^ "Glycine ")
         | His::queue -> loop queue  (result ^ "Histidine ")
         | Ile::queue -> loop queue  (result ^ "Isoleucine ")
         | Leu::queue -> loop queue  (result ^ "Leucine ")
         | Lys::queue -> loop queue  (result ^ "Lysine ")
         | Met::queue -> loop queue  (result ^ "Methionine ")
         | Phe::queue -> loop queue  (result ^ "Phenylalanine ")
         | Pro::queue -> loop queue  (result ^ "Proline ")
         | Ser::queue -> loop queue  (result ^ "Serine ")
         | Thr::queue -> loop queue  (result ^ "Threonine ")
         | Trp::queue -> loop queue  (result ^ "Tryptophane ")
         | Tyr::queue -> loop queue  (result ^ "Tyrosine ")
         | Val::queue -> loop queue  (result ^ "Valine ")
         | _ -> result
  in
  loop p ""

let decode_arn (r:rna) : protein =
  let rec loop r result = match r with
    | (U,A,A)::(U,A,G)::_ -> result @ [Stop]
    | (G,C,A)::(G,C,C)::(G,C,G)::(G,C,U)::queue -> loop queue (result @ [Ala])
    | (A,G,A)::(A,G,G)::(C,G,A)::(C,G,C)::(C,G,G)::(C,G,U)::queue -> loop queue (result @ [Arg])
    | (A,A,C)::(A,A,U)::queue -> loop queue (result @ [Asn])
    | (G,A,C)::(G,A,U)::queue -> loop queue (result @ [Asp])
    | (U,G,C)::(U,G,U)::queue -> loop queue (result @ [Cys])
    | (C,A,A)::(C,A,G)::queue -> loop queue (result @ [Gln])
    | (G,A,A)::(G,A,G)::queue -> loop queue (result @ [Glu])
    | (G,G,A)::(G,G,C)::(G,G,G)::(G,G,U)::queue -> loop queue (result @ [Gly])
    | (C,A,C)::(C,A,U)::queue -> loop queue (result @ [His])
    | (A,U,A)::(A,U,C)::(A,U,U)::queue -> loop queue (result @ [Ile])
    | (C,U,A)::(C,U,C)::(C,U,G)::(C,U,U)::(U,U,A)::(U,U,G)::queue -> loop queue (result @ [Leu])
    | (A,A,A)::(A,A,G)::queue -> loop queue (result @ [Lys])
    | (A,U,G)::queue -> loop queue (result @ [Met])
    | (U,U,C)::(U,U,U)::queue -> loop queue (result @ [Phe])
    | (C,C,C)::(C,C,A)::(C,C,G)::(C,C,U)::queue -> loop queue (result @ [Pro])
    | (U,C,A)::(U,C,C)::(U,C,G)::(U,C,U)::queue -> loop queue (result @ [Ser])
    | (A,C,A)::(A,C,C)::(A,C,G)::(A,C,U)::queue -> loop queue (result @ [Thr])
    | (U,G,G)::queue -> loop queue (result @ [Trp])
    | (U,A,C)::(U,A,U)::queue -> loop queue (result @ [Tyr])
    | (G,U,A)::(G,U,C)::(G,U,G)::(G,U,U)::queue -> loop queue (result @ [Val])
    | _::queue -> loop queue result
    | _ -> result
  in
  loop (generate_bases_triplets(r)) []

let main () = 
  let h1 = generate_helix 500 in
  (* let getBoi n =
    match n with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | U -> "U"
    | _ -> "None"
  in *)

  List.iter print_nucleobase h1;
  Printf.printf "\n";

  let rn = generate_rna h1 in
  print_rna rn;
  Printf.printf "\n";
  
  List.iter print_triplet (generate_bases_triplets rn);
  Printf.printf "\n";

  Printf.printf "%s\n" (string_of_protein [Leu; His; Met; Stop]);
  
  let prtn = decode_arn rn in
  Printf.printf "%s\n" (string_of_protein prtn)

let () = main ()
