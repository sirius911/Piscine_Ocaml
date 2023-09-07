(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   one_nn.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/05 14:59:50 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/07 09:07:28 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type radar = (float array * string)

let fd (file:string) : in_channel =
  (*function that takes the name of a file, 
     opens it and returns the File Descriptor fd*)
  try open_in file with
      | _     -> begin
                      Printf.printf "Something bad happend when oppening '%s'\n" file;
                      exit 0
                  end

let count_lines_in_file (fd : in_channel) : int =
  (*Counts the number of lines in an open file (FD)*)
  let count = ref 0 in
  try
    while true do
      ignore (input_line fd);
      count := !count + 1;
    done;
    !count
  with
  | End_of_file ->
    seek_in fd 0;
    !count       

let load_csv (file: in_channel) (arr : string array ref) (nb : int) : unit =
  (*Function filling an array of character strings passed by reference*)
  let i = ref 0 in
  try
    while !i < nb do
      let line = try input_line file with
                  | _ -> ""
      in
      Array.set !arr !i line;
      incr i
    done
  with
  | End_of_file -> close_in file;()

let extractData (str:string) : (float array * string) = 
  (*returns a cuple with 1st element = a float array and 2nd element = a character string*)
    let ll = String.split_on_char ',' str in
    let nb_col = (List.length ll) - 1 in
    let arr_float = ref (Array.make nb_col 0.0) in
    let i = ref 0 in
    while !i < nb_col do
      Array.set !arr_float !i (float_of_string (List.nth ll !i));
      incr i
    done;
    (!arr_float, List.nth ll !i)
 
let extractLine (arr : string array) : (float array * string) list =
  let i = ref 0 in
  let liste = ref [] in
  while !i < Array.length arr do
    liste := extractData arr.(!i) :: !liste;
    incr i
  done;
  !liste

let examples_of_file (file:string) : radar list = 
  let file_descriptor = fd file in
  let nb_line = count_lines_in_file file_descriptor in
  let arr = ref (Array.make nb_line "") in
  load_csv file_descriptor arr nb_line;
  extractLine !arr

let eu_dist (a : float array) (b : float array) : float = 
  let i = ref 0 in
  let sum = ref 0.0 in
  while !i < Array.length a do
    begin
      sum.contents <- !sum +. ((a.(!i) -. b.(!i)) *. ((a.(!i) -. b.(!i)))) ;
      incr i
    end
  done;
  sqrt !sum

let trouve_min_int (lst : (int * float) list) : int =
  match lst with
  | [] -> 0
  | (h_int, h_float) :: tl ->
    let rec aux min_pair = function
      | [] -> fst min_pair 
      | (int_val, float_val) :: tail ->
        if float_val < snd min_pair then
          aux (int_val, float_val) tail
        else
          aux min_pair tail
    in
    aux (h_int, h_float) tl

let nearestRadar (num_radar:int) (radars : radar list) : int =
  let ret = ref [] in
  let j = ref 0 in
  let whichRadar = List.nth radars num_radar in
  let a = fst whichRadar in

  while !j < (List.length radars) do
      let b = fst (List.nth radars !j) in 
      let dist =  eu_dist a b in
      if !j != num_radar then
        ret := (!j, dist) :: !ret;
    incr j
  done;
  trouve_min_int (!ret)

let printRadar (r:radar):unit = 
  let flArray = fst r in
  let gb = snd r in
  Array.iter (Printf.printf "%f ") flArray;
  Printf.printf "'%s'" gb

let () =
  let radars = examples_of_file "ionosphere.data" in
  let n = 0 in
  let nr = nearestRadar n radars in
  Printf.printf "Pour le radar %d : \n" n;
  printRadar (List.nth radars n);
  Printf.printf "\nle radar avec la dist eucl la plus faible est le %d : \n" nr;
  printRadar (List.nth radars nr);
  print_endline "\nend"
