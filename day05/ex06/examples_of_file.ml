(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   examples_of_file.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/05 14:59:50 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/06 11:39:26 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

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

let examples_of_file (file:string) : (float array * string) list = 
  let file_descriptor = fd file in
  let nb_line = count_lines_in_file file_descriptor in
  let arr = ref (Array.make nb_line "") in
  load_csv file_descriptor arr nb_line;
  extractLine !arr

let () =
  if Array.length Sys.argv != 2 then Printf.printf "Enter a file\n"
  else let l = examples_of_file Sys.argv.(1) in
       List.iter (fun x -> Array.iter (Printf.printf "%f ") (fst x);
       Printf.printf "%s\n" (snd x)) l
