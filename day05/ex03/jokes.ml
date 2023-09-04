(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/04 17:56:33 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/04 19:10:25 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let count_lines_in_file (fd : in_channel) : int =
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

let fd (file:string) : in_channel =
  try open_in file with
      | _     -> begin
                      print_endline "Something bad happend";
                      exit 0
                  end

let load_jokes (file: in_channel) arr (nb : int) : unit =
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

let () = 
  Random.self_init();
  let file_name = "jokes.txt" in
  let fd_file = fd file_name in
  let num_lines = count_lines_in_file fd_file in
  (* Printf.printf "Le fichier %s contient %d lignes.\n" file_name num_lines; *)
  let arr = ref (Array.make num_lines "") in
  load_jokes fd_file arr num_lines;
  print_endline (!arr.(Random.int (Array.length !arr)))
