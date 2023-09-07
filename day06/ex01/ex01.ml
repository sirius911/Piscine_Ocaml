(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex01.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: clorin <clorin@student.42.fr>              +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2023/09/07 12:34:24 by clorin            #+#    #+#             *)
(*   Updated: 2023/09/07 14:00:02 by clorin           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module HashString =
  struct
    type t = String.t
    let equal (str1 : t) (str2 : t) : bool = (String.compare str1 str2 = 0)
    let hash (str : t) : int =
      let len = String.length str in
      let rec hash_rec i ac =
        if i < len then
          let char_code = Char.code str.[i] in
          hash_rec (i + 1) (ac * 31 + char_code) 
        else
          ac
      in
      hash_rec 0 0

      let hash_to_string (str : t) : string =
        let hash_value = hash str in
        string_of_int hash_value
  end

module StringHashtbl = Hashtbl.Make (HashString)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ; "this is the end!"] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht;

  (* Affichage des clés et de leurs hachages *)
  List.iter (fun (k, v) ->
    let hash_str = HashString.hash_to_string k in
    Printf.printf "Key = \"%s\", Hash = %s, Value = \"%d\"\n" k hash_str v
  ) pairs
  