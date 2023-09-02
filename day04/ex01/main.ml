let () =
  let rec map fonction list = 
    match list with
      | []            -> print_string ""
      | head::tail    -> print_endline (fonction head); map fonction tail
  in

   
  let card = Value.T2 in
  print_string "card = [";
  print_string ( Value.toString (Some card));
  print_string "] => ";
  print_endline ( Value.toStringVerbose (Some card));

  print_endline "--------------------- all ----------------------";
  map Value.toString Value.all;
  print_endline "------------- verbose -------------";
  map Value.toStringVerbose Value.all;
  print_endline "------- Next -------";

  let card = Value.Jack in
  let nextCard = Value.next card in
  print_string "card = ";
  print_string (Value.toStringVerbose (Some card));
  print_string " [next] --> ";
  print_endline ( Value.toStringVerbose nextCard);

  print_endline "------- Previus -------";
  let previusCard = Value.previous card in
  print_string "card = ";
  print_string (Value.toStringVerbose (Some card));
  print_string " [previus] --> ";
  print_endline ( Value.toStringVerbose previusCard)