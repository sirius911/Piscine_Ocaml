let () = 
  let card1 = Card.newCard Card.Value.Jack Card.Color.Heart in
  let card2 = Card.newCard Card.Value.T10 Card.Color.Spade in
  print_endline(Card.toString card1);
  print_endline(Card.toStringVerbose card1);

  print_string "Max of ";
  print_string(Card.toStringVerbose card1);
  print_string " and ";
  print_string(Card.toStringVerbose card2);
  print_string " = ";
  print_endline (Card.toStringVerbose(Card.max card1 card2));

  print_string "Min of ";
  print_string(Card.toStringVerbose card1);
  print_string " and ";
  print_string(Card.toStringVerbose card2);
  print_string " = ";
  print_endline (Card.toStringVerbose(Card.min card1 card2));

  let control = Card.Color.Heart in
  print_string (Card.toStringVerbose card1);
  print_string " isOf ";
  print_string (Card.Color.toStringVerbose control);
  print_string " = ";
  Printf.printf "%b\n" (Card.isOf card1 control);
  print_string (Card.toStringVerbose card1);
  print_string " isHeart ";
  print_string " = ";
  Printf.printf "%b\n" (Card.isHeart card1);
  print_string (Card.toStringVerbose card1);
  print_string " isDiamond ";
  print_string " = ";
  Printf.printf "%b\n" (Card.isDiamond card1)
