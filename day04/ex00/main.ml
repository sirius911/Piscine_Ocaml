let () =
    let spade = Color.Spade in
    let heart = Color.Heart in
    let diamond = Color.Diamond in
    let club = Color.Club in
    let rec map fonction list = 
      match list with
        | []            -> print_string ""
        | head::tail    -> print_endline (fonction head); map fonction tail
    in
    print_endline "---------------- individual --------------------";
    print_endline (Color.toString spade);
    print_endline (Color.toString heart);
    print_endline (Color.toString diamond);
    print_endline (Color.toString club);
    print_endline (Color.toStringVerbose spade);
    print_endline (Color.toStringVerbose heart);
    print_endline (Color.toStringVerbose diamond);
    print_endline (Color.toStringVerbose club);
    print_endline "--------------------- all ----------------------";
    map Color.toString Color.all;
    print_endline "------------- verbose -------------";
    map Color.toStringVerbose Color.all