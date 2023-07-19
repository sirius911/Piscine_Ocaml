let repeat_string ?str:(nstr = "x") n =
  let rec repeat_str str count =
    if count <= 0 then
      ""
    else
      str ^ repeat_str str (count - 1)
  in
  if n < 0 then "Error"
  else 
    repeat_str nstr n


let main () =
    print_endline(repeat_string (-1));
    print_endline(repeat_string (0));
    print_endline(repeat_string ~str:"Toto" 1);
    print_endline(repeat_string 2);
    print_endline(repeat_string ~str:"a" 5);
    print_endline(repeat_string ~str:"what" 3)

let () = main ()