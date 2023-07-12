let repeat_string ~str:nstr n =
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
    print_endline(repeat_string ~str:"Toto" 3)

let () = main ()