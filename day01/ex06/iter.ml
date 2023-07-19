let rec iter f x n = match n with
  | x  when x < 0 -> -1
  | 0 -> x
  | 1 -> f x
  | _ -> f (iter f x (n-1))

let main () =
  print_int(iter (fun x -> x*x) 2 4);
  print_char '\n';
  print_int(iter (fun x -> x*2) 2 4);
  print_char '\n';
  print_int(iter (fun x -> x / 2) 2 3)

let () = main ()