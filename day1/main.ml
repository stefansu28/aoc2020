let input =
  let rec readFile (f: in_channel) =
    try
      let line = int_of_string(input_line f) in
      line::(readFile f)
    with End_of_file -> close_in f; [] in

  let file  = (open_in "input") in
  readFile file
;;

let rec getPair first inputs =
  match inputs with
    [] -> None
  | h::rest when h + first = 2020 -> Some(first, h)
  | _::rest -> getPair first rest

let ans1 =
  let rec forFirst inputs =
    match inputs with
      [] -> None
    | h::rest ->
       match getPair h rest with
         None -> forFirst rest
       | Some(x, y) -> Some(x * y) in

  match forFirst input with
    Some(x) -> x
  | None -> failwith "no answer"
;;

let ans2 =
  let rec getTriple first inputs =
    match inputs with
      [] -> None
    | h::rest ->
       match getPair (first + h) rest with
         None -> getTriple first rest
       | Some(_, y) -> Some(first, h, y)  in

  let rec forFirst inputs =
    match inputs with
      [] -> None
    | h::rest ->
       match getTriple h rest with
         None -> forFirst rest
       | Some(x,y,z) -> Some(x * y * z) in

  match forFirst input with
    Some(x) -> x
  | None -> failwith "no answer"
;;

print_int ans1;
print_newline ();
print_int ans2;
print_newline ()
