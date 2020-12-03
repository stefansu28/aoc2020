type protocol = Protocol of int * int * char;;

let input =
  let rec readFile (f: in_channel) =
    try
      let line = input_line f in
      let [protcol; password] = String.split_on_char ':' line in
      let [protcolRange; letter] = String.split_on_char ' ' protcol in
      let [low; high] = List.map int_of_string (String.split_on_char '-' protcolRange) in
      (Protocol (low, high, String.get letter 0), String.trim password)::(readFile f)
    with End_of_file -> close_in f; [] in

  let file  = (open_in "input") in
  readFile file
;;

let char_count char pass =
  let rec char_count_helper acc pass =
    let len = String.length pass in
    match len with
      0 -> acc
    | _ ->
       let inc = if char = String.get pass 0 then 1 else 0 in
       char_count_helper (acc + inc) (String.sub pass 1 (len-1)) in
  char_count_helper 0 pass
;;

let ans1 =
  let is_valid (Protocol(min, max, c), pass) =
    let count = char_count c pass in
    if count >= min && count <= max then
      true
    else
      false in

  List.fold_left (fun acc pass_pair ->
      if is_valid pass_pair then
        acc + 1
      else
        acc
    ) 0 input
;;

let ans2 =
  let is_valid (Protocol(first_index, second_index, c), pass) =
    let first = (String.get pass (first_index -1)) = c in
    let second = (String.get pass (second_index-1)) = c in
    (List.length (List.filter (fun x -> x) [first; second])) = 1 in

  List.fold_left (fun acc pass_pair ->
      if is_valid pass_pair then
        acc + 1
      else
        acc
    ) 0 input
;;

print_int ans1;
print_newline ();
print_int ans2;
print_newline ()
