type passportField =
  | BYR of string
  | IYR of string
  | EYR of string
  | HGT of string
  | HCL of string
  | ECL of string
  | PID of string
  | CID of string
;;

let rec string_to_list string =
  match string with
    "" -> []
  | _ -> (String.get string 0)::(string_to_list (String.sub string 1 (String.length string -1)))
;;

open Str
let input =
  let rec readFile (f: in_channel) =
    let current = ref [] in
    try
      let line = ref " " in

      while String.length !line > 0 do
        line := input_line f;
        current := (split (regexp "[ ]+") !line) @ !current;
      done;
      (!current)::(readFile f)
    with End_of_file -> close_in f; [!current] in

  let file = (open_in "input") in
  let fields = (readFile file) in
  List.map (fun passFields ->
      List.map (fun field ->
          match split (regexp ":") field with [x; y] -> (x,y)
        ) passFields)
    fields
;;

let isValidPassport passport = List.length (List.filter (fun (f, v) -> not (String.equal f "cid")) passport) = 7;;

let ans1 =
  let valid = List.filter isValidPassport input in
  List.fold_left (fun acc x -> acc + 1) 0 valid
;;

let rec hasValidateValues passport =
  match passport with
    [] -> true
  | (field, value)::rest ->
     let validValue = match field with
         "byr" -> let value = int_of_string value in value >= 1920 && value <= 2002
       | "iyr" -> let value = int_of_string value in value >= 2010 && value <= 2020
       | "eyr" -> let value = int_of_string value in value >= 2020 && value <= 2030
       | "hgt" ->
          let replaced = replace_first (regexp "[a-z]") " \\0" value in
          (match split (regexp " ") replaced with
            [x; "cm"] -> let x = int_of_string x in x >= 150 && x <= 193
           | [x; "in"] -> let x = int_of_string x in x >= 59 && x <= 76
           | _ -> false)
       | "hcl" -> string_match (regexp "^#[a-z0-9][a-z0-9][a-z0-9][a-z0-9][a-z0-9][a-z0-9]") value 0
       | "ecl" -> List.mem value ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
       | "pid" -> (string_match (regexp "[0-9]+") value 0) && (String.length value) = 9
       | "cid" -> true
       | _ -> false in

     print_string (if validValue then "valid: " else "invalid: ");
     print_string (field ^ ": " ^ value);
     print_newline ();

     validValue && (hasValidateValues rest)
;;

let ans2 =
  let valid = List.filter (fun p -> (isValidPassport p) && (hasValidateValues p)) input in
  List.fold_left (fun acc x -> acc + 1) 0 valid
;;
    
