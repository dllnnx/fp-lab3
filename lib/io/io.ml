let parse_point line =
  try
    let parts = String.split_on_char ' ' line in
    match parts with
    | [ x_str; y_str ] ->
        let x = float_of_string x_str in
        let y = float_of_string y_str in
        Some (x, y)
    | _ -> None
  with Failure _ -> None

let format_point (x, y) = Printf.sprintf "%.3f %.3f" x y

let read_points () =
  let rec read acc =
    try
      let line = input_line stdin in
      match parse_point line with
      | Some point -> read (point :: acc)
      | None -> read acc
    with End_of_file -> List.rev acc
  in
  read []

let read_point_stream () =
  try
    let line = input_line stdin in
    parse_point line
  with End_of_file -> None
