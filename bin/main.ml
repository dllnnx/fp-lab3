open Io

type algorithm = Linear | Lagrange of int

let parse_args () =
  let args = Array.to_list Sys.argv in
  let rec parse_algorithms algorithms step args =
    match args with
    | "--linear" :: rest -> parse_algorithms (Linear :: algorithms) step rest
    | "--lagrange" :: n :: rest -> (
        try
          parse_algorithms (Lagrange (int_of_string n) :: algorithms) step rest
        with Failure _ -> parse_algorithms algorithms step rest)
    | "--step" :: s :: rest -> (
        try parse_algorithms algorithms (float_of_string s) rest
        with Failure _ -> parse_algorithms algorithms step rest)
    | _ :: rest -> parse_algorithms algorithms step rest
    | [] -> (algorithms, step)
  in
  let algs, step = parse_algorithms [] 0.1 (List.tl args) in
  (* skip program name *)
  (List.rev algs, step)

let interpolate_stream algorithm step points =
  match algorithm with
  | Linear -> Interpolations.linear_interpolation step points
  | Lagrange n -> Interpolations.lagrange_interpolation_step step n points

let get_prefix = function Linear -> "linear:" | Lagrange _ -> "lagrange:"

let print_points prefix points =
  List.iter
    (fun point -> Printf.printf "%s %s\n" prefix (format_point point))
    points

let process_stream algorithms step =
  let window_size =
    List.fold_left max 2
      (List.map (function Linear -> 2 | Lagrange n -> n) algorithms)
  in

  let rec loop window =
    match Io.read_point_stream () with
    (* one point at a time *)
    | Some point ->
        flush stdout;

        let new_window =
          if List.length window >= window_size then
            (* remove oldest point and add new one *)
            point :: List.rev (List.tl (List.rev window))
          else point :: window
        in

        if List.length new_window >= window_size then
          let reversed_window = List.rev new_window in
          List.iter
            (fun algorithm ->
              let interpolated =
                interpolate_stream algorithm step reversed_window
              in
              let prefix = get_prefix algorithm in
              print_points prefix interpolated;
              flush stdout)
            algorithms
        else ();

        loop new_window
    | None -> ()
  in
  loop []

let main () =
  let algorithms, step = parse_args () in
  if List.length algorithms > 0 then process_stream algorithms step
  else
    Printf.printf "Usage: %s [--linear] [--lagrange n] [--step s]\n"
      Sys.argv.(0);
  exit 1

let () = main ()
