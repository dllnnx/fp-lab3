let generate_points_in_range step x_min x_max f =
  let rec generate x acc =
    if x >= x_max then List.rev acc else generate (x +. step) ((x, f x) :: acc)
  in
  let first_point = ceil (x_min /. step) *. step in
  let start_x =
    if abs_float (first_point -. x_min) < 1e-10 then first_point +. step
    else first_point
  in
  generate start_x []

let linear_two_points (x1, y1) (x2, y2) x =
  let ratio = (x -. x1) /. (x2 -. x1) in
  y1 +. ((y2 -. y1) *. ratio)

let linear_interpolation step points =
  match points with
  | [] | [ _ ] -> []
  | _ ->
      let rec process_segments acc segments =
        match segments with
        | (x1, y1) :: (x2, y2) :: rest ->
            let segment_func x = linear_two_points (x1, y1) (x2, y2) x in
            let x_min = x1 in
            let x_max = x2 in
            let new_points =
              generate_points_in_range step x_min x_max segment_func
            in
            process_segments (new_points :: acc) ((x2, y2) :: rest)
        | _ -> List.flatten (List.rev acc)
      in
      process_segments [] points

let lagrange_two_points points x =
  let n = List.length points in
  if n < 2 then failwith "At least 2 points required for Lagrange interpolation"
  else
    let rec sum_terms i acc =
      if i >= n then acc
      else
        let xi, yi = List.nth points i in
        let rec product_terms j prod =
          if j >= n then prod
          else if j = i then product_terms (j + 1) prod
          else
            let xj, _ = List.nth points j in
            let factor = (x -. xj) /. (xi -. xj) in
            product_terms (j + 1) (prod *. factor)
        in
        let li = product_terms 0 1.0 in
        sum_terms (i + 1) (acc +. (yi *. li))
    in
    sum_terms 0 0.0

let lagrange_interpolation step n points =
  if List.length points < n then []
  else
    let window =
      let rec take k lst =
        if k <= 0 then []
        else match lst with x :: xs -> x :: take (k - 1) xs | [] -> []
      in
      take n points
    in

    let x_min = fst (List.hd window) in
    let x_max = fst (List.hd (List.rev window)) in

    generate_points_in_range step x_min x_max (lagrange_two_points window)
