let linear_interpolation step points =
  match points with
  | [] | [ _ ] -> []
  | _ ->
      let rec generate_points_with_step (x1, y1) (x2, y2) acc =
        if x1 +. step >= x2 then acc
        else
          let ratio = step /. (x2 -. x1) in
          let y = y1 +. ((y2 -. y1) *. ratio) in
          generate_points_with_step
            (x1 +. step, y)
            (x2, y2)
            ((x1 +. step, y) :: acc)
      in
      let rec interpolate acc pts =
        match pts with
        | (x1, y1) :: (x2, y2) :: rest ->
            let intermediate_points =
              generate_points_with_step (x1, y1) (x2, y2) acc
            in
            interpolate intermediate_points ((x2, y2) :: rest)
        | _ -> acc
      in
      List.rev (interpolate [] points)

let lagrange points x =
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

let lagrange_interpolation_step step n points =
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

    let rec generate x acc =
      if x >= x_max then List.rev acc
      else generate (x +. step) ((x, lagrange window x) :: acc)
    in
    generate (x_min +. step) []
