module TestTypes = struct
  type point = float * float
  
  let test_point_equal (p1: point) (p2: point) =
    let (x1, y1) = p1 in
    let (x2, y2) = p2 in
    abs_float (x1 -. x2) < 0.001 && abs_float (y1 -. y2) < 0.001

  let pp_point fmt (x, y) = Format.fprintf fmt "(%.3f, %.3f)" x y

  let testable_point = Alcotest.testable pp_point test_point_equal
end

module TestInterpolations = struct
  open TestTypes

  let test_linear_interpolation_simple () =
    let points = [(0.0, 0.0); (2.0, 2.0)] in
    let result = Interpolations.linear_interpolation 1.0 points in
    let expected = [(1.0, 1.0)] in
    Alcotest.(check (list testable_point)) "simple linear interpolation" expected result

  let test_linear_interpolation_multiple_segments () =
    let points = [(0.0, 0.0); (1.0, 1.0); (2.0, 0.0)] in
    let result = Interpolations.linear_interpolation 0.5 points in
    let expected = [(0.5, 0.5); (1.5, 0.5)] in
    Alcotest.(check (list testable_point)) "multiple segments linear interpolation" expected result

  let test_linear_interpolation_empty_list () =
    let points = [] in
    let result = Interpolations.linear_interpolation 1.0 points in
    let expected = [] in
    Alcotest.(check (list testable_point)) "empty list" expected result

  let test_linear_interpolation_single_point () =
    let points = [(1.0, 1.0)] in
    let result = Interpolations.linear_interpolation 1.0 points in
    let expected = [] in
    Alcotest.(check (list testable_point)) "single point" expected result

  let test_lagrange_interpolation_step_simple () =
    let points = [(0.0, 0.0); (1.0, 1.0); (2.0, 4.0)] in
    let result = Interpolations.lagrange_interpolation_step 1.0 3 points in
    Alcotest.(check bool) "non-empty result" true (List.length result > 0)

  let test_lagrange_interpolation_insufficient_points () =
    let points = [(0.0, 0.0); (1.0, 1.0)] in
    let result = Interpolations.lagrange_interpolation_step 1.0 3 points in
    let expected = [] in
    Alcotest.(check (list testable_point)) "insufficient points" expected result
end

module TestIO = struct
  open TestTypes

  let test_parse_valid_point () =
    let result = Io.parse_point "1.5 2.3" in
    let expected = Some (1.5, 2.3) in
    Alcotest.(check (option testable_point)) "valid point parsing" expected result

  let test_parse_invalid_point () =
    let result = Io.parse_point "invalid input" in
    let expected = None in
    Alcotest.(check (option testable_point)) "invalid point parsing" expected result

  let test_parse_point_with_extra_spaces () =
    let result = Io.parse_point "  1.5   2.3  " in
    let expected = None in
    Alcotest.(check (option testable_point)) "point with extra spaces" expected result

  let test_format_point () =
    let point = (1.567, 2.891) in
    let result = Io.format_point point in
    let expected = "1.567 2.891" in
    Alcotest.(check string) "point formatting" expected result
end

let () =
  Alcotest.run "fp-lab3" [
    "interpolations", [
      Alcotest.test_case "Linear interpolation simple" `Quick TestInterpolations.test_linear_interpolation_simple;
      Alcotest.test_case "Linear interpolation multiple segments" `Quick TestInterpolations.test_linear_interpolation_multiple_segments;
      Alcotest.test_case "Linear interpolation empty list" `Quick TestInterpolations.test_linear_interpolation_empty_list;
      Alcotest.test_case "Linear interpolation single point" `Quick TestInterpolations.test_linear_interpolation_single_point;
      Alcotest.test_case "Lagrange interpolation step simple" `Quick TestInterpolations.test_lagrange_interpolation_step_simple;
      Alcotest.test_case "Lagrange interpolation insufficient points" `Quick TestInterpolations.test_lagrange_interpolation_insufficient_points;
    ];
    "io", [
      Alcotest.test_case "Parse valid point" `Quick TestIO.test_parse_valid_point;
      Alcotest.test_case "Parse invalid point" `Quick TestIO.test_parse_invalid_point;
      Alcotest.test_case "Parse point with extra spaces" `Quick TestIO.test_parse_point_with_extra_spaces;
      Alcotest.test_case "Format point" `Quick TestIO.test_format_point;
    ];
  ]
