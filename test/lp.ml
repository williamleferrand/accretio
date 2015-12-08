open Glpk

let solve =

  let capacity = 3 in

  let involvements =
    Array.of_list
      [
        (1, 1.0, 1, -.10.0) ;
        (2, 20.0, 1, 20.0) ;
        (3, 1.0, 1, -.15.0) ;
        (4, 3.0, 5, 10.0) ;
      ]
  in

  let bids = Array.map (fun (uid, bid, count, weight) -> weight) involvements in
  let counts = Array.map (fun (uid, bid, count, weight) -> float_of_int count) involvements in
  let ranges = Array.map (fun _ -> (0.0, 1.0)) involvements in
  let lp = make_problem Maximize
             bids
             [|
               counts ;
               (* [|10.; 4.; 5.|];
               [|2.; 2.; 6.|] *)
             |]
             [| -.infinity, float_of_int capacity  ; (* -.infinity, 600.; -.infinity, 300. *) |]
             ranges in

  set_class lp Mixed_integer_prog ;

  set_col_kind lp 0 Integer_var ;
  set_col_kind lp 1 Integer_var ;
  set_col_kind lp 2 Integer_var ;
  set_col_kind lp 3 Integer_var ;

  use_presolver lp true;
  simplex lp ;
  branch_and_bound_opt lp;
  let prim = get_col_primals lp in

  let total = ref 0.0 in
  for i = 0 to 3 do
    let _, bid, _, _ = involvements.(i) in
    total := (bid *. prim.(i)) +. !total
  done ;

  Printf.printf "total: %f Z: %g    x0: %g    x1: %g    x2: %g    x3: %g\n%!" !total (get_obj_val lp) prim.(0) prim.(1) prim.(2) prim.(3)
