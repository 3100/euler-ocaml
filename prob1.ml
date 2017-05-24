(* If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
 * Find the sum of all the multiples of 3 or 5 below 1000.
 *)

let rec sum total next endVal =
  match next with
    n when n = endVal -> total
  | n when (n mod 3 = 0 || n mod 5 = 0) -> sum (total+n) (n+1) endVal
  | _ -> sum total (next+1) endVal;;

let solve =
  sum 0 1 1000;;

let _ =
  print_int solve;
  print_newline () ;;
