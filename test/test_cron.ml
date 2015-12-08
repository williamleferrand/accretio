open OUnit2
open Cron
open Cron_parser
open CalendarLib

let test_parser_c1 _ =
  let result = crontab_of_string "* * * * * *" in
  assert_equal result each_minute

let test_parser_c2 _ =
  let result = crontab_of_string "45 17 7 6 * *" in
  assert_equal result
    {
      minute = Int 45 ;
      hour = Int 17 ;
      day_of_the_month = Int 7 ;
      month_of_the_year = Int 6 ;
      day_of_the_week = Star None ;
      year = Star None ;
    }

let test_parser_c3 _ =
  let result = crontab_of_string "*/15 */6 1,15,31 * 1-5 *" in
  assert_equal result
    {
      minute = Star (Some 15) ;
      hour = Star (Some 6) ;
      day_of_the_month = List ([ 1; 15; 31 ]) ;
      month_of_the_year = Star None ;
      day_of_the_week = Range ((1, 5), None) ;
      year = Star None ;
    }

let check_evaluator_c1 _ =
  let now = Calendar.now () in
  let base = Calendar.rem now (Calendar.Period.second (Calendar.second now)) in
  let top3 =
    Lwt_main.run
      (let stream = stream_of_crontab each_minute now in
       Lwt_stream.nget 3 stream) in
  assert_equal top3
    [
      Calendar.add base (Calendar.Period.minute 1) ;
      Calendar.add base (Calendar.Period.minute 2) ;
      Calendar.add base (Calendar.Period.minute 3) ;
    ]

let check_evaluator_c2 _ =
  let now = Calendar.now () in
  let base = Calendar.rem now (Calendar.Period.second (Calendar.second now)) in
  let crontab = crontab_of_string "59 12 * * 1 *" in
  let top3 =
    Lwt_main.run
      (let stream = stream_of_crontab crontab now in
       Lwt_stream.nget 3 stream) in
  List.iter
    (Printer.Calendar.print "%c\n")
    top3 ;
  assert_equal top3
    [
      Calendar.add base (Calendar.Period.minute 1) ;
      Calendar.add base (Calendar.Period.minute 2) ;
      Calendar.add base (Calendar.Period.minute 3) ;
    ]




let suite =
  "Cron" >::: [ "test_parser_each_minute" >:: test_parser_c1 ;
                "test_parser_every_year_on_june_7_at_17_45" >:: test_parser_c2 ;
                "test_parser_c3" >:: test_parser_c3 ;
                "check_evaluator_c1" >:: check_evaluator_c1 ]

let _ =
  run_test_tt_main suite
