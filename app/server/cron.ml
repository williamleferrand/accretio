(*
 * Accretio is an API, a sandbox and a runtime for social playbooks
 *
 * Copyright (C) 2015 William Le Ferrand
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


(* cron interface *)
(*

   * * * * * *
   | | | | | |
   | | | | | +-- Year              (range: 1900-3000)
   | | | | +---- Day of the Week   (range: 1-7, 1 standing for Monday)
   | | | +------ Month of the Year (range: 1-12)
   | | +-------- Day of the Month  (range: 1-31)
   | +---------- Hour              (range: 0-23)
   +------------ Minute            (range: 0-59)

*)

(*

Any of these 6 fields may be an asterisk ( * ). This would mean the entire range of possible values, i.e. each minute, each hour, etc. In the first four fields, nnCron users can also use "nonstandard" character ? (question mark), described here.

Any field may contain a list of values separated by commas, (e.g. 1,3,7) or a range of values (two integers separated by a hyphen, e.g. 1-5).

After an asterisk ( * ) or a range of values, you can use character / to specify that values are repeated over and over with a certain interval between them. For example, you can write "0-23/2" in Hour field to specify that some action should be performed every two hours (it will have the same effect as "0,2,4,6,8,10,12,14,16,18,20,22"); value "*/4" in Minute field means that the action should be performed every 4 minutes, "1-30/3" means the same as "1,4,7,10,13,16,19,22,25,28".

In Month and Day of Week fields, you can use names of months or days of weeks abbreviated to first three letters ("Jan,Feb,...,Dec" or "Mon,Tue,...,Sun") instead of their numeric values.

*)

open Ys_cron
open Cron_parser

exception Invalid

(* our representation of the crontab ******************************************)

(* some utils *****************************************************************)

let print crontab =
  let print_mod = function
      None -> ""
    | Some m -> Printf.sprintf "/%d" m
  in
  let print_field = function
    | Star m ->
      Printf.printf "Star(%s)\n" (print_mod m)
    | Range ((min, max), m) ->
      Printf.printf "Range(%d, %d, %s)\n" min max (print_mod m)
    | List (elems) ->
      Printf.printf "List(%s)\n"
        (String.concat "," (List.map string_of_int elems))
    | Int (i) ->
      Printf.printf "Int(%d)\n" i
  in
  print_field crontab.minute ;
  print_field crontab.hour ;
  print_field crontab.day_of_the_month ;
  print_field crontab.month_of_the_year ;
  print_field crontab.day_of_the_week ;
  print_field crontab.year

let validate crontab =
  let check_range min max = function
    | Star _ -> ()
    | Int n when n >= min && n <= max -> ()
    | Range ((min_, max_), _) when min_ >= min && max_ <= max && min_ <= max_ -> ()
    | List (_) -> ()
    | _ -> raise Invalid
  in
  check_range 0 59 crontab.minute ;
  check_range 0 23 crontab.hour ;
  check_range 1 31 crontab.day_of_the_month ;
  check_range 1 12 crontab.month_of_the_year ;
  check_range 1 7 crontab.day_of_the_week ;
  check_range 1900 1300 crontab.year ;;

(* an example *****************************************************************)

let each_minute =
  {
    minute = Star None ;
    hour = Star None ;
    day_of_the_month = Star None ;
    month_of_the_year = Star None ;
    day_of_the_week = Star None ;
    year = Star None ;
  }

(* now, our dates utils *******************************************************)

open CalendarLib

let crontab_of_string s =
  let lexbuf = Lexing.from_string s in
  Cron_parser.crontab Cron_lexer.token lexbuf

let evaluate crontab date =

  let check_modulus modulus d = d mod modulus == 0 in
  let check_range min max d = d >= min && d <= max in

  let check value = function
    | Star None -> true
    | Star (Some modulus) when check_modulus modulus value -> true
    | Range ((min, max), None) when check_range min max value -> true
    | Range ((min, max), Some modulus) when check_range min max value && check_modulus modulus value -> true
    | Int m when value = m -> true
    | List elements when List.mem value elements -> true
    | _ -> false
  in

  check (Calendar.minute date) crontab.minute
  && check (Calendar.hour date) crontab.hour
  && check (Calendar.days_in_month date) crontab.day_of_the_month
  && check (Calendar.Date.int_of_month (Calendar.month date)) crontab.month_of_the_year
  && check (Calendar.Date.int_of_day (Calendar.day_of_week date)) crontab.day_of_the_week
  && check (Calendar.year date) crontab.year

(* this is probably extremely inefficient *)

let stream_of_crontab crontab start =
  validate crontab ;
  let start = Calendar.rem start (Calendar.Period.second (Calendar.second start)) in
  let reference = ref start in
  let minutes =
    Lwt_stream.from_direct
      (fun () ->
         reference := Calendar.add !reference (Calendar.Period.minute 1) ;
         Some !reference) in
  Lwt_stream.filter (evaluate crontab) minutes
