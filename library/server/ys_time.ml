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


let now () = Int64.of_float (Unix.time ())

let add_hours t hours =
  Int64.add t (Int64.of_int (hours * 3600))

let to_string t =
  Int64.to_string t

(* date printing **************************************************************)

open CalendarLib

let print timestamp =
  let date = Calendar.from_unixfloat (Int64.to_float timestamp) in
  let local_date = Calendar.convert date Time_Zone.UTC Time_Zone.Local in
  let result = Printer.Calendar.sprint "%a %b %d %Y, %H:%M %p" local_date in
  Lwt_log.ign_info_f "timestamp: %Ld to %s" timestamp result ;
  result


let _ =
  Time_Zone.change Time_Zone.Local


(* date parsing ***************************************************************)

let to_timestamp (datetime, utc_offset) =
  try
    (* 2015-12-31T13:59 *)
    let calendar = Printer.Calendar.from_fstring "%FT%T" (datetime^":00") in
    let timestamp = Int64.of_float (Calendar.to_unixfloat calendar -. 60. *. float_of_int utc_offset) in
    Lwt_log.ign_info_f "timestamp is %Ld" timestamp ;
    (* Some timestamp *) None
  with exn -> Lwt_log.ign_info_f "couldn't convert %s: %s" datetime (Printexc.to_string exn) ; None

let to_twilio timestamp =
  let date = Calendar.from_unixfloat (Int64.to_float timestamp) in
  let result = Printer.Calendar.sprint "%Y-%m-%d" date in
  result
