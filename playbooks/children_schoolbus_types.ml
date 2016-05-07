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

type child = {
  name : string ;
  age_string : string ;
  age_in_months : int ;
} with yojson

type profile = {
  uid : int ;
  email : string ;
  name : string ;
  children : child list ;
  neighborhood : string ;
  schedule : string ;
  groups : string list ;
} with yojson

type profile_field = Name | Children | Neighborhood | Schedule with yojson
type profile_fields = profile_field list with yojson

type quote_request = {
  reference : string ;
  route : Ys_googlemaps_types.route ;
  comment : string ;
} with yojson

type quote = {
  reference : string ;
  number_of_seats : int ;
  cost : float ;
  currency : string ;
  description : string ;
} with yojson

type quotes = quote list with yojson

type quote_reply = Error of string | Quotes of quote list with yojson

type date =
  {
    year : int ;
    month : int ;
    day : int ;
  } with yojson

type time =
  {
    hour : int ;
    minute : int ;
  } with yojson

type time_or_timerange = Time of time | TimeRange of (time * time) with yojson

type activity_step =
  {
    step_time : time_or_timerange ;
    step_description : string ;
  } with yojson

type attachment =
  {
    filename : string ;
    content_type : string ;
    content : string ;
  } with yojson

type activity_confirmed = {
  activity_number_of_spots : int ;
  activity_price_per_spot : int ;
  activity_price_description : string ;
  activity_price_remark : string ;
} with yojson

type activity_suggestion = {
  activity_suggestion : string ;
} with yojson

type activity_status =
    Confirmed of activity_confirmed
  | Suggestion of activity_suggestion with yojson

type booking =
  {
    booking_member : int ;
    booking_count : int ;
    booking_date : int64 ;
    booking_payment : int ;
  } with yojson

type activity =
  {
    activity_uid : int ;
    activity_reference : string ;
    activity_min_age_in_months : int ;
    activity_max_age_in_months : int ;
    activity_date : date ;
    activity_title : string ;
    activity_description : string ;
    activity_summary : string ;
    activity_steps : activity_step list ;
    activity_status : activity_status  ;
    activity_attachments : attachment list ;
    activity_bookings : booking list ;
  } with yojson

type request_lock_spots =
  {
    request_lock_spots_activity_uid : int ;
    request_lock_spots_count : int ;
  } with yojson

type lock_spots =
  {
    lock_spots_activity_uid : int ;
    lock_spots_count : int ;
    lock_attachments : attachment list ;
    lock_until : int64 ;
  } with yojson

type reply_lock_spots = EventFull | EventLock of lock_spots with yojson

type request_confirm_booking =
  {
    request_confirm_booking_activity : int ;
    request_confirm_booking_payment : int ;
  } with yojson
