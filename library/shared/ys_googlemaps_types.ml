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

type status =
    OK
  | NOT_FOUND
  | ZERO_RESULTS
  | MAX_WAYPOINTS_EXCEEDED
  | INVALID_REQUEST
  | OVER_QUERY_LIMIT
  | REQUEST_DENIED
  | UNKNOWN_ERROR with yojson

type duration = {
  text : string ;
  value : int ;
} with yojson

type step = {
  duration : duration ;
  html_instructions : string
} with yojson

type leg = {
  duration : duration ;
  steps : step list
} with yojson

type route = {
  legs : leg list
} with yojson

type directions = {
  status : string ;
  routes : route list ;
} with yojson
