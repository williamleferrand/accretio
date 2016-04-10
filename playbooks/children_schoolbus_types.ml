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
