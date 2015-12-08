/*
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
 *
 */

%{

open Ys_cron

%}

%token STAR
%token SPACE
%token SLASH
%token DASH
%token COMMA
%token EOF
%token <int> INT

%type <Ys_cron.t> crontab
%start crontab

%%

int_list:
  | INT COMMA int_list             { $1 :: $3 }
  | INT                            { [ $1 ] }

field:
  | STAR SLASH INT                 { Star (Some $3) }
  | STAR                           { Star None }
  | INT                            { Int $1 }
  | INT DASH INT SLASH INT         { Range (($1, $3), Some $5) }
  | INT DASH INT                   { Range (($1, $3), None) }
  | INT COMMA int_list             { List ($1 :: $3) }

crontab:
  | field SPACE field SPACE field SPACE field SPACE field SPACE field EOF
  {
    {
      minute = $1 ;
      hour = $3 ;
      day_of_the_month = $5 ;
      month_of_the_year = $7 ;
      day_of_the_week = $9 ;
      year = $11 ;
    }
  }

;

%%
