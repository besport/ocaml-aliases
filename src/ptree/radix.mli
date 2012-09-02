(* 
 * This file is part of ocaml-ptree.
 *
 * OCaml-ptree is free software: you can redistribute it and/or 
 * modify it under the terms of the GNU Lesser General Public 
 * License as published by the Free Software Foundation.
 *
 * OCaml-ptree is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with ocaml-ptree. If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2012 Be Sport Inc 580 Howard St San Francisco CA USA
 *)

type 'a t

val create : unit -> 'a t

val bind : 'a t -> string -> 'a -> 'a t

val remove : 'a t -> string -> 'a -> 'a t

val lookup : 'a t -> string -> ('b -> 'a -> 'b * bool) -> 'b -> 'b * bool
