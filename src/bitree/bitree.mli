(*
 * This file is part of Ocaml-aliases.
 *
 * Ocaml-quadtree is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * Ocaml-quadtree is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Ocaml-aliases. If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2012 Be Sport
 *)

type 'a t
(** the type of bitree *)

val init : ?cell_size:int -> int -> int -> 'a t
(** [init min max] creates a bitree to bind value between to
    int where [min] < int < [max]. *)

val clear : 'a t -> unit
(** [clear t] clear the bitree [t] *)

val insert : 'a t -> int -> 'a -> unit
(** [insert t i v] bind the key [i] to the value [v] in the bitree [t] *)

val remove : 'a t -> int -> 'a -> unit
(** [remove t i v] remove the key [i] with value [v] in the bitree [t] *)

val fold : 'a t -> int -> int -> ('b -> 'a -> 'b * bool) -> 'b -> 'b
(** [fold t lower upper f acc] fold over bindings between [lower] and [upper] bound in [t].
    The boolean indicate whether or not the fold should be stopped even if
    more matching bindings are available *)
