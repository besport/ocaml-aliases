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
(** type of the imperative radix tree *)

val create : unit -> 'a t
(** [create ()] create an empty rafix tree *)

val clear : 'a t -> unit
(** [clear t] clears the radix tree [t] *)

val insert : 'a t -> string -> 'a -> unit
(** [insert tree str v] insert the key [str] with value
    [v] in the radix tree [tree] *)

val remove : 'a t -> string -> 'a -> unit
(** [remote tree str v] remove the key [str] with value
    [v] from the tree [tree] *)

val fold : 'a t -> string -> ('b -> 'a -> 'b * bool) -> 'b  -> 'b * bool
(** [fold tree str f a] fold over bindings in [tree] that matching [str] prefix.
    The boolean indicate whether or not the fold should be stopped even
    if more matching prefix exists. *)

val fold_with_max : 'a t -> max:int -> string -> ('b -> 'a -> 'b) -> 'b -> 'b
(** [fold ~max tree str f a] fold over binding in [tree] that matching [str] prefix.
    It will fold at most [max] time *)
