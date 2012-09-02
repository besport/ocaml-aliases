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

(* mostly inspired by https://github.com/khigia/ocaml-stringset *)

type string_cmp_i_t =
  | Eq
  | Prefix
  | Contain
  | Inf of int
  | Sup of int

let rec string_cmp_i s1 p1 l1 s2 p2 l2 =
  if p1 = l1 then
    begin
      if p2 = l2 then
        Eq
      else
        Prefix
    end
  else
    begin
      if p2 = l2 then
        Contain
      else
        let c1 = s1.[p1] in
        let c2 = s2.[p2] in
        if c1 = c2 then
          string_cmp_i s1 (p1 + 1) l1 s2 (p2 + 1) l2
        else if c1 < c2 then
          Inf p1
        else (* c1 > c2 *)
          Sup p1
    end

let string_cmp_i s1 p1 l1 s2 p2 l2 =
  if p1 > l1 then Prefix else string_cmp_i s1 p1 l1 s2 p2 l2


type 'a _n_t = (* node: Leaf, Trunk (for prefix inputs), Branch *)
  | L of string * 'a list
  | T of 'a _n_t * string * 'a list
  | B of 'a _n_t * 'a _n_t * int * char * string (* left, right, critical char index, char, key *)

type 'a t = 'a _n_t option

let create () = None

let merge (v:'a) (l:'a list) : 'a list =
  if List.mem v l
  then l
  else v::l

let filter (v:'a) (l:'a list) : 'a list = List.filter ((<>)v) l

let rec _bind j t s sl v =
  match t with
    | L(k, d) ->
        begin
          let kl = String.length k in
          match string_cmp_i s j sl k j kl with
            | Eq -> L(k, merge v d)
            | Prefix -> T(t, s, [v])
            | Contain -> T(L(s, [v]), k, filter v d)
            | Inf p -> B(L(s, [v]), t, p, s.[p], s)
            | Sup p -> B(t, L(s, [v]), p, k.[p], k)
        end
    | T(m, k, d) ->
	begin
          let kl = String.length k in
          match string_cmp_i s j sl k j kl with
            | Eq -> T(m, k, merge v d)
            | Prefix -> T(t, s, [v])
            | Contain -> T(_bind kl m s sl v, k, filter v d)
            | Inf p -> B(L(s, [v]), t, p, s.[p],s)
            | Sup p -> B(t, L(s, [v]), p, k.[p],k)
	end
    | B(l, r, i, b,k) ->
        begin
          match string_cmp_i s j sl k j (i+1) with
            | Prefix -> T(t, s, [v])
	    | Eq | Contain-> B(_bind i l s sl v, r, i, b, k)
            | Inf p ->
		if p = i
		then B(_bind i l s sl v, r, i, b, k)
		else B(L(s, [v]), t, p, s.[p],s)
	    | Sup p ->
		if p = i
		then B(l,_bind i r s sl v, i, b, k)
		else B(t,L(s,[v]), p, k.[p],k)
        end

let bind radix s v =
  match radix with
    | None -> Some(L(s, [v]))
    | Some t ->
        let sl = String.length s in
        Some(_bind 0 t s sl v)

let rec _remove j t s sl v =
  match t with
    | L(k, d) ->
        begin
          let kl = String.length k in
          match string_cmp_i s j sl k j kl with
            | Eq -> L(k, filter v d)
            | _ -> t
        end
    | T(m, k, d) ->
	begin
          let kl = String.length k in
          match string_cmp_i s j sl k j kl with
            | Eq -> T(m, k, filter v d)
            | Contain -> T(_remove kl m s sl v, k, d)
            | _ -> t
	end
    | B(l, r, i, b,str) ->
        begin
          let k = str in
          match string_cmp_i s j sl k j (i+1) with
            | Eq | Contain -> B(_remove i l s sl v, r, i, b, str)
	    | Sup p when p = i -> B(l,_remove i r s sl v, i, b, str)
	    | Inf p when p = i -> B(_remove i l s sl v, r, i, b, str)
            | _ -> t
	end

let remove radix s v =
  match radix with
    | None -> None
    | Some t ->
        let sl = String.length s in
        Some(_remove 0 t s sl v)


let rec fold_left f acc = function
  | [] -> acc,false
  | x::xs ->
      let acc,stop = f acc x in
      if stop then acc,stop else fold_left f acc xs

let rec _lookup j t s sl f acc =
  match t with
    | L(k, v) ->
	let kl = String.length k in
        (match string_cmp_i s j sl k j kl with
	  | Eq | Prefix -> fold_left f acc v
	  | _ -> acc,false
	)
    | T(m, k, v) ->
        let kl = String.length k in
        (match string_cmp_i s j sl k j kl with
	  | Eq | Prefix ->
	      let acc,stop = fold_left f acc v in
	      if stop then acc,stop else _lookup kl m s sl f acc
	  | Contain -> _lookup kl m s sl f acc
	  | _ -> acc,false
	)
    | B(l, r, i, b, k) ->
	(match string_cmp_i s j sl k j (i+1) with
	  | Eq | Contain -> _lookup i l s sl f acc
	  | Prefix ->
	      (* let pos = min (pred sl) i in *)
	      let acc,stop = _lookup i l s sl f acc in
	      if stop
	      then acc,stop
	      else _lookup i r s sl f acc
	  | Inf p when p = i -> _lookup i l s sl f acc
	  | Sup p when p = i -> _lookup i r s sl f acc
	  | _ -> acc, false)

let lookup radix s f acc =
  match radix with
    | None -> acc,false
    | Some t ->
        let sl = String.length s in
        _lookup 0 t s sl f acc

