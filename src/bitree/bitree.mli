type 'a t

val init : ?cell_size:int -> int -> int -> 'a t

val clear : 'a t -> unit

val insert : 'a t -> int -> 'a -> unit

val remove : 'a t -> int -> 'a -> unit

val fold : 'a t -> int -> int -> ('b -> 'a -> 'b * bool) -> 'b -> 'b
