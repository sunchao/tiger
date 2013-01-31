(* $Id: hash_table.sml,v 1.1 2012/02/28 04:02:42 csun Exp $
 * A simple hash table 
 * TODO: improve hash function 
 *)

signature HASH_TABLE = 
sig
    type 'a table
    val insert : 'a table * string * 'a -> unit
    val lookup : 'a table * string -> 'a
    val pop : 'a table * string -> unit
    val clear : 'a table -> unit
end

structure HashTable : HASH_TABLE = 
struct

val INITIAL_SIZE = 109
val n_buckets = ref 0
val n_items = ref 0
type 'a bucket = (string * 'a) list
type 'a table = 'a bucket array ref

fun hash(s:string,size:int) =
        CharVector.foldl (fn(c,n) => (n*256+ord(c)) mod size) 0 s

fun insert(t:'a table,s:string,b:'a) = 
    let 
        val n = Array.length(!t)
        val h = hash(s,n)
        val rest = Array.sub(!t,h)
    in
        n_items := !n_items+1;
        case rest of 
            nil => n_buckets := !n_buckets+1
          | _ => ();
        if !n_items div !n_buckets > 2 
        then rehash(n) else ();
        Array.update(!t,h,(s,b)::rest)
    end

fun rehash(size:int) = 
    let val new_table = Array.array(size,nil) in
        n_items := 0;
        n_buckets := 0;
        Array.app(fn r =>
                     List.app (fn a => insert(ref new_table,#1a,#2a)) r) (!t);
        t := new_table
    end

        
exception NotFound

                                                   
fun lookup(t:'a table,s:string) =
    let
        val n = Array.length(!t)
        val h = hash(s,n)
        fun search((s',b)::rest) = if s=s' then b else search(rest)
          | search nil = raise NotFound
    in
        search(Array.sub(!t,h))
    end
            
fun pop(t:'a table, s:string) = 
    let 
        val n = Array.length(!t)
        val h = hash(s,n)
        val (s',b)::rest = Array.sub(!t,h)
    in Array.update(!t,h,rest)
    end        

fun clear(t:'a table) = Array.modify (fn _ => nil) (!t)
    
end
