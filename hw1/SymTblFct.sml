(*Signature for inidividual components*)
signature IntMapSig=
sig
  type 'a map
  exception NotFound
  val apply: 'a map * int -> 'a
  val update: 'a map * int * 'a -> 'a map
  val emptyMap: 'a map
end;

signature ValSig =
sig
  type value
end;

signature SymSig=
sig
  eqtype sym
  val hash: sym -> int
end;

(*Individual functors for Sym, IntMap and Val*)
functor IntFct():IntMapSig=
struct
	type 'a map= int list 
	exception NotFound
  	fun apply(a:'a map,b:int)=3 
  	(*fun update(a:'a map ,b:int ,c:'a)=a   (*i am not able to define this function*) *)
  	val emptyMap= 0 
end 

functor SymFct():SymSig=
struct 
	   type sym= string 
	   fun hash (a:sym) = 3
end 

functor ValFct():ValSig=
struct 
	   type value = int 
end 

signature SymTblSig=
sig 
  type table
  exception Lookup
  val lookup : table * Sym.sym -> Val.value
  val update : table * Sym.sym * Val.value -> table
end


functor SymTblFct(
   structure IntMap : IntMapSig
   structure Val : ValSig
   structure Sym : SymSig) :SymTblSig =

 struct 
   datatype table = TBL of 
   (Sym.sym * Val.value) list IntMap.map

   exception Lookup

   fun find (sym,[]) = raise Lookup
     | find (sym, (sym',v)::rest) =
          if sym = sym' then v
          else find (sym,rest)

   fun lookup (TBL map, s) =
          let val n = Sym.hash(s)
              val l = IntMap.apply(map,n)
          in find (s,l)
          end handle IntMap.NotFound => raise Lookup

(* Update function to update the value in the list*)
  fun update(TBL map,s,v)=
   let val n = Sym.hash(s)
       val l = IntMap.apply(map,n) handle IntMap.NotFound => []
       val newmap= IntMap.update(map,n,(s,v)::l)
    in TBL newmap
   end 
 end;
