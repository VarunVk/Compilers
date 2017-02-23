signature btreeSig=
sig
	type item
	type tree 
	val leq: item*item ->bool 
	val initTree: unit -> tree
	val insert: item*tree -> tree
	val find: item*tree->bool
	exception EmptyTree
	val top: tree ->item 
end

(* Different signatures for STRING and INT*)
signature STRING_ITEM=
sig
type item=string
val leq: item*item->bool
val initial:item
end

signature INT_ITEM=
sig
type item=int
val leq: item*item->bool
val initial:item
end

signature ITEM=
sig
type item
val leq: item*item->bool
val initial:item
end


functor btreeFct(Item:ITEM):btreeSig =
struct
	type item =Item.item;
	datatype tree = Empty
	        |Lf of item
	        |Nd of item*tree*tree
	
	fun leq(a:item,b:item):bool = Item.leq(a,b)
	fun max(p,q) = if  leq(p,q) then q else p
	fun min(p,q) = if  leq(p,q) then p else q
	
	
	(* Get the length of the longer sub tree *)
	fun Max(i:int,j:int)= if i<=j then j else i
	
	exception EmptyTree
	fun top Empty = raise EmptyTree |
	        top(Lf i) = i
	        |top (Nd (i,_,_))= i
	
	(* Recursively call len to get the depth of the tree*)
	fun len (Empty) = 0
	        |len (Lf _) = 1
	        |len (Nd(i,left,right))= Max(len left,len right)+1
	
	fun initTree () = Empty
	
	fun insert (i,Empty)= Lf(i)
	        |insert (i,Lf(j)) = 
				if leq(i,j)  then Nd(j,Lf(i),Empty) 
				else Nd(j, Empty,Lf(i)) 
	        |insert (i, Nd(j,l,r)) = 
				if leq (i,j) then insert (i ,l) 
				else insert(i,r)
	
	(*Recursively try to find the element of the binary tree*)
	fun find(i,Empty) = false
	        |find (i,Lf j) = 
				if leq(i,j) then false 
				else 
					if leq(j,i) then false 
					else true
	        |find (i, Nd(j,l,r)) = 
				if leq(i,j) then find(i,l) 
				else 
					if leq(j,i) then find (i,r) 
					else true
	
end
