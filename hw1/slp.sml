signature IntSig =
sig
	type id
	type binop
	val Plus : binop
	val Minus : binop
	val Times : binop
	val Div : binop

	type stm
	type exp
	val CompoundStm : stm*stm -> stm
	val AssignStm   : id*exp -> stm
	val PrintStm    : exp list -> stm
	val IdExp       : id-> exp
	val NumExp      : int -> exp
	val OpExp       : exp*binop*exp -> exp
	val EseqExp     : stm*exp -> exp

	type sym_table
	val interp 		: (stm) -> unit 
	val interpStm   : (stm*sym_table) -> sym_table
	val interpExp   : (exp*sym_table) -> int*sym_table
	val maxargs 	: stm -> int 
	val Max 		: stm -> int
	val MaxExp		: exp -> int
end;


structure Interpreter : IntSig= struct
	type id = string
	datatype binop = Plus | Minus | Times | Div
	datatype stm = CompoundStm of stm*stm
	              | AssignStm of id*exp
	              | PrintStm of exp list
	     and exp = IdExp of id
	              | NumExp of int
	              | OpExp of exp*binop*exp
	              | EseqExp of stm*exp
	type sym_table = (id*int) list

	fun update(symbolTable:sym_table, sym:id, value:int): sym_table =
		(sym,value)::symbolTable
	
	fun lookup([], Sym:id) = [] 
		|lookup((Sym,Val)::b:sym_table, search:id) =
			if(Sym=search)
				then [Val]
			else lookup(b,search)

	(* function to interpret the Statements *)
	fun interpStm(CompoundStm(a,b), symbolTable:sym_table): sym_table =
	       interpStm(b, interpStm(a,symbolTable))
	
	   |interpStm(AssignStm(a,b), symbolTable:sym_table): sym_table =
	       let
	          val (value, newSymbolTable) = interpExp(b,symbolTable)
	       in
	          update(newSymbolTable, a, value)
	       end
	
	   |interpStm(PrintStm([]), symbolTable: sym_table): sym_table =
	       symbolTable
	
	   |interpStm(PrintStm(a::b), SymTbl:sym_table): sym_table =
	       let val (finalString, finalTable) =
	          let val (numberValue, newSymbolTable) =
	               interpExp(a, SymTbl)  ;
	          in
	               (Int.toString numberValue, newSymbolTable)
	          end
	       in
	          (print finalString;  print " "; interpStm(PrintStm(b),finalTable); finalTable)
	       end
	
	(* Function to interpret Expressions reursively*)
	and interpExp(IdExp(a), Table:sym_table): (int*sym_table) =
		let 
			val b=lookup(Table,a)
			val value=hd b
		in  
			(value, Table)
		end
	   |interpExp(NumExp(a), Table:sym_table): (int*sym_table) = (a, Table)

	   |interpExp(OpExp(a,Plus,b), Table:sym_table): (int*sym_table) =
	       let
	          val (res1, Tbl1) = interpExp(a, Table);
	          val (res2, Tbl2) = interpExp(b, Tbl1)
	       in
	          ((res1+res2), Tbl2)
	       end
	   |interpExp(OpExp(a, Minus, b), Table:sym_table): (int*sym_table) =
	       let
	          val (res1, Tbl1) = interpExp(a, Table)
	          val (res2, Tbl2) = interpExp(b, Tbl1)
	       in
	          ((res1 - res2), Tbl2)
	       end
	   |interpExp(OpExp(a, Times, b), Table:sym_table): (int*sym_table) =
	       let
	          val (res1, Tbl1) = interpExp(a, Table)
	          val (res2, Tbl2) = interpExp(b, Tbl1)
	       in
	          ((res1 * res2), Tbl2)
	       end
	   |interpExp(OpExp(a, Div, b), Table:sym_table): (int*sym_table) =
	       let
	          val (res1, Tbl1) = interpExp(a, Table)
	          val (res2, Tbl2) = interpExp(b, Tbl1)
	       in
	          ((res1 div res2), Tbl2)
	       end
	   |interpExp(EseqExp(a,restExprs), Table:sym_table): (int*sym_table) =
	       let
	          val Tbl2 = interpStm(a, Table)
	       in
	          interpExp(restExprs, Tbl2)
	       end

	(* Main function to call interpret the Compound Stm *)
	fun interp(a) = (interpStm(a,[]); ())
	

	fun Max(CompoundStm(a,b))
	    	=if Max(a) > Max(b) 
	        	then Max(a)  else Max(b) |
	    Max(AssignStm(a,b)) 
	    	= MaxExp(b) |
	    Max(PrintStm(a))=
		(* Handle print inside print situation here *)
	       let
	          val len = length a 
	          fun loopPrint([], length) = 0 |
			  	  loopPrint(last::[], length) = if length > MaxExp(last) then length
	                            				else MaxExp(last) |
				  loopPrint(first::rest, length) = if length > MaxExp(first) then loopPrint(rest,length)
												   else loopPrint(rest, MaxExp(first))
		   in 
				loopPrint(a,len)
	       end
	
	and 
	    MaxExp(IdExp(_))=0|
	    MaxExp(NumExp(_))=0|
	    MaxExp(OpExp(_,_,_))=0|
	    MaxExp(EseqExp(a,b)) = if Max(a)>= MaxExp(b) then Max(a) else MaxExp(b) 
	

	(* Main function to call maxargs *)
	fun maxargs(a) = Max(a) 

val prog = 
CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
		CompoundStm(AssignStm("b",
				EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
					OpExp(NumExp 10, Times, IdExp"a"))),
			PrintStm[IdExp "b"]))

val prog2 = 
CompoundStm(
		AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
		CompoundStm(AssignStm("b",
				EseqExp(PrintStm[IdExp"a",
					EseqExp(PrintStm [IdExp "a", 
						IdExp "a", 
						IdExp "a"],
						OpExp(IdExp"a", Minus,NumExp 1)),
					EseqExp(PrintStm[NumExp 4, 
						NumExp 4, 
						NumExp 4, 
						NumExp 4
					],
					OpExp( IdExp "a", Minus, NumExp 3))],
					OpExp(NumExp 10, Times, IdExp"a"))),
			PrintStm[IdExp "b"]))

val prog3 = 
CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
		CompoundStm(
			AssignStm("b",
				EseqExp(PrintStm[IdExp"a",
					EseqExp(AssignStm("a",
							OpExp(IdExp "a", 
								Times, 
								NumExp 5)),
						OpExp(IdExp"a", 
							Minus,
							NumExp 1))],
					OpExp(NumExp 10, Times, IdExp"a"))),
			PrintStm[IdExp "b"]))
end;
