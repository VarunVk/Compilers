signature PRINTTREE =
sig
 type stm
 type frag
 val printprog: TextIO.outstream -> (frag list) -> unit 
 val printtree : TextIO.outstream * stm list -> unit 
end

functor PrintTreeFun(structure T: TREE
                  structure F: FRAME
                  structure Temp: TEMP
                    sharing type T.label = Temp.label
                        and type T.exp = F.exp
                        and type T.stm = F.stm
                        and type T.temp = Temp.temp) : PRINTTREE
            where type stm = T.stm
              and type frag = F.frag =
struct

type stm = T.stm

type frag = F.frag

fun printtree (outstream, s0) =
 let fun say s =  TextIO.output(outstream,s)
  fun sayln s= (say s; say "\n") 

  fun indent 0 = ()
    | indent i = (say " "; indent(i-1))

  fun stm(T.SEQ(a,b),d) =
          (indent d; sayln "SEQ("; stm(a,d+1); sayln ","; stm(b,d+1); say ")")
    | stm(T.LABEL lab, d) = (indent d; say "LABEL "; say (Temp.labname lab))
    | stm(T.JUMP (e,_), d) =  (indent d; sayln "JUMP("; exp(e,d+1); say ")")
    | stm(T.CJUMP(r,a,b,t,f),d) = (indent d; say "CJUMP(";
				relop r; sayln ",";
				exp(a,d+1); sayln ","; exp(b,d+1); sayln ",";
				indent(d+1); say(Temp.labname t); 
				say ","; say (Temp.labname f); say ")")
    | stm(T.MOVE(a,b),d) = (indent d; sayln "MOVE("; exp(a,d+1); sayln ",";
			    exp(b,d+1); say ")")
    | stm(T.EXP e, d) = (indent d; sayln "EXP("; exp(e,d+1); say ")")

  and exp(T.BINOP(p,a,b),d) = (indent d; say "BINOP("; binop p; sayln ",";
			       exp(a,d+1); sayln ","; exp(b,d+1); say ")")
    | exp(T.MEM(e),d) = (indent d; sayln "MEM("; exp(e,d+1); say ")")
    | exp(T.TEMP t, d) = (indent d; say "TEMP t"; say(Temp.tempname t))
    | exp(T.ESEQ(s,e),d) = (indent d; sayln "ESEQ("; stm(s,d+1); sayln ",";
			  exp(e,d+1); say ")")
    | exp(T.NAME lab, d) = (indent d; say "NAME "; say (Temp.labname lab))
    | exp(T.CONST i, d) = (indent d; say "CONST "; say(Int.toString i))
    | exp(T.CALL(e,el),d) = (indent d; sayln "CALL("; exp(e,d+1);
			   app (fn a => (sayln ","; exp(a,d+2))) el;
			   say ")")

  and stmlst(sl) =
        let fun stmlst_aux [s] = (stm (s,0) ; sayln " "; sayln "]")
              | stmlst_aux (s::sl) = (stm (s,0); sayln "," ; stmlst_aux sl)
        in if (sl = nil) then say "[]"
           else (sayln "[" ; stmlst_aux sl)
        end

  and binop T.PLUS = say "PLUS"
    | binop T.MINUS = say "MINUS"
    | binop T.MUL = say "MUL"
    | binop T.DIV = say "DIV"
    | binop T.AND = say "AND"
    | binop T.OR = say "OR"
    | binop T.LSHIFT = say "LSHIFT"
    | binop T.RSHIFT = say "RSHIFT"
    | binop T.ARSHIFT = say "ARSHIFT"
    | binop T.XOR = say "XOR"

  and relop T.EQ = say "EQ"
    | relop T.NE = say "NE"
    | relop T.LT = say "LT"
    | relop T.GT = say "GT"
    | relop T.LE = say "LE"
    | relop T.GE = say "GE"
    | relop T.ULT = say "ULT"
    | relop T.ULE = say "ULE"
    | relop T.UGT = say "UGT"
    | relop T.UGE = say "UGE"

 in  stmlst s0; sayln ""; TextIO.flushOut outstream
end

fun printprog outstream frags =
  let fun printfrags [] = ()
	| printfrags (onefrag::restfrags) =
           (if (F.StringFrag onefrag) 
            then (TextIO.output(outstream,"LABEL ");
		  TextIO.output(outstream,((F.StringFragLab onefrag) ^ "\n"));
                  TextIO.output(outstream,
                                String.translate (fn x => if x = #"\"" 
                                                          then "\\\"" 
                                                          else String.str x)  
                                                 (F.StringFragStr onefrag) 
                                                               ^ "\"\n\n"))
	    else (TextIO.output(outstream,"LABEL ");
		  TextIO.output(outstream,(F.ProcFragLab onefrag) ^ "\n");
                  (printtree (outstream,(F.ProcFragBody onefrag)));
                  TextIO.output(outstream,"\n"));
	    (printfrags restfrags))
  in (TextIO.output (outstream,
                     "The fragments generated from the program: \n\n");
      printfrags frags)
  end

end

