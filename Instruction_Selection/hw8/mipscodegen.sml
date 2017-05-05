signature MIPSCODEGEN =
sig
   type stm
   type instr

   val codegen : stm list -> instr list
end


functor MipsCodeGenFun(structure IR : TREE
                       structure A : ASSEM
                       structure Temp : TEMP
                       structure MipsRegs : MIPSREGS
                       structure Frame : FRAME
                        sharing type A.label = IR.label = Temp.label
                            and type MipsRegs.temp = A.temp
                                       = IR.temp = Temp.temp) :
               MIPSCODEGEN
                 where type stm = IR.stm
                   and type instr = A.instr =
struct

type stm = IR.stm
type instr = A.instr

(* this function is needed to get the minus sign right for MIPS code *)
fun InttoString i =
      if (i < 0)
      then "-" ^ (Int.toString (0-i))
      else Int.toString i

      fun relopToString IR.EQ = "beq"
        | relopToString IR.NE = "bne"
        | relopToString IR.LT = "blt"
        | relopToString IR.GT = "bgt"
        | relopToString IR.LE = "ble"
        | relopToString IR.GE = "bge"
        | relopToString _ = "XXX"



fun codegen (stml : stm list) : instr list =

      let val ilist = ref (nil: instr list)

          fun emit x = ilist := x :: !ilist

          fun munchStmList nil = ()
            | munchStmList (s::sl) = (munchStm s; munchStmList sl)

          (* some cases filled in to show how to proceed; need to fill
             in others in place of ... to get complete coverage.
             Make sure to understand the role of destinations, sources
             and jumps before starting. Also pay attention to using MOVE
             for register to register transfer; this will permit their
             elimination where possible later *)
          and munchStm (IR.LABEL l) = emit(A.LABEL
                                             {assem = Temp.labname l ^ ":\n",
                                              lab = l})
            | munchStm (IR.JUMP(IR.NAME l,_)) =
                   emit(A.OPER {assem = "j `j0\n", dst = nil,
                                src = nil, jump = (SOME [l])})
            | munchStm (IR.JUMP(e,ll)) =
                   emit(A.OPER {assem = "jr `s0\n", dst = nil,
                                src = [munchExp e],jump = (SOME ll)})

            | munchStm (IR.MOVE (IR.TEMP t1,IR.TEMP t2)) =
              emit(A.MOVE {assem ="move `d0,`s0\n",dst = t1, src = t2})

	    (* All Store operations - Move into Memory *)

	    | munchStm (IR.MOVE (IR.MEM(IR.BINOP(IR.PLUS, exp1, IR.CONST c)), exp2))
	      = emit(A.OPER {assem="sw `s0," ^InttoString c ^ "(`s1)\n",
			     src=[munchExp exp2, munchExp exp1], dst=[], jump=NONE})

	    | munchStm (IR.MOVE ( IR.MEM(IR.BINOP(IR.PLUS, IR.CONST c, exp1)), exp2))
	      = emit (A.OPER {assem="sw `s0," ^InttoString c^ "(`s1)\n",
			      src=[munchExp exp2, munchExp exp1], dst=[], jump=NONE})

	    | munchStm (IR.MOVE ( IR.MEM(IR.BINOP(IR.MINUS, exp1, IR.CONST c)), exp2))
	      = emit (A.OPER {assem="sw `s0," ^InttoString c^ "(`s1)\n",
			      src=[munchExp exp2, munchExp exp1], dst=[], jump=NONE})

	    | munchStm (IR.MOVE (IR.MEM(exp1), exp2))
	      = emit (A.OPER {assem="sw `s0,0(`s1)\n",
			      src=[munchExp exp2] , dst=[munchExp exp1], jump=NONE})

	    (* Load operations move from Memory to Temp  *)
            | munchStm(IR.MOVE(IR.TEMP(t), IR.MEM(IR.BINOP(IR.PLUS, exp, IR.CONST c))))
	      = emit(A.OPER {assem="lw `d0," ^ Int.toString c ^ "(`s0)\n",
                          src=[munchExp exp], dst=[t], jump=NONE}
              )
            | munchStm(IR.MOVE(IR.TEMP(t), IR.MEM(IR.BINOP(IR.PLUS, IR.CONST c, exp))))
	      = emit(A.OPER {assem="lw `d0," ^ (Int.toString c) ^ "(`s0)\n",
                          src=[munchExp exp], dst=[t], jump=NONE}
              )
            | munchStm(IR.MOVE(IR.TEMP(t), IR.MEM(IR.BINOP(IR.MINUS, exp, IR.CONST c))))
	      = emit(A.OPER {assem="lw `d0," ^ Int.toString c ^ "(`s0)\n",
                          src=[munchExp exp], dst=[t], jump=NONE}
              )
            | munchStm(IR.MOVE(IR.TEMP(t), IR.NAME(label)))
	      = emit(A.OPER {assem="la `d0," ^ (Temp.labname label) ^ "\n",
                          src=[], dst=[t], jump=NONE}
              )
            | munchStm(IR.MOVE(IR.TEMP(t), IR.CONST c))
	      = emit(A.OPER {assem="li `d0," ^ Int.toString c ^ "\n",
                          src=[], dst=[t], jump=NONE}
              )
            | munchStm(IR.MOVE(IR.TEMP(t), e))
	      = emit(A.MOVE {assem="move `d0,`s0\n",
                          src=munchExp e, dst=t}
              )

	    (* Handling SEQ in stm *)
	    | munchStm ( IR.MOVE (IR.ESEQ(stm1, IR.MEM(exp1)), exp2))
	      =(munchStm stm1;
		emit(A.OPER {assem="sw `s0,(`d0)\n",
			     src=[munchExp exp2],
			     dst=[munchExp exp1],
			     jump=NONE}))

	    | munchStm (IR.MOVE(IR.ESEQ(s1, IR.TEMP(t1)), e1))
	      = (munchStm s1;
		 emit(A.MOVE {assem="move `d0,`s0\n",
                              src=munchExp e1,
			      dst=t1}
              ))

            | munchStm(IR.MOVE(IR.ESEQ(s1, e1loc as IR.ESEQ _), e1)) =
              (munchStm s1; munchStm(IR.MOVE(e1loc, e1)))

	    (* Handling exp in stm*)
            | munchStm (IR.EXP(e)) = (munchExp e ; ())

	    (* Handling CJUMP in stm *)
            | munchStm (IR.CJUMP (relop, exp1, exp2, lab1, lab2)) =
                  emit (A.OPER {
                  assem=(relopToString relop) ^ " `s0,`s1," ^ Temp.labname lab1 ^ "\n",
                  dst=[],
                  src=[munchExp exp1, munchExp exp2],
                  jump=SOME[lab1,lab2]})


         (* has the type IR.exp -> Temp.temp, i.e. returns a register
            that will bear the results of evaluation the expression *)
         and munchExp (IR.CONST c) =
                  let val t = Temp.newtemp()
                  in emit(A.OPER {assem ="li `d0," ^ (InttoString c) ^ "\n",
                                  dst = [t],src = nil,jump = NONE}) ;
                     t
                  end
           | munchExp (IR.NAME lab) =
             let val t = Temp.newtemp()
             in emit(A.OPER {assem ="la `d0," ^ (Temp.labname lab) ^ "\n",
                             dst = [t],src = nil,jump = NONE}) ;
                t
             end
           | munchExp (IR.TEMP t) = t
	   | munchExp (IR.MEM(IR.BINOP(IR.PLUS, exp, IR.CONST c))) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER {assem="lw `d0," ^ InttoString c ^ "(`s0)"^"\n",
			      src=[munchExp exp], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.MEM(IR.BINOP(IR.PLUS, IR.CONST c,exp))) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER {assem="lw `d0," ^ InttoString c ^ "(`s0)"^"\n",
			      src=[munchExp exp], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.MEM(IR.BINOP(IR.MINUS, exp, IR.CONST c))) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER {assem="lw `d0," ^ InttoString c ^ "(`s0)"^"\n",
			      src=[munchExp exp], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.MEM(exp)) =
	     let
		 val t = Temp.newtemp()
	     in
		 emit(A.OPER{assem="lw `d0,0(`s0)\n", src=[munchExp exp], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.BINOP(IR.PLUS, exp, IR.CONST c)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="addi `d0,`s0,"^ Int.toString c ^"\n",
			     src=[munchExp exp], dst=[t], jump=NONE});
	       t
	     end
	   | munchExp (IR.BINOP(IR.PLUS, IR.CONST c, exp)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="addi `d0,`s0,"^ Int.toString c ^"\n",
			     src=[munchExp exp], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.BINOP(IR.PLUS, exp1, exp2)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="add `d0,`s0,`s1\n",
			     src=[munchExp exp1, munchExp exp2], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.BINOP(IR.MINUS, exp, IR.CONST c)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="sub `d0,`s0," ^(InttoString c) ^ "\n",
			     src=[munchExp exp], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.BINOP(IR.MINUS, exp1, exp2)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="sub `d0,`s0,`s1\n",
			     src=[munchExp exp1, munchExp exp2], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.BINOP(IR.MUL, exp1, exp2)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="mult `d0,`s0,`s1\n",
			     src=[munchExp exp1, munchExp exp2], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.BINOP(IR.DIV, exp1, exp2)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="div `d0,`s0,`s1\n",
			     src=[munchExp exp1, munchExp exp2], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.BINOP(IR.AND, exp, IR.CONST c)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="andi `d0,`s0,"^ InttoString c ^"\n",
			     src=[munchExp exp], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.BINOP(IR.AND, IR.CONST c, exp)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="andi `d0,`s0,"^ InttoString c ^"\n",
			     src=[munchExp exp], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.BINOP(IR.AND, exp1, exp2)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="and `d0,`s0,`s1\n",
			     src=[munchExp exp1, munchExp exp2], dst=[t], jump=NONE});
		 t
	     end

	   | munchExp (IR.BINOP(IR.OR, exp, IR.CONST c)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="ori `d0,`s0,"^ InttoString c ^"\n",
			     src=[munchExp exp], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.BINOP(IR.OR, IR.CONST c, exp)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="ori `d0,`s0,"^ InttoString c ^"\n",
			     src=[munchExp exp], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.BINOP(IR.OR, exp1, exp2)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="or `d0,`s0,`s1\n",
			     src=[munchExp exp1, munchExp exp2], dst=[t], jump=NONE});
		 t
	     end
	   | munchExp (IR.BINOP(IR.XOR, exp1, exp2)) =
	     let
		 val t=Temp.newtemp()
	     in
		 emit(A.OPER{assem="sll `d0,`s0,`s1\n",
			     src=[munchExp exp1, munchExp exp2], dst=[t], jump=NONE});
		 t
	     end
		 (*
	   | munchExp (IR.CALL(IR.NAME(name), arguments)) =
	     let
		 val defs = Frame.RA::Frame.RV::(Frame.getRegisterTemps F.calleesaves)
	     in
		 emit(A.OPER{assem="jal "^ Temp.labname name^"\n",
			     sr=munchArgs(0, arguments, 16), dst=defs, jump=NONE}
		     );
		 Frame.RV
	     end*)
	   | munchExp (IR.ESEQ (stm,exp)) = (munchStm stm; munchExp exp)

	(* and munchArgs(i, [], offset) = []
           | munchArgs(i, a::l, offset) =
             let
                 val argTemp = if i < 4
                               then List.nth ((F.getRegisterTemps F.argregs), i)
                               else Temp.newtemp() (* not used *)
                 fun moveArgToTemp arg = munchStm(T.MOVE(T.TEMPLOC(argTemp), arg))
                 fun moveArgToFrame(arg, offset) =
                   munchStm(T.MOVE(T.MEMLOC(T.BINOP(T.PLUS, T.TEMP(F.SP), T.CONST offset)), arg))
             in
                 if i < 4
                 then (moveArgToTemp a; argTemp::munchArgs(i + 1, l, offset))
                 else (moveArgToFrame(a, offset); munchArgs(i + 1, l, offset + 4))
             end
		*)
      in (munchStmList stml) ;
         (rev ((A.OPER {assem ="", dst = nil,
                        src = MipsRegs.ReturnSink, jump = NONE}) ::
               (!ilist)))
      end
end
