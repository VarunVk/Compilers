functor MipsFrameFun(structure T: TEMP
                     structure Tree: TREE
                     structure MipsRegs : MIPSREGS
                        sharing type Tree.temp = T.temp = MipsRegs.temp
                            and type Tree.label = T.label) :> FRAME
               where type label = T.label
                 and type exp = Tree.exp
                 and type stm = Tree.stm
                 and type temp = T.temp =

struct

    type exp = Tree.exp
    type stm = Tree.stm
    type label = T.label
    type temp = T.temp

    datatype access = InFrame of int | InReg of T.temp

    (* formals here are used in compiling procedure body hence
       registers must be new, i.e. distinct from actual arg regs *)
    type frame = {
                   label: T.label,
                   epilogue : label,
                   numformals : int,
                   formals : access list,
                   offset: int ref,
                   fsize: int ref
                 }


    (* multiplier for words *)
    val FP = T.newtemp()
    val RV = T.newtemp()
    val RA = T.newtemp()
    val WS = 4

    (* names of some register needed in frame related activity *)
    val RVreg = MipsRegs.RVreg
    val FPreg = MipsRegs.FPreg

    val RetVal = (Tree.TEMP RVreg)

    datatype frag = PROC of {body:Tree.stm list,frame:frame}
                  | STRING of label * string

    fun stringfrag lab str = (STRING (lab,str))

    fun name ({label,...}:frame) = label

    fun procfrag fr stmtlst =
            (PROC {body=stmtlst,frame=fr})

    fun StringFrag (STRING _) = true
      | StringFrag (PROC _) = false

    fun StringFragLab (STRING (lab,_)) = (T.labname lab)
    fun StringFragLabel (STRING (lab,_)) = (lab)

    fun StringFragStr (STRING (_,str)) = str

    fun ProcFragLab (PROC {body,frame}) = (T.labname (name frame))

    fun ProcFragBody (PROC {body,frame}) = body

    fun newFrame {name:label, formals} =
      let val numargs = length formals
      in {
           label = name,
           epilogue = T.newlabel(),
           numformals = numargs,
           formals = let val i = ref (~numargs)
                         val lastargreg = !i + MipsRegs.numargsregs
                     in (map (fn true => (i := (!i) + 1;
                                          InFrame(WS * (!i)))

   	                       | false => (i := (!i) + 1;
                                           if (!i > lastargreg)
                                           then InFrame(WS *(!i))
                                           else InReg(T.newtemp())))
                              formals)
                     end,
           offset = (ref (~(WS * numargs))),
           fsize = (ref (~(WS * numargs)))
         }
      end

    fun formals ({formals=fmls,...}:frame) = fmls

    fun allocLocal ({offset,fsize,...}:frame) true =
                 (offset := !offset - WS; fsize := (!fsize) - WS;
                  InFrame(!offset))
      | allocLocal _ false = (InReg(T.newtemp()))

    fun exp (fraccess, frameaddr) =
      case fraccess of
	  InFrame offset => Tree.MEM(Tree.BINOP(Tree.PLUS, frameaddr, Tree.CONST offset))
       |  InReg temp => Tree.TEMP(temp)

    fun externalCall str exps =
        Tree.CALL(Tree.NAME(T.namedlabel str),exps)

  fun procBody _ exp = Tree.MOVE(RetVal,exp)

    fun procEpilogue ({epilogue=lab,...}:frame) = lab

    fun static_link ({numformals=nf,...}:frame) =
            Tree.CONST (~(WS * (nf - 1)))

        fun mapvar _ (InReg t) = Tree.TEMP t
          | mapvar frames (InFrame k) =
              let fun chainback nil e = Tree.MEM(Tree.BINOP(Tree.PLUS,
                                                           (Tree.CONST k),e))
                    | chainback (frame::frames) e =
                        (print "Fn Chaining ";(chainback frames
                                   (Tree.MEM(Tree.BINOP(Tree.PLUS,
                                                        static_link frame,
                                                        e)
                                            ))))
              in (chainback frames (Tree.TEMP FPreg))
              end

        fun mapslink frames =
              let fun chainback nil e = (print "Fn Nil Chaining ";e)
                    | chainback (frame::frames) e =
                         (print "Fn Chaining ";chainback frames (Tree.MEM(Tree.BINOP(Tree.PLUS,
                                                               static_link frame,
                                                               e
                                                              )
                                                   )
                                          ))
              in (chainback frames (Tree.TEMP FPreg))
              end


end
