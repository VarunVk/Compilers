functor TranslateFun(structure F: FRAME
                     structure T: TEMP
                     structure IR: TREE
                     sharing type F.label = T.label = IR.label
                         and type T.temp = IR.temp
                         and type F.exp = IR.exp
                         and type F.stm = IR.stm) :> TRANSLATE
             where type label = T.label
               and type frag = F.frag =
struct

   type unique = unit ref
   type binop = IR.binop
   type relop = IR.relop

   datatype level = Bottom
              | Other of F.frame * level * unique

   type access = level * F.access

   type label = T.label

   val outermost = Bottom
   val fragList = ref [] : F.frag list ref

   fun initfrags () = !fragList

   fun newLevel {parent: level, name: label, formals: bool list} =
       (Other ((F.newFrame {name=name, formals=(true :: formals)}),
               parent, ref()))

   fun name (Other (frame,_,_)) = (F.name frame)

   fun parent (Other (_,parent,_)) = parent

   fun formals (level as (Other (frame,_,_))) =
       (map (fn x => (level,x)) (tl (F.formals frame)))

   fun allocLocal (level as (Other (frame,_,_))) esc =
       (level,(F.allocLocal frame esc))

   (* we collect the fragments created from the program into this *)
   val fragments:((F.frag list) ref) = ref nil

   (* The fragment list should initially contain at least a label
      and an associated location to use for nil; compare with this
      when trying to determine if you are dereferencing a null pointer
   *)

   val NilLab = T.newlabel()

   fun initfrags () =
      fragments := [F.stringfrag NilLab " "]

   (* a useful function for creating a sequence out of two statement forms *)
   fun seq [s1,s2] = (IR.SEQ (s1,s2))
     | seq (s1 :: rs) = (IR.SEQ (s1,(seq rs)))


   (* the translation form for expressions that was discussed in class *)
   datatype exp = Ex of IR.exp
                | Nx of IR.stm
                | Cx of T.label * T.label -> IR.stm

   (* a couple of the coercion functions discussed in class; you should
      add a definition for mkNx *)
   fun mkEx (Ex e) = e
     | mkEx (Cx genstm) =
         let val r = T.newtemp()
             val t = T.newlabel() and f = T.newlabel()
         in IR.ESEQ(seq [IR.MOVE(IR.TEMP r, IR.CONST 1),
                         genstm(t,f),
                         IR.LABEL f,
                         IR.MOVE(IR.TEMP r, IR.CONST 0),
                         IR.LABEL t],
                    IR.TEMP r)
         end
     | mkEx (Nx s) = IR.ESEQ(s,IR.CONST 0)

  fun mkCx (Ex (IR.CONST 0)) = (fn (x,y) => (IR.JUMP ((IR.NAME y),[y])))
    | mkCx (Ex (IR.CONST 1)) = (fn (x,y) => (IR.JUMP ((IR.NAME x),[x])))
    | mkCx (Ex e) = (fn (x,y) => IR.CJUMP(IR.EQ,(IR.CONST 0),e,y,x))
    | mkCx (Cx stm) = stm

  fun mkNx (Ex e) = IR.EXP(e)
    | mkNx (Nx n) = n
    | mkNx (c) = mkNx(Ex(mkEx(c)))


   (* These declarations are only needed to permit the printing
      of fragments for debugging/testing---see how these are used in
      parse.sml. When the compiler is completed, we can dispense with
      these.
   *)

  type frag = F.frag
  fun getResults () = (!fragments)

  (* some sample functions that might be written for translation *)
  val error = ErrorMsg.error

  fun followSLs Bottom Bottom bestguess = (error 0 "Following static links failed"; bestguess)
    | followSLs Bottom _ bestguess = (error 0 "Following static links failed"; bestguess)
    | followSLs _ Bottom bestguess = (error 0 "Following static links failed"; bestguess)
    | followSLs (declevel as Other(_, _,uniqdec)) (uselevel as Other(_, useparent,uniquse)) bestguess =
      if uniqdec = uniquse
      then bestguess
      else followSLs declevel useparent (IR.MEM bestguess)

  fun simpleVarIR ((declevel, fraccess), uselevel) =
    let
	val a= (IR.TEMP (T.newtemp()))
	val b =followSLs declevel uselevel a
    in
	Ex(F.exp (fraccess, b))
    end

  fun binopIR (binop, left, right) = Ex(IR.BINOP(binop, mkEx(left), mkEx(right)))

  fun relopIR (relop, left, right) =
    case relop of
	IR.EQ => Ex(F.externalCall "stringEqual" [mkEx left, mkEx right])
      | IR.LE => Ex(F.externalCall "stringLE" [mkEx left, mkEx right])
      | IR.LT => Ex(F.externalCall "stringLT" [mkEx left, mkEx right])
      | IR.GE => Ex(F.externalCall "stringGE" [mkEx left, mkEx right])
      | IR.GT => Ex(F.externalCall "stringGT" [mkEx left, mkEx right])
      | _ => Cx(fn (t, f) => IR.CJUMP(relop, mkEx(left), mkEx(right), t, f))


  fun PlusIR (exp1, exp2) = binopIR(IR.PLUS, exp1, exp2)
  fun MinusIR (exp1, exp2) = binopIR(IR.MINUS , exp1, exp2)
  fun TimesIR (exp1, exp2) = binopIR(IR.MUL , exp1, exp2)
  fun DivideIR (exp1, exp2) = binopIR(IR.DIV , exp1, exp2)

  fun EqIR (exp1, exp2) = relopIR(IR.EQ, exp1, exp2)
  fun NeqIR (exp1, exp2) = relopIR(IR.EQ, exp1, exp2)
  fun LtIR (exp1, exp2) = relopIR(IR.EQ, exp1, exp2)
  fun LeIR (exp1, exp2) = relopIR(IR.EQ, exp1, exp2)
  fun GtIR (exp1, exp2) = relopIR(IR.EQ, exp1, exp2)
  fun GeIR (exp1, exp2) = relopIR(IR.EQ, exp1, exp2)

  fun procEntryExit({level=level', body=body'}) =
    let
        val levelFrame =
            case level' of
                Bottom => (error 0 "Fundec should not happen in outermost";
                           F.newFrame {name=T.newlabel(), formals=[]})
	      | Other((frame', _, _)) => frame'
        val treeBody = mkNx body'
	val y = F.procfrag levelFrame treeBody
    in
        fragments := y::(!fragments)
    end

  fun forIR (varEx, escape, loEx, hiEx, bodyNx, breaklabel) =
    let
	val var = mkEx(varEx)
	val lo = mkEx(loEx)
	val hi = mkEx(hiEx)
	val body = mkNx(bodyNx)
	val testlabel = T.newlabel()
	val bodylabel = T.newlabel()
    in
	Nx(seq[IR.MOVE(var, lo),
         IR.JUMP(IR.NAME (testlabel), [testlabel]),
	       IR.CJUMP(IR.LE, var, hi, bodylabel, breaklabel),
	       IR.LABEL(bodylabel),
         IR.MOVE(var, IR.BINOP(IR.PLUS, var, IR.CONST 1)),
	       body,
         IR.LABEL(testlabel),
	       IR.CJUMP(IR.LE, var, hi, bodylabel, breaklabel),
	       IR.LABEL(breaklabel)])
    end

  fun exp2loc (IR.MEM exp') = IR.MEMLOC exp'
    | exp2loc (IR.TEMP temp') = IR.TEMPLOC temp'
    | exp2loc (IR.ESEQ (stm', exp' as IR.MEM(_))) = IR.ESEQLOC(stm', exp')
    | exp2loc (IR.ESEQ (stm', exp' as IR.TEMP(_))) = IR.ESEQLOC(stm', exp')
    | exp2loc _ = (error 0 "Can't convert exp to loc"; IR.TEMPLOC(T.newtemp()))


  fun sequencingIR [] = Ex (IR.CONST 0)
    | sequencingIR [exp] = exp
    | sequencingIR (head :: l) = Ex (IR.ESEQ (mkNx head, mkEx (sequencingIR l)))

  fun assignIR (left, right) = Nx (IR.MOVE (mkEx left, mkEx right))

  (* this may be called when translating an integer expression *)
  fun IntExp n = (Ex (IR.CONST n))

  fun stringIR(lit) =(*Ex(IR.NAME (T.newlabel()))*)
    let
        fun checkFragLit(frag) =
          if F.StringFrag frag
          then String.compare(F.StringFragStr(frag), lit)=EQUAL
          else false
        fun genFragLabel() =
        let
          val x=valOf (List.find checkFragLit (!fragList))
        in
          if F.StringFrag (x)
          then
            F.StringFragLabel (x)
          else
              let
                  val lab' = T.newlabel()
              in
                  fragList := F.stringfrag lab' lit::(!fragList);
                  F.stringfrag lab' lit ::(!fragList);
                  lab'
              end
        end
        val lab = genFragLabel()
    in
        Ex(IR.NAME(lab))
    end

  (* This is something to use for translating and addition expression *)

  fun whileIR (test, body, breaklabel) =
    let
        val testlabel = T.newlabel()
        val bodylabel = T.newlabel()
        val test = mkCx test
        val body = mkNx body
    in
        Nx(seq [IR.LABEL testlabel,
                test (bodylabel, breaklabel),
                IR.LABEL(bodylabel),
                body,
                IR.JUMP (IR.NAME testlabel, [testlabel]),
                IR.LABEL breaklabel])
    end

  fun breakIR breaklabel = Nx(IR.JUMP (IR.NAME breaklabel, [breaklabel]))

  fun ifIR (test, then', else') =
    let
        val genstm = mkCx(test)
        val e2 = mkEx(then')
        val e3 = mkEx(else')
        val resulttemp = T.newtemp()
        val t = T.newlabel()
        val f = T.newlabel()
        val join = T.newlabel()
    in
        Ex(IR.ESEQ(seq [
			genstm(t, f),
			IR.LABEL(t), IR.MOVE(IR.TEMP(resulttemp), e2), IR.JUMP(IR.NAME(join), [join]),
			IR.LABEL(f), IR.MOVE(IR.TEMP(resulttemp), e3), IR.JUMP(IR.NAME(join), [join])
		    ], IR.TEMP(resulttemp)))
    end

  fun subscriptIR (arrEx, indexEx) =
    let
        val addr = T.newtemp()
        val size = T.newtemp()
        val arr = mkEx arrEx
        val index = mkEx indexEx
        val errUpLab = T.newlabel()
        val errLoLab = T.newlabel()
        val goodcaseLab = T.newlabel()
        val endLabel = T.newlabel()
    in
        (*Add lower and upper bound check . Access the array from arr+F.WS*)
        Ex(
          IR.ESEQ(
            seq [
          IR.MOVE(IR.TEMP(size), IR.MEM(arr)),

          IR.CJUMP(IR.LE, IR.TEMP(size), index , errUpLab, goodcaseLab),
          IR.LABEL(errUpLab),
          mkNx (Ex (IR.CALL (( IR.NAME (T.namedlabel ("$UpperBoundExit$$"))), []))),
          IR.JUMP(IR.NAME endLabel, [endLabel]),

          IR.CJUMP(IR.LE, index , IR.CONST 0, errLoLab, goodcaseLab),
          IR.LABEL(errLoLab),
          mkNx (Ex (IR.CALL (( IR.NAME (T.namedlabel ("$LowerBoundExit$$"))), []))),
          IR.JUMP(IR.NAME endLabel, [endLabel]),

          IR.LABEL(goodcaseLab),
		      IR.MOVE(IR.TEMP(addr),
			           IR.BINOP(IR.PLUS, arr,
                        IR.BINOP(IR.MUL, index, IR.CONST(F.WS)))),
          IR.LABEL(endLabel)],
		IR.MEM(IR.TEMP(addr))))
    end

  fun recordIR (exps) =
    let
        val n = length exps
        val r = T.newtemp()
        val recordInit = IR.MOVE(IR.TEMP(r), F.externalCall "initRecord" [IR.CONST n])
        fun setField (exp, elem) = IR.MOVE((IR.MEM(
                                                 IR.BINOP(IR.PLUS, IR.TEMP(r), IR.CONST(F.WS * elem)))),
                                           mkEx exp)
        fun instantiateFields ([]) = [recordInit]
          | instantiateFields (head :: l) = (setField(head, length l)) :: (instantiateFields (l))
    in
        Ex(IR.ESEQ(
                seq(rev(instantiateFields(exps))),
                IR.TEMP(r)))
    end

  fun fieldIR (nameEx, elem) =
    Ex(IR.MEM(IR.BINOP(
                   IR.PLUS, mkEx nameEx,
                   IR.BINOP(IR.MUL, IR.CONST(elem), IR.CONST (F.WS)))))


  fun nilIR () = Ex (IR.CONST 0)

  fun intIR (n) = Ex (IR.CONST n)

  fun callexpIR (Bottom, calllevel, label, args) = Ex (IR.TEMP (T.newtemp()))(*Ex (IR.TEMP(F.FP))*)
    | callexpIR (declevel as Other(frame, parent, uniq), calllevel, label, args) =
      let
          (*val fp1 = IR.TEMP (F.FP)*)
          val fp1=(IR.TEMP (T.newtemp()))
          val sl = followSLs parent calllevel fp1
          val mkExArgs = map mkEx args
      in
          Ex (IR.CALL (IR.NAME label, sl :: mkExArgs))
      end

  fun arrayIR (sizeEx, initEx) =
    let
  val res = T.newtemp()
  val address = T.newtemp()
	val size = mkEx sizeEx
	val init = mkEx initEx
	val LsizeError = T.newlabel()
	val Lalloc = T.newlabel()
	val Lexit = T.newlabel()
    in
	  Ex(
      IR.ESEQ (
      seq [
          IR.CJUMP(IR.LT, size, IR.CONST 0, LsizeError, Lalloc),
          IR.LABEL(LsizeError),
          mkNx (Ex (IR.CALL ((IR.NAME (T.namedlabel("$SizeError$$"))), []))),
          IR.JUMP(IR.NAME Lexit, [Lexit]),

	        IR.LABEL(Lalloc),
          IR.MOVE(IR.TEMP (address), (IR.CALL((IR.NAME (T.namedlabel("$InitArray$$"))), [size, init]))),
          IR.MOVE(IR.TEMP (address), F.externalCall "$InitArray$$" [size, init]),
          IR.MOVE(IR.MEM (IR.TEMP address), size),

          IR.LABEL(Lexit)
          ]
        ,IR.TEMP res))
    end




  (* here is something to use when translating equality checking;
     the str parameter indicates whether for string or for integers.
     understand how the Cx form is generated here *)
  fun EqExp exp1 exp2 str =
    if str
    then (Cx (fn (t,f) => IR.CJUMP(IR.EQ,(F.externalCall "$StringEqual$$"
							 [(mkEx exp1),(mkEx exp2)]),
                                   (IR.CONST 0),f,t)))
    else (Cx (fn (t,f) => (IR.CJUMP(IR.EQ,(mkEx exp1),(mkEx exp2),t,f))))

end
