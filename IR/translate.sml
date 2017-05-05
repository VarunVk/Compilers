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
   val UnitLab = T.newlabel()

   fun initfrags () =
      fragments := [F.stringfrag UnitLab " "]@[F.stringfrag NilLab " "]

   (* a useful function for creating a sequence out of two statement forms *)
   fun seq [s1] =  s1
     | seq [s1,s2] = (IR.SEQ (s1,s2))
     | seq (s1 :: rs) = (IR.SEQ (s1,(seq rs)))
     | seq [] = (IR.EXP(IR.CONST 0))

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
  fun getResults () = rev (!fragments)

  (* some sample functions that might be written for translation *)
  val error = ErrorMsg.error

  fun traceSL Bottom Bottom  = (error 0 "Both at Bottom level";[] )
  | traceSL Bottom _  = (error 0 "Declared level reached Bottom";[])
    | traceSL _ Bottom  = (error 0 "Uselevel reached Bottom.";[])
    | traceSL (declevel as Other(decframe, _,uniqdec)) (uselevel as Other(frame, useparent,uniquse))  =
      if uniqdec = uniquse
      then []
      else [frame]@(traceSL declevel useparent)

  fun simpleVarIR ((declevel, fraccess), uselevel) =
    let
	val frames =traceSL declevel uselevel
  val a= F.mapvar frames fraccess
    in
	Ex(F.exp (fraccess, a))
    end

      fun callexpIR (Bottom, calllevel, label, args) = Ex (IR.CALL (IR.NAME label, map mkEx args))
        | callexpIR (declevel as Other(frame, parent, uniq), calllevel, label, args) =
          let
              val frames = traceSL parent calllevel
              val sl = F.mapslink frames
              val mkExArgs = map mkEx args
          in
              Ex (IR.CALL (IR.NAME label, sl :: mkExArgs))
          end

  fun binopIR (binop, left, right) = Ex(IR.BINOP(binop, mkEx(left), mkEx(right)))

  fun relopIR (relop, left, right) =
  (*  case relop of
	IR.EQ => Ex(F.externalCall "stringEqual" [mkEx left, mkEx right])
      | IR.LE => Ex(F.externalCall "stringLE" [mkEx left, mkEx right])
      | IR.LT => Ex(F.externalCall "stringLT" [mkEx left, mkEx right])
      | IR.GE => Ex(F.externalCall "stringGE" [mkEx left, mkEx right])
      | IR.GT => Ex(F.externalCall "stringGT" [mkEx left, mkEx right])
      | _ => *)Cx(fn (t, f) => IR.CJUMP(relop, mkEx(left), mkEx(right), t, f))


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
        val treeBody = mkEx body'
        val procedureBody = F.procBody levelFrame (mkEx body')
	      val y = F.procfrag levelFrame procedureBody
    in
        fragments := y::(!fragments)
    end

  fun forIR (varEx, escape, loEx, hiEx, bodyNx, breaklabel) =
    let
    val tHi= T.newtemp()
    val tVar= T.newtemp()
	val body = mkNx(bodyNx)
	val updatelabel = T.newlabel()
	val endlabel = T.newlabel()
	val bodylabel = T.newlabel()
    in
	Nx(seq[
        IR.MOVE (IR.TEMP(tVar), mkEx loEx),
        IR.MOVE (IR.TEMP(tHi), mkEx hiEx),
        IR.CJUMP(IR.LE, IR.TEMP(tVar), IR.TEMP(tHi), bodylabel, endlabel),

	       IR.LABEL(bodylabel),
         body,
         IR.CJUMP(IR.LT, IR.TEMP(tVar), IR.TEMP(tHi), updatelabel, endlabel),
         IR.LABEL(updatelabel),
         IR.MOVE(IR.TEMP(tVar), IR.BINOP(IR.PLUS, IR.TEMP(tVar), IR.CONST 1)),
         IR.JUMP(IR.NAME (bodylabel), [bodylabel]),
         IR.LABEL(endlabel)
         ]
         )
    end

  fun exp2loc (IR.MEM exp') = IR.MEMLOC exp'
    | exp2loc (IR.TEMP temp') = IR.TEMPLOC temp'
    | exp2loc (IR.ESEQ (stm', exp' as IR.MEM(_))) = IR.ESEQLOC(stm', exp')
    | exp2loc (IR.ESEQ (stm', exp' as IR.TEMP(_))) = IR.ESEQLOC(stm', exp')
    | exp2loc _ = (error 0 "Can't convert exp to loc"; IR.TEMPLOC(T.newtemp()))


  fun sequencingIR [] = Ex (IR.NAME NilLab)
    | sequencingIR [exp] =Ex (IR.ESEQ (mkNx exp, IR.CONST 0))
    | sequencingIR (head :: l) = Ex (IR.ESEQ (mkNx head, mkEx (sequencingIR l)))

  fun assignIR (left, right) = Nx (IR.MOVE (mkEx left, mkEx right))

  (* this may be called when translating an integer expression *)
  fun IntExp n = (Ex (IR.CONST n))

  fun stringIR(lit) =
    let
        fun checkFragLit(frag) =
          if F.StringFrag frag
          then String.compare(F.StringFragStr(frag), lit)=EQUAL
          else false
        fun genFragLabel() =
        let
          val x=(List.find checkFragLit (!fragList))
        in
          if isSome(x) andalso F.StringFrag (valOf x)
          then
            F.StringFragLabel (valOf x)
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

  fun subscriptIR (arrEx, indexEx, break) =
    let
        val tArrEx = T.newtemp()
        val tIndexEx = T.newtemp()
        val errUpLab = T.newlabel()
        val errLoLab = T.newlabel()
        val goodcaseLab = T.newlabel()
        val checkLowerLab = T.newlabel()
    in
      Ex(IR.MEM(
          IR.ESEQ(
            seq [
            IR.MOVE(IR.TEMP(tArrEx), mkEx arrEx),
            IR.MOVE(IR.TEMP(tIndexEx), mkEx indexEx),

          IR.CJUMP(IR.LE, IR.MEM(IR.TEMP(tArrEx)), IR.TEMP(tIndexEx) , errUpLab, checkLowerLab),
          IR.LABEL(errUpLab),
          mkNx (Ex (IR.CALL (( IR.NAME (T.namedlabel ("$UpperBoundExit$$"))), []))),
          IR.JUMP(IR.NAME break, [break]),

          IR.LABEL(checkLowerLab),
          IR.CJUMP(IR.LT, IR.TEMP(tIndexEx) , IR.CONST 0, errLoLab, goodcaseLab),
          IR.LABEL(errLoLab),
          mkNx (Ex (IR.CALL (( IR.NAME (T.namedlabel ("$LowerBoundExit$$"))), []))),
          IR.JUMP(IR.NAME break, [break]),

          IR.LABEL(goodcaseLab)
          ],
		      IR.BINOP(IR.PLUS, IR.TEMP(tArrEx),
                        IR.BINOP(IR.MUL, IR.CONST(F.WS),
                        IR.BINOP(IR.PLUS, IR.CONST 1, IR.TEMP(tIndexEx))))
          )))
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
          val _ = print "RecordIR()"
    in
        Ex(IR.ESEQ(
                seq(rev(instantiateFields(exps))),
                IR.TEMP(r)))
    end

  fun fieldIR (nameEx, elem) = (print "FieldIR()";
    Ex(IR.MEM(IR.BINOP(
                   IR.PLUS, mkEx nameEx,
                   IR.BINOP(IR.MUL, IR.CONST(elem), IR.CONST (F.WS))))))


  fun nilIR () = Ex (IR.NAME NilLab)

  fun intIR (n) = Ex (IR.CONST n)


  fun arrayIR (sizeEx, initEx, break) =
    let
	val tSize = T.newtemp()
	val tInit = T.newtemp()
	val LsizeError = T.newlabel()
	val Lalloc = T.newlabel()
    in
	Ex(
	    IR.ESEQ (
		seq [
        IR.MOVE(IR.TEMP(tSize), mkEx sizeEx),
        IR.MOVE(IR.TEMP(tInit), mkEx initEx),
        IR.CJUMP(IR.LT, IR.TEMP(tSize), IR.CONST 0, LsizeError, Lalloc),
		    IR.LABEL(LsizeError),
		    mkNx (Ex (IR.CALL ((IR.NAME (T.namedlabel("$SizeError$$"))), []))),
		    IR.JUMP(IR.NAME break, [break]),
        IR.LABEL(Lalloc)
		],
    (IR.CALL (( IR.NAME (T.namedlabel("$InitArray$$")), [IR.TEMP(tSize), IR.TEMP(tInit)])))
         ))
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

  fun LetBodyJoin (tyExp, exp) =
  let
    val _ = 5
    in
    Ex(IR.ESEQ(seq(rev (map mkNx tyExp)), mkEx exp))
  end
end
