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

   datatype level = Bottom 
              | Other of F.frame * level * unique 

   type access = level * F.access

   type label = T.label

   val outermost = Bottom

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

    (*
  fun simpleVarIR ((declevel, fraccess), uselevel) =
       Ex(IR.TEMP (fraccess, followSLs declevel uselevel (IR.TEMP F.FPreg)))
       *)

  (* this may be called when translating an integer expression *)  
  fun IntExp n = (Ex (IR.CONST n))

  (* This is something to use for translating and addition expression *)
  fun PlusExp exp1 exp2 =
      (Ex (IR.BINOP (IR.PLUS,(mkEx exp1),(mkEx exp2))))

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
   
