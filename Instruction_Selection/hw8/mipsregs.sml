functor MipsRegsFun(structure T: TEMP) :> MIPSREGS
                      where type temp = T.temp =

struct

    type temp = T.temp

    val sub = Array.sub

    (* number of argument registers *)
    val numargsregs = 4

    (* names for argument registers; use Array.sub(argregs,i) for ith reg *)
    val argregs = Array.fromList [T.newtemp(),T.newtemp(),
                                  T.newtemp(),T.newtemp()]

    (* caller save registers *)
    val callersaveregs = Array.fromList [T.newtemp(),T.newtemp(),
                                         T.newtemp(),T.newtemp(),
                                         T.newtemp(),T.newtemp(),
                                         T.newtemp(),T.newtemp(),
                                         T.newtemp(),T.newtemp()]

    (* callee save registers *)
    val calleesaveregs = Array.fromList [T.newtemp(),T.newtemp(),
                                         T.newtemp(),T.newtemp(),
                                         T.newtemp(),T.newtemp(),
                                         T.newtemp(),T.newtemp()]


    (* names for special MIPS registers *)
    val Zero = T.newtemp()         (* always 0 *)
    val RVreg = T.newtemp()        (* return value register *)
    val SLreg = T.newtemp()        (* static link, not used *)
    val FPreg = T.newtemp()        (* frame pointer *)
    val SPreg = T.newtemp()        (* stack pointer *)
    val RAreg = T.newtemp()        (* return address *)

    (* Registers potentially trashed by a procedure call *)
    val CallDefs = (sub(callersaveregs,0)) :: (sub(callersaveregs,1)) ::
                   (sub(callersaveregs,2)) :: (sub(callersaveregs,3)) ::
                   (sub(callersaveregs,4)) :: (sub(callersaveregs,5)) ::
                   (sub(callersaveregs,6)) :: (sub(callersaveregs,7)) ::
                   (sub(callersaveregs,8)) :: (sub(callersaveregs,9)) :: 
                   (sub(argregs,0)) :: (sub(argregs,1)) :: 
                   (sub(argregs,2)) :: (sub(argregs,3)) ::
                   RAreg :: RVreg :: SLreg :: nil

    (* registers live at the end of the procedure body *)
    val ReturnSink = RVreg :: SPreg :: FPreg :: nil

    (* all the registers *)
    val AllRegs =  (sub(calleesaveregs,0)) :: (sub(calleesaveregs,1)) ::
                   (sub(calleesaveregs,2)) :: (sub(calleesaveregs,3)) ::
                   (sub(calleesaveregs,4)) :: (sub(calleesaveregs,5)) ::
                   (sub(calleesaveregs,6)) :: (sub(calleesaveregs,7)) ::
                   (sub(callersaveregs,0)) :: (sub(callersaveregs,1)) ::
                   (sub(callersaveregs,2)) :: (sub(callersaveregs,3)) ::
                   (sub(callersaveregs,4)) :: (sub(callersaveregs,5)) ::
                   (sub(callersaveregs,6)) :: (sub(callersaveregs,7)) ::
                   (sub(callersaveregs,8)) :: (sub(callersaveregs,9)) :: 
                   (sub(argregs,0)) :: (sub(argregs,1)) :: 
                   (sub(argregs,2)) :: (sub(argregs,3)) ::
                   RAreg :: SLreg :: ReturnSink

    (* MIPS names for the registers; to be coordinated with AllRegs *)
    val RegNames = "$s0" :: "$s1" :: "$s2" :: "$s3" :: 
                   "$s4" :: "$s5" :: "$s6" :: "$s7" :: 
                   "$t0" :: "$t1" :: "$t2" :: "$t3" :: "$t4" :: 
                   "$t5" :: "$t6" :: "$t7" :: "$t8" :: "$t9" :: 
                   "$a0" :: "$a1" :: "$a2" :: "$a3" :: 
                   "$ra" :: "$v1" :: "$v0" :: "$sp" :: "$fp" :: nil

    (* The next two definitions are for providing a map from temps to names *)
    fun zip nil nil = nil
      | zip (h1::t1) (h2::t2) = (h1,h2) :: (zip t1 t2)

    val regmap : string T.Table.table = 
           T.Table.enter ((foldr (fn ((t,n),tbl) => 
                                        T.Table.enter (tbl,t,n))
                                 T.Table.empty
                                 (zip AllRegs RegNames)),
                          Zero,"$0")

    fun tempmap pm t = 
        let val what = (T.Table.look (regmap,(pm t)))
            fun tempname NONE t = T.makestring t
              | tempname (SOME s) _ = s
        in tempname what t
        end
end                  
