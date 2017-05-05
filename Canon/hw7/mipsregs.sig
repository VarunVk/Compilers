signature MIPSREGS =
sig
   type temp

   (* number of argument register *) 
   val numargsregs : int

   val tempmap : (temp -> temp) -> temp -> string

   (* MIPS registers with specific names *)
   val Zero : temp    (* always zero *)
   val RVreg : temp   (* return value *)
   val SLreg : temp   (* static link if used *)
   val SPreg : temp   (* stack pointer *)
   val FPreg : temp   (* frame pointer *)
   val RAreg : temp   (* return address register *)
   
   val callersaveregs : temp array (* caller save regs in indexible form *)
   val calleesaveregs : temp array (* callee save regs in indexible form *)
   val argregs : temp array        (* argument regs in indexible form *)

   (* Registers potentially trashed by a procedure call *)
   val CallDefs : temp list

   (* All the MIPS registers, needed in coloring based reg assignment *)
   val AllRegs : temp list

   (* Registers that need to be recorded as live at end of procedure body *)
   val ReturnSink : temp list
end
