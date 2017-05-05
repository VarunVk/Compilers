signature FRAME =
sig
    (* these are needed because the frame has to determine how to wrap
       intermediate language code for a procedure to realize view shifts;
       right now this is done through procBody
    *)
    type exp 
    type stm
    type temp

    (* word size; may be needed in estimating space requests for arrays
       and records in translation 
    *)
    val FP: temp
    val WS: int  

    (* the stuff about frames from the homework 6 *)
    type frame
    type label
    type access
    val newFrame: {name: label, formals: bool list} -> frame
    val name: frame -> label
    val formals: frame -> access list
    val allocLocal: frame -> bool -> access

    (* this one takes the expression corresponding to the procedure 
       body and returns it as a statement with the appropriate 
       view shift wrapper
    *)
    val procBody : frame -> exp -> stm

    (* used to get to the label for the beginning of procedure epilogue part *)
    val procEpilogue : frame -> label

    (* treatment of external calls; this one belongs here because 
       how to interface with external code is a machine dependent issue *)
    val externalCall: string -> (exp list) -> exp

    (* Declarations for treating fragments, don't really believe this
       should be in the frame code but will stick to Appel's idea for now *)
    type frag
    val stringfrag: label -> string -> frag
    val procfrag: frame -> stm list -> frag
    val StringFrag: frag -> bool
    val StringFragLab: frag -> string
    val StringFragStr: frag -> string
    val ProcFragLab: frag -> string
    val ProcFragBody: frag -> stm list
    val exp : access * exp -> exp
    val StringFragLabel: frag -> label
end
