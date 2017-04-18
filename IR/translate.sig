signature TRANSLATE =
sig
   type level
   type access
   type label
   type exp     (* New. Represents the result of translation. Notice that 
                   this is the translation, not IR, form of exp as discussed
                   in class. See the definition in the functor definition *)

   val outermost : level
   val newLevel : {parent: level, name: label,
                   formals: bool list} -> level

   (* we assume that the next four functions are never called with 
      outermost frame as argument *)
   val name: level -> label
   val formals: level -> access list
   val parent: level -> level
   val allocLocal: level -> bool -> access

   val initfrags: unit -> unit  (* initializer for fragments *)

   (* these are needed to enable printing of results for testing *)
   type frag        
   val getResults: unit -> (frag list)

  (* some sample translation functions *)
   val IntExp: int -> exp
   val PlusExp: exp -> exp -> exp
   val EqExp: exp -> exp -> bool -> exp

   (*
   val simpleVarIR : access * level -> exp
   val binopIR : (*binop*) exp * exp * exp -> exp
   val relopIR : (*relop*) exp * exp * exp -> exp
   val ifIR : exp * exp * exp -> exp
   val assignIR : exp * exp -> exp
   val whileIR : exp * exp * label -> exp
   val breakIR : label -> exp
   val forIR : exp * bool ref * exp * exp * exp * label -> exp
   val arrayIR : exp * exp -> exp
   val subscriptIR : exp * exp -> exp
   val recordIR : exp list -> exp
   val fieldIR : exp * int -> exp
   val sequencingIR : exp list -> exp 
   val nilIR : unit -> exp
   val intIR : int -> exp
   val stringIR : string -> exp*)
end


