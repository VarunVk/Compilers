signature SEMANT =
sig
  type exp
  type transty
  val transProg: exp -> unit
end


functor SemantFun(structure A: ABSYN
                  structure E: ENV
                  structure Symbol: SYMBOL
                  structure Tr: TRANSLATE
                  structure T: TEMP
                    sharing type A.symbol = E.symbol = Symbol.symbol
                        and type E.table = Symbol.table
                        and type T.label = Tr.label = E.label
                        and type E.access = Tr.access
                        and type E.level = Tr.level) :> SEMANT
             where type exp = A.exp =
struct

type  exp = A.exp
type  transty = Tr.exp * E.ty
val error = ErrorMsg.error
structure S=Symbol
structure Env=E

type venv = E.fnvar Symbol.table
type tenv = E.ty Symbol.table

fun IfSameType (tyA, tyB, pos, errMsg) = if E.eq(tyA, tyB)
					      then ()
					      else error pos errMsg
fun checkIfInteger ({exp= _, ty= E.INT}, pos) = ()
  | checkIfInteger ({exp= _, ty= _ }, pos) = error pos "Error : Integer required "

fun compParam ({exp=_, ty=E.INT},
	       {exp=_, ty=E.INT}, pos) = ()
  | compParam ({exp=_, ty=_},
	       {exp=_, ty=E.INT}, pos) = (error pos "Error: left argument must be of int type identical to right")
  | compParam ({exp=_, ty=E.INT},
	       {exp=_, ty=_}, pos) = (error pos "Error: right argument must be of int type identical to left")
  | compParam ({exp=_, ty=E.STRING},
	       {exp=_, ty=E.STRING}, pos) = ()
  | compParam ({exp=_, ty=_},
	       {exp=_, ty=E.STRING}, pos) = (error pos "Error: left argument must be of string type identical to right")
  | compParam ({exp=_, ty=E.STRING},
	       {exp=_, ty=_}, pos) = (error pos "Error: right argument must be of string type identical to left")
  | compParam ({exp=_, ty=E.NIL},
	       {exp=_, ty=E.RECORD(_, _)}, pos) = ()
  | compParam ({exp=_, ty=E.RECORD(_, _)},
	       {exp=_, ty=E.NIL}, pos) = ()
  | compParam ({exp=_, ty=E.RECORD(_, ref1)},
	       {exp=_, ty=E.RECORD(_, ref2)}, pos) =
    if ref1 = ref2 then () else error pos "Error: right argument must be of record type identical to left"
  | compParam ({exp=_, ty=_},
          {exp=_, ty=E.RECORD(_,_)}, pos) = (error pos "Error: left argument must be of record type identical to right")
  | compParam ({exp=_, ty=E.RECORD(_,_)},
          {exp=_, ty=_}, pos) = (error pos "Error: right argument must be of record type identical to left")
  | compParam ({exp=_, ty=E.ARRAY(_, ref1)},
	       {exp=_, ty=E.ARRAY(_, ref2)}, pos) =
    if ref1 = ref2 then () else error pos "Error: right argument must be of array type identical to left"
    | compParam ({exp=_, ty=E.ERROR},
  	       {exp=_, ty=E.NIL}, pos) =
      error pos "Error : ERROR <> NIL"
    | compParam ({exp=_, ty=_},
	       {exp=_, ty=_}, pos) =
    error pos "Error : Comparison Err. Non matching int, string, record, or array types"

  (* Only for the comparison operators *)
fun compRelOper ({exp=_, ty=E.INT}, {exp=_, ty=E.INT}, pos) = ()
  | compRelOper ({exp=_, ty=E.STRING}, {exp=_, ty=E.STRING}, pos) = ()
  | compRelOper ({exp=_, ty=E.INT}, {exp=_, ty=_ }, pos) = error pos "Error: right argument must be of integer type"
  | compRelOper ({exp=_, ty=_ }, {exp=_, ty=E.INT}, pos) = error pos "Error: left argument must be of integer type"
  | compRelOper ({exp=_, ty=E.STRING}, {exp=_, ty=_ }, pos) = error pos "Error: right argument must be of string type"
  | compRelOper ({exp=_, ty=_}, {exp=_, ty=E.STRING }, pos) = error pos "Error: left argument must be of string type"
  | compRelOper ({exp=_, ty=_}, {exp=_, ty=_}, pos) = error pos "Error: Incomparable"



fun checkTypesAssignable (var, value, pos, errMsg) =
  if E.comp(var, value) = E.EQ orelse E.comp(var, value) = E.GT
  then ()
  else error pos errMsg

val depth : int ref = ref 0
fun InLoop (pos, errorMsg) =
  if !depth = 0
  then error pos errorMsg
  else ()

fun incDepth () = depth := !depth+1
fun decDepth () = depth := !depth-1


(* Main recursive type-checking functions *)
fun transExp (venv, tenv, exp, level: Tr.level, break) =
  let fun
      trexp (A.VarExp(var)) = trvar var
      | trexp (A.IntExp(intvalue)) = {exp= Tr.intIR(intvalue), ty=E.INT}
      | trexp (A.StringExp(stringvalue, pos)) = (print ("Its a string "^stringvalue^"\n" ); {exp=(Tr.stringIR(stringvalue)), ty=E.STRING})
      | trexp (A.NilExp) = {exp= (Tr.nilIR()), ty=E.NIL}
      | trexp (A.CallExp({func, args, pos})) =
	let
	    (* recursively loop on all parameters to check if they are of the same type and number*)
	    fun checkArgs (forTy::formalList, argExp::argList, pos) = if E.eq(forTy, #ty (trexp argExp))
								      then checkArgs(formalList, argList, pos)
								      else error pos "error : Wrong type of parameter."
	      | checkArgs ([], argExp::argList, pos) = error pos "Error: functiong applied to too many arguments"
	      | checkArgs (forTy::formalList, [], pos) = error pos "Error: functiong applied to too few arguments"
	      | checkArgs ([], [], pos) = ()
	    fun makearglist (a, b) = (#exp (trexp a))::b
	    val argExpList = foldr makearglist [] args

	in
	    case S.look(venv, func) of
		SOME(Env.FunEntry({level=declevel, label, formals, result}))
		=> (checkArgs(formals, args, pos);
		    {exp=(Tr.callexpIR(declevel, level, label, argExpList)), ty=result})
	     |  SOME(_) => (error pos ("symbol not function " ^ S.name func);
			    {exp=(Tr.nilIR()), ty=E.ERROR})
	     |  NONE => (error pos ("Error: undefined function " ^ S.name func);
			 {exp=(Tr.nilIR()), ty=E.ERROR})
	end

      | trexp (A.OpExp{left, oper, right, pos}) =
	(case oper of
	     A.PlusOp => (checkIfInteger(trexp left, pos);
			  checkIfInteger(trexp right, pos);
			  {exp=(Tr.PlusIR(#exp (trexp left), #exp (trexp right) ) ), ty=E.INT})
	  |  A.MinusOp => (checkIfInteger(trexp left, pos);
			   checkIfInteger(trexp right, pos);
			   {exp=(Tr.MinusIR(#exp (trexp left), #exp (trexp right))), ty=E.INT})
	  |  A.TimesOp => (checkIfInteger(trexp left, pos);
			   checkIfInteger(trexp right, pos);
			   {exp=(Tr.TimesIR( #exp (trexp left), #exp (trexp right))), ty=E.INT})
	  |  A.DivideOp => (checkIfInteger(trexp left, pos);
			    checkIfInteger(trexp right, pos);
			    {exp=(Tr.DivideIR(#exp (trexp left), #exp (trexp right))), ty=E.INT})
	  |  A.EqOp => (compParam(trexp left, trexp right, pos);
			let
			    val {exp=exp', ty=ty'} = trexp left
			in
			    {exp= Tr.EqIR(exp', #exp (trexp right)), ty=E.INT}
			end
		       )
	  |  A.NeqOp => (compParam(trexp left, trexp right, pos);
                         let
			     val {exp=exp', ty=ty'} = trexp left
			 in
			     {exp= Tr.NeqIR(exp', #exp (trexp right)), ty=E.INT}
			 end
			)
	  |  A.LtOp => (compRelOper(trexp left, trexp right, pos);
		        let
			    val {exp=exp', ty=ty'} = trexp left
			in
			    {exp= Tr.LtIR(exp', #exp (trexp right)), ty=E.INT}
			end
		       )
	  |  A.LeOp => (compRelOper(trexp left, trexp right, pos);
		        let
			    val {exp=exp', ty=ty'} = trexp left
			in
			    {exp= Tr.LeIR(exp', #exp (trexp right)), ty=E.INT}
			end
		       )
	  |  A.GtOp => (compRelOper(trexp left, trexp right, pos);
		        let
			    val {exp=exp', ty=ty'} = trexp left
			in
			    {exp= Tr.GtIR(exp', #exp (trexp right)), ty=E.INT}
			end
		       )
	  |  A.GeOp => (compRelOper(trexp left, trexp right, pos);
			let
		            val {exp=exp', ty=ty'} = trexp left
		        in
		            {exp= Tr.GeIR(exp', #exp (trexp right)), ty=E.INT}
		        end
		       )
	)
      | trexp (A.RecordExp({fields, typ, pos})) =
        (case S.look(tenv, typ) of
             SOME x =>
             (case x of
                  E.RECORD(f, _) =>
                  let
		      val recFormal : (S.symbol * E.ty) list = f
                      fun getFieldType (name: string, []) = E.ERROR
                        | getFieldType (name: string, (sym, exp, pos)::l) = (
                           if String.compare (name, S.name sym) = EQUAL
                           then (print ("Found "^ (S.name sym)^"\n");(#ty (trexp exp)))
                           else getFieldType(name, l))
                      fun checkFormal (sym, ty) = (getFieldType(S.name sym, fields);())
                      (*if not (E.leq(getFieldType(S.name sym, fields), ty))
                        then error pos ("actual type doesn't match formal type: " ^ S.name sym)
                        else ()*)
                      fun iterator((fieldname, typeid), ()) =
                        (print (S.name fieldname^": checking for this field.\n"); checkFormal (fieldname, x);print "Done.\n"; ())

                        (*case S.look(tenv, typeid) typeid of
                            SOME x => (checkFormal (fieldname, x); ())
                          | NONE => (error pos ("unknown type in record: " ^ S.name typ); ()))*)
                  in
                      if List.length(recFormal) <> List.length(fields)
                      then (error pos ("record list is wrong length: " ^ S.name typ);
			    {exp=(Tr.nilIR()), ty=x})
                      else (foldr iterator () recFormal;
			    print "Record Exp\n";
			    {exp=(Tr.recordIR(map #exp (map trexp (map #2 fields)))), ty=x})
                  end
                | _ => (error pos ("error : expected record type, not: " ^ S.name typ); {exp=(Tr.nilIR()), ty=E.NIL})
             )
           | NONE => (error pos ("Error: undefined type symbol " ^ S.name typ); {exp=(Tr.nilIR()), ty=E.NIL})
        )
      | trexp (A.SeqExp(expList)) =
	let
	    fun helper((seqExp, pos), {stmlist=stmlist', ty=ty'}) =
	      let
		  val {exp=exp'', ty=ty''} = trexp seqExp
	      in
		  {stmlist=stmlist'@[(exp'')], ty=ty''}
	      end
	    fun checkSequence sequence = foldl helper {stmlist=[], ty=E.UNIT} sequence
        in
            {exp=(Tr.sequencingIR(#stmlist (checkSequence expList))), ty= (#ty (checkSequence expList))}
	end
      | trexp (A.AssignExp({var, exp, pos})) =
	let
	    fun getVarSymbol var' =
	      case var' of
		  A.SimpleVar(sym, _) => S.look(venv, sym)
	       |  A.FieldVar(var', _, _) => getVarSymbol var'
	       |  A.SubscriptVar(var', _, _) => getVarSymbol var'
	    fun canAssign var' =
	      case getVarSymbol var' of
		  SOME(Env.VarEntry({access:Tr.access, ty:E.ty, read_only:bool})) =>
		  if read_only
		  then error pos "Error: illegal assignment to loop control variable"
		  else ()
	       |  _ => error pos "Error cannot assign to a function"
	in
	    canAssign var;
	    checkTypesAssignable(#ty (trvar var),
				 #ty (trexp exp),
				 pos, "error : Wrong type used for assignment");
	    {exp=(Tr.assignIR(#exp (trvar var),#exp (trexp exp))), ty=E.UNIT}
	end
      | trexp (A.IfExp({test, then', else', pos})) =
	(
	  if E.eq(#ty (trexp test), E.INT)
	  then (
	      case else' of
		  SOME(elseExp) =>
		  (
		    case (#ty (trexp then'), #ty (trexp elseExp)) of
			(E.RECORD(_), NIL) => ({exp=Tr.ifIR(#exp (trexp test), #exp (trexp then'), #exp (trexp elseExp)), ty=(#ty (trexp then'))})
		     |  (NIL, E.RECORD(_)) => ({exp=Tr.ifIR(#exp (trexp test), #exp (trexp then'), #exp (trexp elseExp)), ty=(#ty (trexp elseExp))})
		     |  (tyA, tyB) => if E.eq(tyA, tyB)
				      then      ({exp=Tr.ifIR(#exp (trexp test), #exp (trexp then'), #exp (trexp elseExp)), ty=(#ty (trexp then'))})
				      else  (error pos "Error: types of conditional branches do not match";
                  {exp=Tr.ifIR(#exp (trexp test), #exp (trexp then'), #exp (trexp elseExp)), ty=(E.ERROR)})
		  )
	       |  NONE => if E.eq (#ty (trexp then'), E.UNIT)
			  then({exp=Tr.nilIR(), ty=E.UNIT})
			  else  (error pos "Error: types of conditional branches do not match"; {exp=(Tr.nilIR()), ty=(E.ERROR)})
	  )
	  else
	      (error pos "Condition in if is not INTEGER!"; {exp=(Tr.nilIR()), ty=(E.ERROR)})
	)
      | trexp (A.WhileExp({test, body, pos})) =
	(
	  if E.eq(#ty (trexp test), E.INT)
	  then
	      (
		let
		    val breakpoint = T.newlabel();
		in
		    depth := !depth + 1;

		    if E.eq (#ty (trexp body), E.UNIT)
		    then
			(
			  depth := !depth - 1;
			  {exp=(Tr.whileIR(#exp (transExp(venv, tenv, test, level, break)),
					   #exp (transExp(venv, tenv, body, level, breakpoint)), breakpoint)), ty=E.UNIT}
			)
		    else
			(error pos "Error: unit type expected for body of while" ; {exp=Tr.nilIR(), ty=(E.UNIT)})
		end
	      )
	  else (error pos "test does not evaluate to an int"; {exp=Tr.nilIR(), ty=(E.ERROR)})
	)
      | trexp (A.ForExp({var, escape, lo, hi, body, pos})) =
	let
	    val venv' = S.enter(venv, var, Env.VarEntry({access= Tr.allocLocal level true, ty=E.INT, read_only=true}))
	    val breakpoint = T.newlabel()
	    val _= incDepth ()
	    val {exp=bodyexp, ty=bodytype} = (transExp(venv', tenv, body, level, breakpoint))
	    val _= decDepth()
	in
	    if E.eq (#ty (trexp lo), E.INT)
	    then
		(
		  if E.eq(#ty (trexp hi), E.INT)
		  then
		      (
			if E.eq(bodytype, E.UNIT)
			then
			    (
			      case S.look(venv', var) of
				  SOME x =>
				  (case x of
				       Env.VarEntry{access, ty, read_only} =>
				       {exp=Tr.forIR(Tr.simpleVarIR(access, level),escape, #exp (trexp lo), #exp (trexp hi), bodyexp,
						     breakpoint), ty=E.UNIT}
				     | _ => (error 0 "Compiler bug: ForExp var isn't VarEntry"; {exp=Tr.nilIR(), ty=E.UNIT})
				  )
				| _ => (error 0 "couldnt find forexp var"; {exp=Tr.nilIR(), ty=E.UNIT})
			    )
			else
			    (error pos "for body must be no value"; {exp=Tr.nilIR(), ty=E.ERROR})
		      )
		  else
		      (error pos "Error: high value of for must be of integer type"; {exp=Tr.nilIR(), ty=E.ERROR})
		)
            else
		(error pos "Error: low value of for must be of integer type"; {exp=Tr.nilIR(), ty=E.ERROR})
	end
      | trexp (A.BreakExp(pos)) =
	(
	  InLoop(pos, "Invalid break: Not in a Loop");
	  {exp=Tr.breakIR(break), ty=E.UNIT}
	)
      | trexp (A.LetExp({decs, body, pos})) =
	let
	    val curDepth = !depth
	    val _ =  depth := 0
	    val {venv=venv', tenv=tenv', expList=decExp} = transDec(venv, tenv, decs, level, break)
	    val _ = print "Done processing Let decs.\n"
	    val _ =  depth := curDepth
	    val {exp= bodyExp, ty=bodyty } = transExp(venv', tenv', body, level, break)
	    val finalExp = Tr.LetBodyJoin (decExp, bodyExp)
	in
	     {exp= finalExp, ty=bodyty}
	end
      | trexp (A.ArrayExp({typ, size, init, pos})) =
	let
	    fun getType(SOME(ty)) = ty
	      | getType(NONE) = E.ERROR
	    fun actualTy ty =
	      case ty of
		  E.NAME(name, tyRef) => actualTy(getType(S.look(tenv, name)))
	       |  someTy => someTy
	in
	    (
	      case S.look(tenv, typ) of
		  SOME(x) =>
		  (
		    case actualTy x of
			E.ARRAY(ty, unique) =>
			(
			  let
			      fun getType(SOME(ty)) = ty
				| getType(NONE) = E.ERROR
			      fun actualTy ty =
				case ty of
				    E.NAME(name, tyRef) => actualTy(getType(S.look(tenv, name)))
				 |  someTy => someTy
			  in
			      checkIfInteger(trexp size, pos);
			      if E.eq(#ty (trexp init), actualTy ty)
			      then  (print "Allocating memory for an array, calling arrayIR() \n";
				     {exp=Tr.arrayIR(#exp (trexp size), #exp (trexp init), break),
				      ty=E.ARRAY(ty, unique)})
			      else
				  (error pos  "Error: integer expression expected in array initialization";
				   {exp=(Tr.nilIR()), ty=E.ERROR})
			  end
			)
		     |  _ => (error pos "Not of ARRAY type in array creation"; {exp=(Tr.nilIR()), ty=E.ERROR})
		  )
	       |  NONE => (error pos "No such type"; {exp=(Tr.nilIR()), ty=E.ERROR})
	    )
	end
      and trvar (A.SimpleVar(id, pos)) =
	  (case S.look(venv, id) of
	       SOME(Env.VarEntry({access, ty, read_only=_})) => {exp=Tr.simpleVarIR(access, level), ty=ty}
	    (*|  SOME(Env.FunEntry({level, label, formals, result})) => {exp=(), ty=result} *)
	    |  NONE => (error pos ("error: undeclared variable " ^ S.name id); {exp=(Tr.nilIR()), ty=E.ERROR})
	  )
	| trvar (A.FieldVar(v, id, pos)) =
	  (case trvar v of
	       {exp=_, ty=E.RECORD(recGen, unique)} =>
	       let
		   val fields = recGen
		   fun getFieldType ((fSymbol, fTy)::l, id, pos) =
		     if
			 String.compare(S.name fSymbol, S.name id) = EQUAL
		     then
			 (print ("###:Found the fieldtype  : "^S.name id);fTy)
		     else
			 (print ("###"^S.name fSymbol ^ "!= "^S.name id);getFieldType(l, id, pos))

		     | getFieldType ([], id, pos) =
		       (error pos ("Error: no record field with name " ^ S.name id);
			E.ERROR)
		   fun getIndex ([], id, cur) = cur
		     | getIndex (h :: t, id, cur) = if h = id then cur else getIndex(t, id, cur + 1)
	       in
		   {exp=Tr.fieldIR(#exp (trvar v),
				   getIndex(map #1 fields, id, 0)),
		    ty=getFieldType(recGen, id, pos)}
	       end
	     | {exp=_, ty=E.ERROR} => (error pos ("### Error: non-record type used in record selection but its an INT");
				 {exp=(Tr.nilIR()), ty=E.ERROR})
	     | {exp=_, ty=E.ARRAY(_,_)} => (error pos ("### Error: non-record type used in record selection but its an Array");
				 {exp=(Tr.nilIR()), ty=E.ERROR})
	     | {exp=_, ty=_} => (error pos ("### Error: non-record type used in record selection");
				 {exp=(Tr.nilIR()), ty=E.ERROR})
	  )
	| trvar (A.SubscriptVar(v, subExp, pos)) =
	  let
	      fun getType(SOME(ty)) = ty
		| getType(NONE) = E.ERROR
	      fun actualTy ty =
		case ty of
		    E.NAME(name, tyRef) => actualTy(getType(S.look(tenv, name)))
		 |  someTy => someTy
	  in
	      (case trvar v of
		   {exp=_, ty=E.ARRAY(arrTy, unique)} => (checkIfInteger(trexp subExp, pos);
							  {exp=Tr.subscriptIR(#exp (trvar v),
									      #exp (trexp subExp), break),
							   ty=actualTy arrTy})
		|  {exp=_, ty=_} => (error pos ("Error: non-array type used in indexed access");
				     {exp=Tr.nilIR(), ty=E.ERROR})
	      )
	  end
  in
      trexp exp
  end

and transDec(venv, tenv, decs, level, break) =
    let
	fun trdec(venv, tenv, A.VarDec({name, escape, typ, init, pos}), expList) =
	  let
	      fun getType(SOME(ty)) = ty
		| getType(NONE) = E.ERROR
	      fun actualTy ty =
		case ty of
		    E.NAME(name, tyRef) => actualTy(getType(S.look(tenv, name)))
		 |  someTy => someTy
	      val access' = Tr.allocLocal level  (!escape)
	      fun createAssignExp() =
		let
		    val left = Tr.simpleVarIR(access', level)
		    val _ = print ("Simple variable declaration: " ^ (S.name name)^"\n")
		    val right = #exp (transExp(venv, tenv, init, level, break))
		in
		    Tr.assignIR(left, right)
		end
	  in
	      (
		case typ of
		    SOME(symbol, pos) =>
		    (case S.look(tenv, symbol) of
			 SOME ty => (checkTypesAssignable(actualTy ty,
							  #ty (transExp(venv, tenv, init, level, break)),
							  pos,
							  "Error type does not match definition");
				     {venv=S.enter(venv, name,
						   (Env.VarEntry{access=access', ty=actualTy ty, read_only=false})),
				      tenv=tenv, expList=createAssignExp()::expList})
		      |  NONE => (error pos "type not recognized";
				  {venv=venv, tenv=tenv, expList=createAssignExp()::expList})
		    )
		 |  NONE =>
		    let
			val {exp, ty} = transExp(venv, tenv, init, level, break)
		    in
			if E.eq(ty, E.NIL)
			then error pos "Error: variable being declared of indeterminate record type"
			else ();
			{venv=S.enter(venv, name, (Env.VarEntry{access=access', ty=ty, read_only=false})),
			 tenv=tenv, expList=createAssignExp()::expList}
		    end
	      )
	  end
	| trdec(venv, tenv, A.TypeDec(tydeclist), expList) =
	  let
	      fun maketemptydec ({name, ty, pos}, tenv') =
		S.enter(tenv', name, E.ERROR)
	      val temp_tenv = foldl maketemptydec tenv tydeclist
	      fun foldtydec({name, ty, pos}, {venv, tenv, expList}) =
               {venv=venv, tenv=S.enter(tenv, name, transTy(temp_tenv, ty)), expList=expList}
	      val new_env = foldl foldtydec {venv=venv, tenv=tenv, expList=expList} tydeclist

	      fun checkIllegalCycle({name, ty, pos}, ()) =
		let
		    fun checkHelper(seenList, name) =
		      (
			case S.look(#tenv new_env, name) of
			    SOME(E.NAME(symb, _)) => if List.exists (fn y => String.compare(S.name symb,
											    S.name y) = EQUAL) seenList
						     then error pos "Error: cyclic type definition for a"
						     else checkHelper(name::seenList, symb)
			 |  _ => ()
		      )
		in
		    checkHelper([], name)
		end

	      fun checkDuplicates({name, ty, pos}, seenList) =
		if List.exists (fn y => String.compare(S.name name, y) = EQUAL) seenList
		then (error pos "Error : Defined in mutually recursive block"; seenList)
		else (S.name name)::seenList

	  in
	      foldl checkDuplicates [] tydeclist;
	      foldl checkIllegalCycle () tydeclist;
	      new_env
	  end
	| trdec(venv, tenv, A.FunctionDec(fundeclist), expList) =
	  let
	      fun retTylookup rt =
		(case S.look(tenv, rt) of
		     SOME(typ) => typ
		   | NONE => (error 0 ("Return type unrecognized: " ^ S.name rt); E.ERROR)
		)
	      fun transparam {name, escape, typ, pos} =
		(case S.look(tenv, typ) of
		     SOME t => {name=name, escape=escape, ty=t, pos=pos}
		   | NONE => (error 0 ("Parameter type unrecognized: " ^ S.name typ); {name=name, escape=escape, ty=E.ERROR, pos=pos})
		)
	      (* Add to funcdec to venv *)
	      fun enterFuncs (func, venv) =
		let
		    val newlabel = T.newlabel()
		    fun getEscape {name=name', escape=escape', typ=typ', pos=pos'} = !escape'
		    fun genEscapeList params' = map getEscape params'
		in
		    (* For each function create a new level*)
		    case func of
			{name, params, body, pos, result=SOME(rt, pos')} =>
			S.enter(venv, name, Env.FunEntry{level=Tr.newLevel {parent=level, name=newlabel, formals=genEscapeList params},
							 label=newlabel, formals= map #ty (map transparam params), result=retTylookup rt})
		     |  {name, params, body, pos, result=NONE} =>
			S.enter(venv, name, Env.FunEntry{level=Tr.newLevel {parent=level, name=newlabel, formals=genEscapeList params},
							 label=newlabel, formals= map #ty (map transparam params), result=E.UNIT})
		end
	      val venv' = foldr enterFuncs venv fundeclist

	      fun checkfundec({name, params, body, pos, result}) =
		let
		    val Level =
			(case S.look(venv', name) of
			     SOME(Env.FunEntry({level=level', label=_, formals=_, result=_})) => level'
			  |  _ => Tr.newLevel {parent=Tr.outermost, name=T.newlabel(), formals=[]}
			)
		    val _ = print "1"
		    val result_ty =
			(case result of
			     SOME(rt, pos') => retTylookup rt
			  |  NONE => E.UNIT
			)
		    val params' = map transparam params
		    val _ = print "2"
		    val allocatedFormals = Tr.formals Level
		    val _ = print "3"

		    fun enterparam ({name, escape, ty, pos}, (venv, curIndex)) =
		      (S.enter(venv, name, Env.VarEntry{access=List.nth(allocatedFormals, curIndex),
							ty=ty, read_only=false}), curIndex + 1)
		    val _ = print "4"
		    val venv'' = #1 (foldl enterparam (venv', 0) params')
		    val _ = print "5"

		    val body' = transExp (venv'', tenv, body, Level, break)
		in
		    (* Add each function to the IR *)
		    Tr.procEntryExit {level=Level, body=(#exp body')};
		    if not (E.eq((#ty body'), result_ty))
		    then error pos ("Error: function body not of unit type: " ^ S.name name)
		    else ()
		end
	      fun foldfundec (fundec, ()) = checkfundec fundec

	      (* Function to check for duplicate function declaration *)
	      fun checkDuplicates({name, params, body, pos, result}, seenList) =
		if List.exists (fn y => String.compare(S.name name, y) = EQUAL) seenList
		then (error pos "error : two types of same name in mutually recursive fundec"; seenList)
		else (S.name name)::seenList
	  in
	      foldl checkDuplicates [] fundeclist;
	      foldr foldfundec () fundeclist;
	      {venv=venv', tenv=tenv, expList=expList}
	  end
	and folddec(dec, {venv, tenv, expList}) = trdec(venv, tenv, dec, expList)
    in
	foldl folddec {venv=venv, tenv=tenv, expList=[]} decs
    end
and transTy(tenv, ty) =
    let
	fun trty(tenv, A.NameTy (name, _)) =
	  (case S.look(tenv, name) of
	       SOME _ => E.NAME(name, ref(NONE))
	    |  NONE => (error 0 ("Unrecognized name type: " ^ S.name name);
			E.NAME(name, ref(NONE)))
	  )
	  | trty(tenv, A.RecordTy (fields)) =
	    let
		fun fieldProcess {name, escape, typ, pos} =
		  case S.look(tenv, typ) of
		      SOME x => (name, x)
		   |  NONE => (error pos ("Error: Undefined type symbol " ^ S.name typ);
			       (name, E.ERROR))
		fun listConcat(a, b) = fieldProcess(a)::b
		fun recGen () = foldr listConcat [] fields
		val recList = recGen()
	    in
		E.RECORD (recList, ref ())
	    end

	  | trty(tenv, A.ArrayTy (sym, pos')) =
	    E.ARRAY (transTy (tenv, A.NameTy (sym, pos')), ref ())
    in
	trty(tenv, ty)
    end


  fun transProg exp =
  let
    val mainlabel = T.namedlabel "$Main$$"
    val exitlabel = T.newlabel()
    val mainlevel = Tr.newLevel {parent=Tr.outermost, name=mainlabel, formals=[]}
    val {exp=mainExp, ty=mainTy} = (transExp (Env.base_venv, Env.base_tenv, exp, mainlevel, exitlabel))
    in
      Tr.procEntryExit {level=mainlevel, body=mainExp}; ()
      (*Tr.getResults();*)
      (*  # ty typ;()*)
    end


(* need to provided a definition of transProg that takes an abstract
   syntax representation whose type is exp and returns a type or prints
   an error. This function will use transExp transTy and transDec as
   discussed in the book and in class *)

end
