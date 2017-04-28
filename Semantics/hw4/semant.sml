signature SEMANT =
sig
  type exp
  type transty
  val transProg: exp -> (transty)
end


functor SemantFun(structure A: ABSYN
                  structure E: ENV
                  structure Symbol: SYMBOL
                    sharing type A.symbol = E.symbol = Symbol.symbol
                        and type E.table = Symbol.table) : SEMANT
             where type transty = E.ty
               and type exp = A.exp =
struct

type  exp = A.exp
type  transty = E.ty
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

(* Main recursive type-checking functions *)
fun transExp (venv, tenv, exp) =
  let fun
      trexp (A.VarExp(var)) = trvar var
      | trexp (A.IntExp(intvalue)) = {exp=(), ty=E.INT}
      | trexp (A.StringExp(stringvalue, pos)) = {exp=(), ty=E.STRING}
      | trexp (A.NilExp) = {exp=(), ty=E.NIL}
      | trexp (A.CallExp({func, args, pos})) =
	let
	    (* recursively loop on all parameters to check if they are of the same type and number*)
	    fun checkArgs (forTy::formalList, argExp::argList, pos) = if E.eq(forTy, #ty (trexp argExp))
								      then checkArgs(formalList, argList, pos)
								      else error pos "error : Wrong type of parameter."
	      | checkArgs ([], argExp::argList, pos) = error pos "Error: functiong applied to too many arguments"
	      | checkArgs (forTy::formalList, [], pos) = error pos "Error: functiong applied to too few arguments"
	      | checkArgs ([], [], pos) = ()
	in
	    case S.look(venv, func) of
		SOME(Env.FunEntry({formals, result})) => (checkArgs(formals, args, pos); {exp=(), ty=result})
	     |  SOME(_) => (error pos ("symbol not function " ^ S.name func); {exp=(), ty=E.ERROR})
	     |  NONE => (error pos ("Error: undefined function " ^ S.name func); {exp=(), ty=E.ERROR})
	end

      | trexp (A.OpExp{left, oper, right, pos}) =
	(case oper of
	     A.PlusOp => (checkIfInteger(trexp left, pos);
			  checkIfInteger(trexp right, pos);
			  {exp=(), ty=E.INT})
	  |  A.MinusOp => (checkIfInteger(trexp left, pos);
			   checkIfInteger(trexp right, pos);
			   {exp=(), ty=E.INT})
	  |  A.TimesOp => (checkIfInteger(trexp left, pos);
			   checkIfInteger(trexp right, pos);
			   {exp=(), ty=E.INT})
	  |  A.DivideOp => (checkIfInteger(trexp left, pos);
			    checkIfInteger(trexp right, pos);
			    {exp=(), ty=E.INT})
	  |  A.EqOp => (compParam(trexp left, trexp right, pos);
			{exp=(), ty=E.INT})
	  |  A.NeqOp => (compParam(trexp left, trexp right, pos);
			 {exp=(), ty=E.INT})
	  |  A.LtOp => (compRelOper(trexp left, trexp right, pos);
			{exp=(), ty=E.INT})
	  |  A.LeOp => (compRelOper(trexp left, trexp right, pos);
			{exp=(), ty=E.INT})
	  |  A.GtOp => (compRelOper(trexp left, trexp right, pos);
			{exp=(), ty=E.INT})
	  |  A.GeOp => (compRelOper(trexp left, trexp right, pos);
			{exp=(), ty=E.INT})
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
                           then (#ty (trexp exp))
                           else getFieldType(name, l))
                      fun checkFormal (sym, ty) = (getFieldType(S.name sym, fields);())
                      (*if not (E.leq(getFieldType(S.name sym, fields), ty))
                        then error pos ("actual type doesn't match formal type: " ^ S.name sym)
                        else ()*)
                      fun iterator((fieldname, typeid), ()) =
                        (checkFormal (fieldname, x); ())

                        (*case S.look(tenv, typeid) typeid of
                            SOME x => (checkFormal (fieldname, x); ())
                          | NONE => (error pos ("unknown type in record: " ^ S.name typ); ()))*)
                  in
                      if List.length(recFormal) <> List.length(fields)
                      then (error pos ("record list is wrong length: " ^ S.name typ);
			    {exp=(), ty=x})
                      else (foldr iterator () recFormal;
			    {exp=(), ty=x})
                  end
                | _ => (error pos ("error : expected record type, not: " ^ S.name typ); {exp=(), ty=E.NIL})
             )
           | NONE => (error pos ("Error: undefined type symbol " ^ S.name typ); {exp=(), ty=E.NIL})
        )
      | trexp (A.SeqExp(expList)) =
	let
	    fun helper((seqExp, pos), {exp=_, ty=_}) = (trexp seqExp)
	    fun a sequence = foldl helper {exp=(), ty=E.UNIT} sequence
	in
	    a expList
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
		  SOME(Env.VarEntry({ty, read_only})) =>
		  if read_only
		  then error pos "Error: illegal assignment to loop control variable"
		  else ()
	       |  _ => error pos "Error cannot assign to a function"
	in
	    canAssign var;
	    checkTypesAssignable(#ty (trvar var),
				 #ty (trexp exp),
				 pos, "error : Wrong type used for assignment");
	    {exp=(), ty=E.UNIT}
	end
      | trexp (A.IfExp({test, then', else', pos})) =
	(
	  if E.eq(#ty (trexp test), E.INT)
	  then (
	      case else' of
		  SOME(elseExp) =>
		  (
		    case (#ty (trexp then'), #ty (trexp elseExp)) of
			(E.RECORD(_), NIL) => ({exp=(), ty=(#ty (trexp then'))})
		     |  (NIL, E.RECORD(_)) => ({exp=(), ty=(#ty (trexp then'))})
		     |  (tyA, tyB) => if E.eq(tyA, tyB)
				      then      ({exp=(), ty=(#ty (trexp then'))})
				      else  (error pos "Error: types of conditional branches do not match"; {exp=(), ty=(E.ERROR)})
		  )
	       |  NONE => if E.eq (#ty (trexp then'), E.UNIT)
			  then({exp=(), ty=E.UNIT})
			  else  (error pos "Error: types of conditional branches do not match"; {exp=(), ty=(E.ERROR)})
	  )
	  else 
	      (error pos "Condition in if is not INTEGER!"; {exp=(), ty=(E.ERROR)})
	)
      | trexp (A.WhileExp({test, body, pos})) =
	(
	  if E.eq(#ty (trexp test), E.INT)
	  then
	      (
		depth := !depth + 1;
	      if E.eq (#ty (trexp body), E.UNIT)
	      then
		  (
		    depth := !depth - 1;
		    {exp=(), ty=E.UNIT}
		  )
	      else
		  (error pos "Error: unit type expected for body of while" ; {exp=(), ty=(E.UNIT)})
	      )
	  else (error pos "test does not evaluate to an int"; {exp=(), ty=(E.ERROR)})
	)
      | trexp (A.ForExp({var, escape, lo, hi, body, pos})) =
	let
	    val venv' = S.enter(venv, var, Env.VarEntry({ty=E.INT, read_only=true}))
	in
	    if E.eq (#ty (trexp lo), E.INT)
	    then
		(
		  if E.eq(#ty (trexp hi), E.INT)
		  then
		      (
			depth := !depth + 1;
			if E.eq(#ty (transExp(venv', tenv, body)), E.UNIT)
			then
			    (
			      depth := !depth - 1;
			      {exp=(), ty=E.UNIT}
			    )
			else
			    (error pos "for body must be no value"; {exp=(), ty=E.ERROR})
		      )
		  else
		      (error pos "Error: high value of for must be of integer type"; {exp=(), ty=E.ERROR})
		)
	    else 
		(error pos "Error: low value of for must be of integer type"; {exp=(), ty=E.ERROR})
	end
      | trexp (A.BreakExp(pos)) =
	(
	  InLoop(pos, "Invalid break: Not in a Loop");
	  {exp=(), ty=E.UNIT}
	)
      | trexp (A.LetExp({decs, body, pos})) =
	let
	    val curDepth = !depth
	    val _ =  depth := 0
	    val {venv=venv', tenv=tenv'} = transDec(venv, tenv, decs)
	    val _ =  depth := curDepth
	in
	    transExp(venv', tenv', body)
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
			      then
				  {exp=(), ty=E.ARRAY(ty, unique)}
			      else
				  (error pos  "Error: integer expression expected in array initialization"; {exp=(), ty=E.ERROR})
			  end
			)
		     |  _ => (error pos "Not of ARRAY type in array creation"; {exp=(), ty=E.ERROR})
		  )
	       |  NONE => (error pos "No such type"; {exp=(), ty=E.ERROR})
	    )
	end
      and trvar (A.SimpleVar(id, pos)) =
	  (case S.look(venv, id) of
	       SOME(Env.VarEntry({ty, read_only=_})) => {exp=(), ty=ty}
	    |  SOME(Env.FunEntry({formals, result})) => {exp=(), ty=result}
	    |  NONE => (error pos ("Error: undefined variable "
				   ^ S.name id); {exp=(), ty=E.ERROR})
	  )
	| trvar (A.FieldVar(v, id, pos)) =
	  (case trvar v of
	       {exp=(), ty=E.RECORD(recGen, unique)} =>
	       let
		   fun getFieldType ((fSymbol, fTy)::l, id, pos) =
		     if String.compare(S.name fSymbol, S.name id) = EQUAL
		     then fTy
		     else getFieldType(l, id, pos)
		     | getFieldType ([], id, pos) = (error pos ("Error: no record field with name " ^ S.name id); E.ERROR)
	       in
		   {exp=(), ty=getFieldType(recGen, id, pos)}
	       end
	     | {exp=_, ty=_} => (error pos ("Error: non-record type used in record selection");
				 {exp=(), ty=E.ERROR})
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
		   {exp=(), ty=E.ARRAY(arrTy, unique)} => (checkIfInteger(trexp subExp, pos);
							   {exp=(), ty=actualTy arrTy})
		|  {exp=_, ty=_} => (error pos ("Error: non-array type used in indexed access");
				     {exp=(), ty=E.ERROR})
	      )
	  end
  in
      trexp exp
  end
and transDec(venv, tenv, decs) =
    let fun
	trdec(venv, tenv, A.VarDec({name, escape, typ, init, pos})) =
	  let
	      fun getType(SOME(ty)) = ty
		| getType(NONE) = E.ERROR
	      fun actualTy ty =
		case ty of
		    E.NAME(name, tyRef) => actualTy(getType(S.look(tenv, name)))
		 |  someTy => someTy
	  in
	      (
		case typ of
		    SOME(symbol, pos) =>
		    (case S.look(tenv, symbol) of
			 SOME ty => (checkTypesAssignable(actualTy ty,
							  #ty (transExp(venv, tenv, init)),
							  pos,
							  "Error type does not match definition");
				     {venv=S.enter(venv, name,
						   (Env.VarEntry{ty=actualTy ty, read_only=false})),
				      tenv=tenv})
		      |  NONE => (error pos "type not recognized";
				  {venv=venv, tenv=tenv})
		    )
		 |  NONE =>
		    let
			val {exp, ty} = transExp(venv, tenv, init)
		    in
			if E.eq(ty, E.NIL)
			then error pos "Error: variable being declared of indeterminate record type"
			else ();
			{venv=S.enter(venv, name, (Env.VarEntry{ty=ty, read_only=false})),
			 tenv=tenv}
		    end
	      )
	  end
	| trdec(venv, tenv, A.TypeDec(tydeclist)) =
	  let
	      fun maketemptydec ({name, ty, pos}, tenv') =
		S.enter(tenv', name, E.ERROR)
	      val temp_tenv = foldl maketemptydec tenv tydeclist
	      fun foldtydec({name, ty, pos}, {venv, tenv}) =
		{venv=venv, tenv=S.enter(tenv, name, transTy(temp_tenv, ty))}
	      val new_env = foldl foldtydec {venv=venv, tenv=tenv} tydeclist

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
	| trdec(venv, tenv, A.FunctionDec(fundeclist)) =
	  let
	      fun transrt rt =
		(case S.look(tenv, rt) of
		     SOME(rt') => rt'
		  |  NONE => (error 0 ("Return type unrecognized: " ^ S.name rt); E.ERROR)
		)
	      fun transparam {name, escape, typ, pos} =
		(case S.look(tenv, typ) of
		     SOME t => {name=name, ty=t}
		  |  NONE => (error 0 ("Parameter type unrecognized: " ^ S.name typ); {name=name, ty=E.ERROR})
		)
	      fun enterFuncs ({name, params, body, pos, result=SOME(rt, pos')}, venv) =
		S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params),
						 result=transrt rt})
		| enterFuncs ({name, params, body, pos, result=NONE}, venv) =
		  S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params),
						   result=E.UNIT})
	      val venv' = foldr enterFuncs venv fundeclist
	      fun checkfundec({name, params, body, pos, result}) =
		let
		    val result_ty =
			(case result of
			     SOME(rt, pos') => transrt rt
			  |  NONE => E.UNIT
			)
		    val params' = map transparam params
		    fun enterparam ({name, ty}, venv) = S.enter(venv, name,
								Env.VarEntry{ty=ty, read_only=false})
		    val venv'' = foldl enterparam venv' params'
		    val body' = transExp (venv'', tenv, body)
		in
		    if not (E.eq((#ty body'), result_ty))
		    then error pos ("Error: function body not of unit type: " ^ S.name name)
		    else ()
		end
	      fun foldfundec (fundec, ()) = checkfundec fundec
	      fun checkDuplicates({name, params, body, pos, result}, seenList) =
		if List.exists (fn y => String.compare(S.name name, y) = EQUAL) seenList
		then (error pos "error : two types of same name in mutually recursive fundec"; seenList)
		else (S.name name)::seenList
	  in
	      foldl checkDuplicates [] fundeclist;
	      foldr foldfundec () fundeclist;
	      {venv=venv', tenv=tenv}
	  end
	and folddec(dec, {venv, tenv}) = trdec(venv, tenv, dec)
    in
	foldl folddec {venv=venv, tenv=tenv} decs
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
       val typ=transExp (E.base_venv, E.base_tenv, exp)
    in
        # ty typ
    end

(* need to provided a definition of transProg that takes an abstract
   syntax representation whose type is exp and returns a type or prints
   an error. This function will use transExp transTy and transDec as
   discussed in the book and in class *)

end
