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

datatype binop = PLUS | MINUS | MUL | DIV
		      | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
								   
datatype relop = EQ | NE | LT | GT | LE | GE
		 | ULT | ULE | UGT | UGE
					 

fun IfSameType (tyA, tyB, pos, errMsg) = if E.eq(tyA, tyB)
					      then ()
					      else error pos errMsg
fun checkIfInteger ({exp= _, ty= E.INT}, pos) = ()
  | checkIfInteger ({exp= _, ty= _ }, pos) = error pos "error : Integer Missing "

fun compParam ({exp=_, ty=E.INT}, {exp=_, ty=E.INT}, pos) = ()
  | compParam ({exp=_, ty=E.STRING}, {exp=_, ty=E.STRING}, pos) = ()
  | compParam ({exp=_, ty=E.NIL}, {exp=_, ty=E.RECORD(_, _)}, pos) = ()
  | compParam ({exp=_, ty=E.ARRAY(_, ref1)}, {exp=_, ty=E.ARRAY(_, ref2)}, pos) = if ref1 = ref2 then () else error pos "Diff array types"
  | compParam ({exp=_, ty=E.RECORD(_, ref1)}, {exp=_, ty=E.RECORD(_, ref2)}, pos) = if ref1 = ref2 then () else error pos "Different record types"
  | compParam ({exp=_, ty=E.RECORD(_, _)}, {exp=_, ty=E.NIL}, pos) = ()
  | compParam ({exp=_, ty=_}, {exp=_, ty=_}, pos) = error pos "error : comparison expected both int, string, record, or array"

  (* Only for the comparison operators *)
fun compRelOper ({exp=_, ty=E.INT}, {exp=_, ty=E.INT}, pos) = ()
  | compRelOper ({exp=_, ty=E.STRING}, {exp=_, ty=E.STRING}, pos) = ()
  | compRelOper ({exp=_, ty=_ }, {exp=_, ty=_ }, pos) = error pos "Comparing 2 different types"


fun checkTypesAssignable (var, value, pos, errMsg) = if E.comp(var, value) = E.EQ orelse E.comp(var, value) = E.GT
						     then ()
						     else error pos errMsg

val depth : int ref = ref 0
fun InLoop (pos, errorMsg) =
  if !depth = 0
  then error pos errorMsg
  else ()

       (* Main recursive type-checking functions *)
fun transExp (venv, tenv, exp, level: Tr.level, break) =
  let fun
      trexp (A.VarExp(var)) = trvar var
      | trexp (A.IntExp(intvalue)) = {exp=(Tr.Ex(E.CONST intvalue)), ty=E.INT}
      | trexp (A.StringExp(stringvalue, pos)) = {exp=(Tr.stringIR(stringvalue)), ty=E.STRING} 
      | trexp (A.NilExp) = {exp=(Tr.NIL), ty=E.NIL}
      | trexp (A.CallExp({func, args, pos})) =
	let
	    fun checkArgs (forTy::formalList, argExp::argList, pos) = if E.eq(forTy, #ty (trexp argExp))
								      then checkArgs(formalList, argList, pos)
								      else error pos "error : Parameters mismatch "
	      | checkArgs ([], argExp::argList, pos) = error pos "error : Number of formals < actuals"
	      | checkArgs (forTy::formalList, [], pos) = error pos "error : Number of formals > actuals"
	      | checkArgs ([], [], pos) = ()
	in
	    case S.look(venv, func) of
		SOME(Env.FunEntry({level=declevel, label, formals, result})) => (checkArgs(formals, args, pos); {exp=(), ty=result})
	     |  SOME(_) => (error pos ("symbol not function " ^ S.name func); {exp=(), ty=E.ERROR})
	     |  NONE => (error pos ("no such function " ^ S.name func); {exp=(), ty=E.ERROR})
	end
      | trexp (A.OpExp{left, oper, right, pos}) =
	(case oper of
	     A.PlusOp => (checkIfInteger(trexp left, pos);
			  checkIfInteger(trexp right, pos);
			  {exp=(Tr.binopIR(PLUS, #exp (trexp left), #exp (trexp right))), ty=E.INT})
	  |  A.MinusOp => (checkIfInteger(trexp left, pos);
			   checkIfInteger(trexp right, pos);
			   {exp=(Tr.binopIR(MINUS, #exp (trexp left), #exp (trexp right))), ty=E.INT})
	  |  A.TimesOp => (checkIfInteger(trexp left, pos);
			   checkIfInteger(trexp right, pos);
			   {exp=(Tr.binopIR(MUL, #exp (trexp left), #exp (trexp right))), ty=E.INT})
	  |  A.DivideOp => (checkIfInteger(trexp left, pos);
			    checkIfInteger(trexp right, pos);
			    {exp=(Tr.binopIR(DIV, #exp (trexp left), #exp (trexp right))), ty=E.INT})
	  |  A.EqOp => (compParam(trexp left, trexp right, pos);
			let
			    val {exp=exp', ty=ty'} = trexp left
			in
			    {exp= Tr.relopIR(EQ, exp', #exp (trexp right), ty'), ty=E.INT}
			end
		       )
	  |  A.NeqOp => (compParam(trexp left, trexp right, pos);
                         let
			     val {exp=exp', ty=ty'} = trexp left
			 in
			     {exp= Tr.relopIR(NE, exp', #exp (trexp right), ty'), ty=E.INT}
			 end
			)
	  |  A.LtOp => (compRelOper(trexp left, trexp right, pos);
		        let
			    val {exp=exp', ty=ty'} = trexp left
			in
			    {exp= Tr.relopIR(LT, exp', #exp (trexp right), ty'), ty=E.INT}
			end
		       )
	  |  A.LeOp => (compRelOper(trexp left, trexp right, pos);
		        let
			    val {exp=exp', ty=ty'} = trexp left
			in
			    {exp= Tr.relopIR(EQ, exp', #exp (trexp right), ty'), ty=E.INT}
			end
		       )
	  |  A.GtOp => (compRelOper(trexp left, trexp right, pos);
		        let
			    val {exp=exp', ty=ty'} = trexp left
			in
			    {exp= Tr.relopIR(GT, exp', #exp (trexp right), ty'), ty=E.INT}
			end
		       )
	  |  A.GeOp => (compRelOper(trexp left, trexp right, pos);
			let
		            val {exp=exp', ty=ty'} = trexp left
		        in
		            {exp= Tr.relopIR(GE, exp', #exp (trexp right), ty'), ty=E.INT}
		        end
		       )
	)
      | trexp (A.RecordExp({fields, typ, pos})) = {exp=(), ty=E.ERROR}
    (*
	(case S.look(tenv, typ) of
	     SOME x =>
	     (case x of
		  E.RECORD(f, _) =>
		  let
		  in
		  end
	     )
	  |  NONE => (error pos ("error : invalid record type: " ^ S.name typ); {exp=(), ty=E.NIL})
	)
    *)
    
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
            {exp=Tr.Nx(Tr.seq(#stmlist (checkSequence expList))), ty= (#ty (checkSequence expList))}
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
		    then error pos "error : index variable erroneously assigned to"
		    else ()
	        |  _ => error pos "cannot assign to a function"
	in
	    canAssign var;
	    checkTypesAssignable(#ty (trvar var), #ty (trexp exp), pos, "error : mismatched types in assignment");

	    {exp=(), ty=E.UNIT}
	end
      | trexp (A.IfExp({test, then', else', pos})) =
	(
	  IfSameType(#ty (trexp test), E.INT, pos, "test in if exp does not evaluate to an int");
	  case else' of
	      SOME(elseExp) =>
	      (
		case (#ty (trexp then'), #ty (trexp elseExp)) of
		    (E.RECORD(_), NIL) => ()
		 |  (NIL, E.RECORD(_)) => ()
		 |  (tyA, tyB) => IfSameType(tyA, tyB, pos, "error : types of then - else differ")
	      )
	   |  NONE => IfSameType(#ty (trexp then'), E.UNIT, pos, "error : if-then returns non unit");
	  {exp=(), ty=(#ty (trexp then'))}
	)
      | trexp (A.WhileExp({test, body, pos})) =
	(
	  IfSameType(#ty (trexp test), E.INT, pos, "test does not evaluate to an int");
	  depth := !depth + 1;
	  IfSameType(#ty (trexp body), E.UNIT, pos, "error : body of while not unit");
	  depth := !depth - 1;
	  {exp=(), ty=E.UNIT}
	)
      | trexp (A.ForExp({var, escape, lo, hi, body, pos})) =
	let
	    val venv' = S.enter(venv, var, Env.VarEntry({access= Tr.allocLocal level true, ty=E.INT, read_only=true}))
	    val breakpoint = T.newlabel()
	    val _ = IfSameType(#ty (trexp lo), E.INT, pos, "error : lo expr is not int")
	    val _ = IfSameType(#ty (trexp hi), E.INT, pos, "error : hi expr is not int")
	    val _ = depth := !depth + 1
	    (* val _ = IfSameType(#ty (transExp(venv', tenv, body, level, breakpoint)), E.UNIT, pos, "for body must be no value") *)
	    val {exp=bodyexp, ty=bodytype} =  transExp(venv', tenv, body, level, breakpoint)
	    val _ = depth := !depth - 1
	in
	    case S.look(venv', var) of
		SOME x =>
		(case x of
		     Env.VarEntry{access, ty, read_only} => {exp=Tr.forIR(Tr.simpleVarIR(access, level),
									 escape,
									 #exp (trexp lo),
									 #exp (trexp hi),
									 bodyexp,
									 breakpoint),
							     ty=E.UNIT}
		)
	end
      | trexp (A.BreakExp(pos)) =
	(
	  InLoop(pos, "incorrect break");
	  {exp=(), ty=E.UNIT}
	)
      | trexp (A.LetExp({decs, body, pos})) =
	let
	    val curDepth = !depth
	    val _ =  depth := 0
	    val {venv=venv', tenv=tenv', expList=expList'} = transDec(venv, tenv, decs, level, break)
	    val _ =  depth := curDepth
	in
	    transExp(venv', tenv', body, level, break)
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
			      IfSameType(#ty (trexp init), actualTy ty, pos, "error : initializing exp and array type differ");
			      {exp=(), ty=E.ARRAY(ty, unique)}
			  end
			)
		     |  _ => (error pos "Not of ARRAY type in array creation"; {exp=(), ty=E.ERROR})
		  )
	       |  NONE => (error pos "No such type"; {exp=(), ty=E.ERROR})
	    )
	end
      and trvar (A.SimpleVar(id, pos)) =
	  (case S.look(venv, id) of
	       SOME(Env.VarEntry({access, ty, read_only=_})) => {exp=Tr.simpleVarIR(access, level), ty=ty}
	    (*|  SOME(Env.FunEntry({level, label, formals, result})) => {exp=(), ty=result} *)
	    |  NONE => (error pos ("error: undeclared variable " ^ S.name id); {exp=(), ty=E.ERROR})
	  )
	| trvar (A.FieldVar(v, id, pos)) = {exp=(), ty=E.UNIT}

	  (case trvar v of
	       {exp=(), ty=E.RECORD(recGen, unique)} =>
	       let
			 case S.look(tenv, fTy) of
			     SOME(ty) => ty
			  |  NONE => (error pos "type error in record"; E.ERROR)
		     else getFieldType(l, id, pos)
		     | getFieldType ([], id, pos) = (error pos "no such field"; E.ERROR)
	       in
		   {exp=(), ty=getFieldType(fields, id, pos)}
	       end
	    |  {exp=_, ty=_} => (error pos ("error : variable not record"); {exp=(), ty=E.ERROR})
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
		   {exp=(), ty=E.ARRAY(arrTy, unique)} => (checkIfInteger(trexp subExp, pos); {exp=(), ty=actualTy arrTy})
		|  {exp=_, ty=_} => (error pos ("requires array"); {exp=(), ty=E.ERROR})
	      )
	  end
  in
      trexp exp
  end

and transDec(venv, tenv, decs, level, break) =
    let fun
        trdec(venv, tenv, A.VarDec({name, escape, typ, init, pos}), expList) =
	  let
	      fun getType(SOME(ty)) = ty
		| getType(NONE) = E.ERROR
	      fun actualTy ty =
		case ty of
		    E.NAME(name, tyRef) => actualTy(getType(S.look(tenv, name)))
		 |  someTy => someTy
	      val access' = Tr.allocLocal level (!escape)
	      fun createAssignExp() =
		let
		    val left = Tr.simpleVarIR(access', level)
		    val right = #exp (transExp(venv, tenv, init, level, break))
		in
		    Tr.assignIR(left, right)
		end
	  in
	      (
		case typ of
		    SOME(symbol, pos) =>
		    (case S.look(tenv, symbol) of
			 SOME ty => (checkTypesAssignable(actualTy ty, #ty (transExp(venv, tenv, init, level, break)), pos, "error : mismatched types in vardec");
				     {venv=S.enter(venv, name, (Env.VarEntry{access=access', ty=actualTy ty, read_only=false})),
				      tenv=tenv, expList=createAssignExp()::expList})
		      |  NONE => (error pos "type not recognized"; {venv=venv, tenv=tenv, expList=createAssignExp()::expList})
		    )
		 |  NONE =>
		    let
			val {exp, ty} = transExp(venv, tenv, init, level, break)
		    in
			if E.eq(ty, E.NIL)
			then error pos "error: initializing nil expressions not constrained by record type"
			else ();
			{venv=S.enter(venv, name, (Env.VarEntry{access=access', ty=ty, read_only=false})),
			 tenv=tenv, expList=createAssignExp()::expList}
		    end
	      )
	  end
	| trdec(venv, tenv, A.TypeDec(tydeclist), expList) =
	  let
	      fun maketemptydec ({name, ty, pos}, tenv') = S.enter(tenv', name, E.ERROR)
	      val temp_tenv = foldl maketemptydec tenv tydeclist
	      fun foldtydec({name, ty, pos}, {venv, tenv, expList}) = {venv=venv, tenv=S.enter(tenv, name, transTy(temp_tenv, ty)), expList=expList}
	      val new_env = foldl foldtydec {venv=venv, tenv=tenv, expList=expList} tydeclist

	      fun checkIllegalCycle({name, ty, pos}, ()) =
		let
		    fun checkHelper(seenList, name) =
		      (
			case S.look(#tenv new_env, name) of
			    SOME(E.NAME(symb, _)) => if List.exists (fn y => String.compare(S.name symb, S.name y) = EQUAL) seenList
						     then error pos "error: mutually recursive types thet do not pass through record or array - cycle"
						     else checkHelper(name::seenList, symb)
			 |  _ => ()
		      )
		in
		    checkHelper([], name)
		end

	      fun checkDuplicates({name, ty, pos}, seenList) =
		if List.exists (fn y => String.compare(S.name name, y) = EQUAL) seenList
		then (error pos "error : two types of same name in mutually recursive tydec"; seenList)
		else (S.name name)::seenList

	  in
	      foldl checkDuplicates [] tydeclist;
	      foldl checkIllegalCycle () tydeclist;
	      new_env
	  end 
	| trdec(venv, tenv, A.FunctionDec(fundeclist), expList) =
	  let
	      fun transrt rt =
		(case S.look(tenv, rt) of
		     SOME(rt') => rt'
		  |  NONE => (error 0 ("Return type unrecognized: " ^ S.name rt); E.ERROR)
		)
	      fun transparam {name, escape, typ, pos} =
		(case S.look(tenv, typ) of
		     SOME t => {name=name, escape=escape, ty=t, pos=pos}
		  |  NONE => (error 0 ("Parameter type unrecognized: " ^ S.name typ); {name=name, escape=escape, ty=E.ERROR, pos=pos})
		)
              fun enterFuncs (func, venv) =
		let
		    val newlabel = T.newlabel()
		    fun getEscape {name=name', escape=escape', typ=typ', pos=pos'} = !escape'
		    fun genEscapeList params' = map getEscape params'
		in
		    case func of
			{name, params, body, pos, result=SOME(rt, pos')} =>
			S.enter(venv, name, Env.FunEntry{level=Tr.newLevel {parent=level, name=newlabel, formals=genEscapeList params},
							 label=newlabel, formals= map #ty (map transparam params), result=transrt rt})
		     |  {name, params, body, pos, result=NONE} =>
			S.enter(venv, name, Env.FunEntry{level=Tr.newLevel {parent=level, name=newlabel, formals=genEscapeList params},
							 label=newlabel, formals= map #ty (map transparam params), result=E.UNIT})
		end
	      val venv' = foldr enterFuncs venv fundeclist
	      fun checkfundec({name, params, body, pos, result}) =
		let
		    val newLevel =
			(case S.look(venv', name) of
			     SOME(Env.FunEntry({level=level', label=_, formals=_, result=_})) => level'
			  |  _ => Tr.newLevel {parent=Tr.outermost, name=T.newlabel(), formals=[]}
			)
		    val result_ty =
			(case result of
			     SOME(rt, pos') => transrt rt
			  |  NONE => E.UNIT
			)
		    val params' = map transparam params
		    val allocatedFormals = Tr.formals newLevel
		    fun enterparam ({name, escape, ty, pos}, (venv, curIndex)) =
		      (S.enter(venv, name, Env.VarEntry{access=List.nth(allocatedFormals, curIndex),
							ty=ty, read_only=false}), curIndex + 1)
		    val venv'' = #1 (foldl enterparam (venv', 1) params')
		    val body' = transExp (venv'', tenv, body, newLevel, break)
		in
		    Tr.procEntryExit {level=newLevel, body=(#exp body')};
		    if not (E.eq((#ty body'), result_ty))
		    then error pos ("Function body type doesn't match return type in function " ^ S.name name)
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
	      {venv=venv', tenv=tenv, expList=expList}
	  end
	and folddec(dec, {venv, tenv, expList}) = trdec(venv, tenv, dec, expList)
    in
	foldl folddec {venv=venv, tenv=tenv, expList=[]} decs
    end
and transTy(tenv, ty) =
    let fun
	trty(tenv, A.NameTy (name, _)) =
	  (case S.look(tenv, name) of
	       SOME _ => E.NAME(name, ref(NONE))
	    |  NONE => (error 0 ("Unrecognized name type: " ^ S.name name); E.NAME(name, ref(NONE)))
	  )
	| trty(tenv, A.RecordTy (fields)) = E.UNIT
	(*
	  let
	      fun fieldProcess {name, escape, typ, pos} =
		case S.look(tenv, typ) of
		    SOME _ => (name, typ)
		 |  NONE => (error pos ("undefined type in rec: " ^ S.name typ); (name, typ))
	      fun listConcat(a, b) = fieldProcess(a)::b
	      fun recGen () = foldl listConcat [] fields
	  in
	      recGen();
	      E.RECORD (recGen, ref ())
	  end
	*)
	| trty(tenv, A.ArrayTy (sym, pos')) =
	  E.ARRAY (transTy (tenv, A.NameTy (sym, pos')), ref ())
    in
	trty(tenv, ty)
    end 

	
fun transProg exp = 
  let 
      val mainlabel = T.newlabel()
      val mainlevel = Tr.newLevel {parent=Tr.outermost, name=mainlabel, formals=[]}
      (*val _ = FindEscape.findEscape my_exp*)
      val mainexp = #exp (transExp (Env.base_venv, Env.base_tenv, exp, mainlevel, mainlabel))
    in 
      Tr.procEntryExit {level=mainlevel, body=mainexp};
      Tr.getResult()
      (*  # ty typ;()*)
    end 

(* need to provided a definition of transProg that takes an abstract 
   syntax representation whose type is exp and returns a type or prints
   an error. This function will use transExp transTy and transDec as 
   discussed in the book and in class *)

end

    
