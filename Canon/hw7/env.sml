signature ENV =
sig

  type unique = unit ref

  eqtype symbol

  type access  
  type level
  type label
  type 'a table

  datatype ty = 
            RECORD of (symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
	  | NAME of symbol * ty option ref
	  | UNIT
	  | ERROR


  datatype fnvar =
       VarEntry of {access: access, ty: ty, read_only:bool}
     | FunEntry of {level: level, label: label, formals: ty list, result: ty}

  val base_tenv : ty table      (* predefined types *)

  val base_venv : fnvar table   (* predefined functions *)

    datatype comp =
            LT
          | GT
          | EQ
          | INCOMP (* incomparable *)
    val eq : ty*ty->bool
    val comp : ty*ty->comp
    val leq : ty*ty->bool
end


functor EnvFun(structure Symbol : SYMBOL
               structure T: TEMP
               structure Tr: TRANSLATE
                 sharing type T.label = Tr.label) : ENV =
struct

  type unique = unit ref

  type symbol = Symbol.symbol

  type 'a table = 'a Symbol.table
  type access = Tr.access
  type label = T.label
  type level = Tr.level

  datatype ty = 
            RECORD of (symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
	  | NAME of symbol * ty option ref
	  | UNIT
	  | ERROR

  datatype fnvar =
       VarEntry of {access: access, ty: ty, read_only: bool}
     | FunEntry of {level: level, label: label, formals: ty list, result: ty}

    datatype comp =
            LT
          | GT
          | EQ
          | INCOMP (* incomparable *)

    fun leq(_, UNIT) = true
      | leq(NIL, RECORD(_)) = true
      | leq(INT, INT) = true
      | leq(STRING, STRING) = true
      | leq(RECORD(_, unique1), RECORD(_, unique2)) = (unique1 = unique2)
      | leq(ARRAY(_, unique1), ARRAY(_, unique2)) = (unique1 = unique2)
      | leq(NIL, NIL) = true
      | leq(NAME(sym1, _), NAME(sym2, _)) = String.compare(Symbol.name sym1, Symbol.name sym2) = EQUAL (* TODO is this correct? *)
      | leq(_, _) = false

    fun comp(t1, t2) = 
    	if leq(t1, t2) andalso leq(t2, t1)
    		then EQ
    	else if leq(t1, t2)
    		then LT
    	else if leq(t2, t1)
    		then GT
    	else
    		INCOMP

    fun eq(t1, t2) = comp(t1, t2) = EQ

    fun printTy ty =
      case ty of
           RECORD(_, _) => print "type is record\n"
         | NIL => print "type is nil\n"
         | INT => print "type is int\n"
         | STRING => print "type is string\n"
         | ARRAY(arrTy, _) => (print "array: "; printTy ty)
         | NAME(sym, _) => print ("name type is " ^ Symbol.name sym ^ "\n")
         | UNIT => print "type is unit\n"
         | ERROR => print "type is Error\n"
  
  (* predefined types *)
  val base_tenv : ty table = 
    (Symbol.enter 
           ((Symbol.enter (Symbol.empty,
                           (Symbol.symbol "string"),
                           STRING)),
            (Symbol.symbol "int"),
           INT))

  (* predefined functions; incomplete and should be completed using the
     list on page 519 *)
  val base_venv : fnvar table = 
    (Symbol.enter
       ((Symbol.enter 
          ((Symbol.enter 
             ((Symbol.enter
                ((Symbol.enter 
                   ((Symbol.enter 
                      ((Symbol.enter 
                         ((Symbol.enter 
                            ((Symbol.enter 
                               ((Symbol.enter
                                  (Symbol.empty,
                                   (Symbol.symbol "print"),
                                   let val name = T.namedlabel("$Print$$")
                                   in (FunEntry {level=Tr.newLevel
                                                        { parent=Tr.outermost,
                                                          name=name,
                                                          formals=[false]
                                                        },
                                                 label=name,
	  			 	         formals=[STRING],
                                                 result=UNIT
                                                })
                                   end)),
                                (Symbol.symbol "flush"),
                                let val name = T.namedlabel "$Flush$$"
                                in (FunEntry {level=Tr.newLevel
                                                     { parent=Tr.outermost,
                                                       name=name,
                                                       formals=[]
                                                     },
                                              label=name,
                                              formals=[],
                                              result=UNIT
                                             })
                                end)),
                            (Symbol.symbol "getchar"),
                            let val name = T.namedlabel "$GetChar$$"
                            in (FunEntry {level=Tr.newLevel
                                                  { parent=Tr.outermost,
                                                    name=name,
                                                    formals=[]
                                                  },
                                          label=name,
                                          formals=[],
                                          result=STRING
                                         })
                            end)),
                         (Symbol.symbol "ord"),
                         let val name = T.namedlabel "$Ord$$"
                         in (FunEntry {level=Tr.newLevel
                                              { parent=Tr.outermost,
                                                name=name,
                                                formals=[false]
                                              },
                                       label=name,
                                       formals=[STRING],
                                       result=INT
                                      })
                         end)),
                      (Symbol.symbol "chr"),
                      let val name = T.namedlabel "$Chr$$"
                      in (FunEntry {level=Tr.newLevel
                                            { parent=Tr.outermost,
                                              name=name,
                                              formals=[false]
                                            },
                                    label=name,
                                    formals=[INT],
                                    result=STRING
                                   })
                      end)),
                   (Symbol.symbol "size"),
                   let val name = T.namedlabel "$Size$$"
                   in (FunEntry {level=Tr.newLevel
                                           { parent=Tr.outermost,
                                             name=name,
                                             formals=[false]
                                           },
                                 label=name,
                                 formals=[STRING],
                                 result=INT
                                })
                   end)),
                (Symbol.symbol "substring"),
                let val name = T.namedlabel "$SubString$$"
                in (FunEntry {level=Tr.newLevel
                                        { parent=Tr.outermost,
                                          name=name,
                                          formals=[false,false,false]
                                        },
                              label=name,
                              formals=[STRING,INT,INT],
                              result=STRING
                             })
                end)),
             (Symbol.symbol "concat"),
             let val name = T.namedlabel "$Concat$$"
             in (FunEntry {level=Tr.newLevel{parent=Tr.outermost,
                                             name=name,
                                             formals=[false,false]
                                            },
                           label=name,
		           formals=[STRING,STRING],
                           result=STRING
                          })
             end)),
          (Symbol.symbol "not"),
          let val name = T.namedlabel "$Not$$"
          in (FunEntry {level=Tr.newLevel { parent=Tr.outermost,
                                            name=name,
                                            formals=[false]
                                          },
                        label=name,
                        formals=[INT],
                        result=INT})
          end)),
       (Symbol.symbol "exit"),
       let val name = T.namedlabel "$Exit$$"
       in (FunEntry { level=Tr.newLevel { parent=Tr.outermost,
                                          name=name,
                                          formals=[false]
                                        },
                      label=name,
                      formals=[INT],
                      result=UNIT})
       end))

end

