(* Interaction with the Parser*)
type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) Tokens.token

val com_lev=ref 0;
val str_pos=ref 0;
val str_val=ref "";
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val stringSection = ref 0;
fun err(p1,p2) = ErrorMsg.error p1

  (* Handle scenarios when we are still in comment or String section*)  
fun eof() =
  let
      val pos = hd(!linePos)
  in
      if(!com_lev > 0) then ErrorMsg.error pos ("Unclosed Comment at EOF")
      else if (!stringSection = 1) then ErrorMsg.error pos ("Unclosed String at EOF")
      else ();
      stringSection := 0;
      com_lev := 0;
      ErrorMsg.lineNum := 1;
      Tokens.EOF(pos,pos)
  end
  
(* Function to handle \ddd - convert it to ASCII *)
fun convert_ddd str =
  let
     val str1 = String.substring(str,1,3)
     val int_value = valOf(Int.fromString(str1))
     val char_value = chr int_value
  in
     Char.toString char_value
  end

fun convert_control str =
	let
	    val ch = List.nth(explode str, 2)
        val intVal = (ord ch) -64
	in
	    implode [chr intVal]
	end 
	
%% 
ALPHA_DIGIT =[-a-zA-Z0-9\ :\.'\(\);];
NAME=[a-zA-Z][a-zA-Z0-9_]*;
INTEGER=[0-9]+;
ws=[\ \t];
%s STR_SEC COMMENT; 
%header  (functor TigerLexFun(structure Tokens: Tiger_TOKENS));

%%

<INITIAL>   "/*"   	=> (com_lev := 1; YYBEGIN COMMENT; continue());
<COMMENT>   "*/"  	=> (com_lev := !com_lev - 1; 
                            if(!com_lev = 0) then YYBEGIN INITIAL else ();  
                            continue());
<COMMENT>   "/*"  	=> (com_lev := !com_lev + 1; continue());
<COMMENT>   \n  	=> (lineNum := !lineNum+1; 
                            linePos := yypos :: !linePos;
                            continue());
<COMMENT>   .  		=> (continue());

<INITIAL>   "type" 	=> (Tokens.TYPE(yypos, yypos+4));
<INITIAL>   "var"  	=> (Tokens.VAR(yypos, yypos+3));
<INITIAL>   "function"  => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>   "break"  	=> (Tokens.BREAK(yypos, yypos+5));
<INITIAL>   "of"    	=> (Tokens.OF(yypos, yypos+2));
<INITIAL>   "end"       => (Tokens.END(yypos, yypos+3));
<INITIAL>   "in"        => (Tokens.IN(yypos, yypos+2));
<INITIAL>   "nil"       => (Tokens.NIL(yypos, yypos+3));
<INITIAL>   "let" 	=> (Tokens.LET(yypos, yypos+3));
<INITIAL>   "do"        => (Tokens.DO(yypos, yypos+2));
<INITIAL>   "to"        => (Tokens.TO(yypos, yypos+2)); 
<INITIAL>   "for"       => (Tokens.FOR(yypos, yypos+3));
<INITIAL>   "while"     => (Tokens.WHILE(yypos, yypos+5));
<INITIAL>   "else"      => (Tokens.ELSE(yypos, yypos+4));
<INITIAL>   "then"      => (Tokens.THEN(yypos, yypos+4));
<INITIAL>   "if"        => (Tokens.IF(yypos, yypos+2));
<INITIAL>   "array"     => (Tokens.ARRAY(yypos, yypos+5));

<INITIAL>   ":=" 	=> (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL>   "|" 	=> (Tokens.OR(yypos, yypos+1));
<INITIAL>   "&"    	=> (Tokens.AND(yypos, yypos+1));
<INITIAL>   ">="        => (Tokens.GE(yypos, yypos+2));
<INITIAL>   ">"    	=> (Tokens.GT(yypos, yypos+1));
<INITIAL>   "<="        => (Tokens.LE(yypos, yypos+2));
<INITIAL>   "<"    	=> (Tokens.LT(yypos, yypos+1));
<INITIAL>   "<>" 	=> (Tokens.NEQ(yypos, yypos+2));
<INITIAL>   "="  	=> (Tokens.EQ(yypos, yypos+1));
<INITIAL>   "/"  	=> (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL>   "*"  	=> (Tokens.TIMES(yypos, yypos+1));
<INITIAL>   "-"  	=> (Tokens.MINUS(yypos, yypos+1));
<INITIAL>   "+"  	=> (Tokens.PLUS(yypos, yypos+1));
<INITIAL>   "."  	=> (Tokens.DOT(yypos, yypos+1));
<INITIAL>   "{"  	=> (Tokens.LBRACE(yypos, yypos+1));
<INITIAL>   "}"  	=> (Tokens.RBRACE(yypos, yypos+1));
<INITIAL>   "["  	=> (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>   "]"  	=> (Tokens.RBRACK(yypos, yypos+1));
<INITIAL>   "("  	=> (Tokens.LPAREN(yypos, yypos+1));
<INITIAL>   ")"  	=> (Tokens.RPAREN(yypos, yypos+1));
<INITIAL>   ";"  	=> (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL>   ":"  	=> (Tokens.COLON(yypos, yypos+1));
<INITIAL>   ","  	=> (Tokens.COMMA(yypos, yypos+1));

<INITIAL>   {INTEGER} 	=> (Tokens.INT(valOf(Int.fromString yytext), yypos, yypos+size yytext)); 
<INITIAL>   {NAME}  	=> (Tokens.ID(yytext, yypos,yypos+size yytext));

<INITIAL>   \" 			=> (YYBEGIN STR_SEC;
                                    str_val:=""; 
                                    str_pos:=yypos;
                                    stringSection:=1;
                                    continue());
<STR_SEC>   {ALPHA_DIGIT}* 	=> (str_val:= !str_val ^ yytext; continue());
<STR_SEC>   \\[\t|\n|\ ]+\\ 	=> (continue());
<STR_SEC>   \\n 		=> (str_val:= !str_val ^ "\n"; continue());
<STR_SEC>   \\t 		=> (str_val:= !str_val ^ "\t"; continue());
<STR_SEC>   \\\" 		=> (str_val:= !str_val ^ "\""; continue());
<STR_SEC>   \\\\ 		=> (str_val:= !str_val ^ "\\"; continue());
<STR_SEC>   \\[0-9][0-9][0-9]   => (str_val:= !str_val ^ convert_ddd(yytext);
                                    continue());
<STR_SEC>   \\[\n\t \f]+\\      => (continue());
<STR_SEC>   \\\^[A-H]           => (str_val:= !str_val ^ convert_control(yytext);
                                     continue());
<STR_SEC>   \"       		=> (YYBEGIN INITIAL; 
                                    stringSection := 0;
                                    Tokens.STRING(!str_val, !str_pos, yypos+size (!str_val)));
<STR_SEC>   \\[^\n\t\ \\\\\"] 	=> (ErrorMsg.error yypos ("Invalid escape character: " ^ yytext); 
                                                           YYBEGIN INITIAL; 
                                                           Tokens.STRING(!str_val, !str_pos, yypos+size (!str_val)));
<STR_SEC>   \n 	        	=> (ErrorMsg.error yypos ("string ended with newline: " ^ !str_val); 
                                                           lineNum := !lineNum+1; 
                                                           linePos := yypos :: !linePos; 
                                                           YYBEGIN INITIAL; 
                                                           Tokens.STRING(!str_val, !str_pos, yypos+size (!str_val)));
<STR_SEC>   .                   => (ErrorMsg.error yypos ("illegal character in String" ^ yytext); continue());


<INITIAL>   \n	 	    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>   {ws}    	    => (continue());
<INITIAL>   .      	    => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
