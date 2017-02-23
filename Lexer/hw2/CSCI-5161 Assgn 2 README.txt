Assignment-2 
Name - Varun Vijaya Kumar 
Student ID - 5296193

1.  Handling Comments - separate state to handle comments, everything between /* and */ are droppped, but for \n we have to increment the line number.
2.  Handling Strings - This was a complicated. All string handling is done in the state STR_SEC, where all aphanumericals are added to the variable str_val. Escape characters are handled 
appropriately, all other escape characters are rejected and error is thrown the screen. Multi line comments are also handled. Control characters are printed as it is. 
3.  Error handling - all unknown characters are handled in the error case, where it prints the line number and yypos. Thus it is important to hold the line number for every new line. 
4.  EOF is handled by the funtion eof() in the tiger.lex file, which calls Tokens.EOF();
5.  Multi line commenting is handled.  Escape characters are handled. All keywords and ID are handled. Error is printed appropriately.