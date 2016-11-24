module ParserTests where


import Ast
import Parser 



-- Tests if a string with only a target is correctly processed
test1 = let 
        ans = parseString "hej :\n"  
        in 
        ans == Right [Rule [[Lt "hej"]] [] []]
 
-- Tests if %'s are processed correctly 
test2 = let 
        ans = parseString "%.o : %.s \n\t%.w\n"
        in
        ans == Right [Rule [[St, Lt ".o"]] [[St, Lt ".s"]] [[St, Lt ".w"]]]
 
-- Tests if several complicated rules are processed correctly 
test3 = let 
        ans = parseString "foo : foo.o bar.o\n\tcc -o foo foo.o bar.o\n%.o : %.c\n\tcc -S %.c\n\tas -o %.o %.s\n"
        in 
        ans == Right [Rule [[Lt "foo"]] [[Lt "foo.o"], [Lt "bar.o"]] [[Lt "cc -o foo foo.o bar.o"]],
                      Rule [[St, Lt ".o"]] [[St, Lt ".c"]] [[Lt "cc -S ", St, Lt ".c"], [Lt "as -o ", St, Lt ".o ", St, Lt ".s"]]]
 
 
-- Tests if two targets are handled correctly
test4 = let
        ans = parseString "foo bar:\n"
        in
        ans == Right [Rule [[Lt "foo"], [Lt "bar"]] [] []]
 
-- Tests if the safeguard against % in prereqs or commands only catches the error
test5 = let 
        ans = parseString "hej : %.o\n"
        in
        ans == Left "Error"

        
-- Tests if the safeguard against % in prereqs or commands only is caught 
test6 = let
        ans = parseString "hej : \n\t%.o\n"
        in
        ans == Left "Error"

-- Tests if a lacking \n in a rules is caught        
test7 = let 
        ans = parseString "hej : hej\n\tkage"
        in
        ans == Left "Error"

-- Tests if a simple string with 1 target, 1 prerequisite and no commands is processed correctly       
test8 = let 
        ans = parseString "   hej:hej\n"
        in
        ans == Right [Rule [[Lt "hej"]] [[Lt "hej"]] []]

-- Tests if \\\\ is handled correctly       
test9 = let
        ans = parseString "hej : h\\\\ej\n"
        in
        ans == Right [Rule [[Lt "hej"]] [[Lt "h\\ej"]] []]

-- Tests if a \\\n is caught correctly in the commands      
test10 = let
        ans = parseString "hej : \n\thello\\\nworld\n"
        in
        ans == Right [Rule [[Lt "hej"]] [] [[Lt "helloworld"]]]
        
-- Tests if a \\\\ is caught correctly in the commands
test11 = let 
         ans = parseString "hej : \n\thello\\\\world\n"
         in
         ans == Right [Rule [[Lt "hej"]] [] [[Lt "hello\\world"]]]

-- Tests if all possible characters are caught correctly in targets         
test12 = let
         ans = parseString "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_./-\\%\\:\\ \\\\:\n"
         in 
         ans == Right [Rule [[Lt "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_./-%: \\"]] [][]]