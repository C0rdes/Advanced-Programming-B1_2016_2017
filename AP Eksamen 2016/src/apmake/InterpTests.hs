module InterpTests where

import Ast
import Interp

-- Test if program makes a build plan without %'s.
test1 = let 
        a = Rule [[Lt "foo"]] [[Lt "foo.o"], [Lt "bar.o"]] [[Lt "cc -o foo foo.o bar.o"], [Lt "nm foo > foo.map"], [Lt "strip foo"]]
        b = Rule [[Lt "foo.o"]] [[Lt "foo.c"]] [[Lt "cc -c foo.c"]]
        c = Rule [[Lt "bar.o"]] [[Lt "bar.s"]] [[Lt "as -o bar.o bar.s"]]
        mf = [a,b,c]
        ret = build mf "foo" (`elem` ["foo.c", "bar.s"]) 2
        in
        ret == Just ["as -o bar.o bar.s", "cc -c foo.c", "cc -o foo foo.o bar.o", "nm foo > foo.map", "strip foo"]

-- tests if program catches wrong formatted makefile
test2 = let 
        a = Rule [[Lt "hej"]] [[St, Lt ".o"]] [[]]
        mf = [a]
        ret = build mf "hej" (`elem` ["hej.o"]) 5
        in
        ret == Nothing

-- Tests if the program correctly returns nothing if not enough steps are allowed        
test3 = let 
        a = Rule [[Lt "foo"]] [[Lt "foo.o"], [Lt "bar.o"]] [[Lt "cc -o foo foo.o bar.o"], [Lt "nm foo > foo.map"], [Lt "strip foo"]]
        b = Rule [[Lt "foo.o"]] [[Lt "foo.c"]] [[Lt "cc -c foo.c"]]
        c = Rule [[Lt "bar.o"]] [[Lt "bar.s"]] [[Lt "as -o bar.o bar.s"]]
        mf = [a,b,c]
        ret = build mf "foo" (`elem` ["foo.c", "bar.s"]) 0
        in
        ret == Nothing
 
-- tests if the program works correctly with %'s
test4 = let
        a = Rule [[St, Lt ".foo"], [St, Lt ".fum"]] [[St, Lt ".bar"]] [[Lt "a"]]
        b = Rule [[St, Lt ".bar"]] [[St, Lt ".foo"]] [[Lt "b"]]
        c = Rule [[Lt "foo.bar"]] [[Lt "blim.foo"]] [[Lt "c"]]
        d = Rule [[Lt "blim.foo"]] [[Lt "bil.foo"]] [[Lt "d"]]
        mf = [a,b,c,d]
        ret = build mf "foo.bar" (`elem` ["bil.foo"]) 6
        in
        ret == Just ["d", "b", "a", "c", "a", "b"]
 

-- tests if the program handles as specified in the assignment test example 
test5 = let 
        a = Rule [[Lt "foo"]] [[Lt "foo.o"], [Lt "bar.o"]] [[Lt "cc -o foo foo.o bar.o"], [Lt "nm foo > foo.map"], [Lt "strip foo"]]
        b = Rule [[Lt "foo.o"]] [[Lt "foo.c"]] [[Lt "cc -c foo.c"]]
        c = Rule [[Lt "bar.o"]] [[Lt "bar.s"]] [[Lt "as -o bar.o bar.s"]]
        mf = [a,b,c]
        ret = build mf "foo" (`elem` ["foo.c", "bar.s"]) 2
        in
        ret == Just ["cc -c foo.c", "as -o bar.o bar.s", "cc -o foo foo.o bar.o", "nm foo > foo.map", "strip foo"]

-- Tests if the program returns Nothing if there is a dependency cycle
test6 = let
        a = Rule [[St, Lt ".o"]] [[St, Lt ".s"]] []
        b = Rule [[St, Lt ".s"]] [[St, Lt ".o"]] []
        mf = [a,b]
        ret = build mf "foo.o" (`elem` []) 10
        in
        ret == Nothing
 