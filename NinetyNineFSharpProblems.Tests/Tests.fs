module Tests

open Expecto
open NinetyNineFSharpProblems

[<Tests>]
let tests =
  testList "Ninety-Nine F# Problems" [

    testList "Problem 1 : Find the last element of a list" [
      test "integers" {
        let result = myLast [1; 2; 3; 4]
        Expect.equal result 4 ""
      }
      test "chars" {
        let result = myLast ['x';'y';'z']
        Expect.equal result 'z' ""
      }
    ]

    testList "Problem 2 : Find the last but one element of a list" [
      test "integers" {
        let result = myButLast [1; 2; 3; 4]
        Expect.equal result 3 ""
      }
      test "chars" {
        let result = myButLast ['a'..'z']
        Expect.equal result 'y' ""
      }
    ]

    testList "Problem 3 : Find the K'th element of a list. The first element in the list is number 1" [
      test "integers" {
        let result = myElementAt 2 [1; 2; 3]
        Expect.equal result 2 ""
      }
      test "chars" {
        let result = myElementAt 5 (List.ofSeq "fsharp")
        Expect.equal result 'r' ""
      }
    ]

    testList "Problem 4 : Find the number of elements of a list" [
      test "integers" {
        let result = myLength [123; 456; 789]
        Expect.equal result 3 ""
      }
      test "chars" {
        let result =  myLength <| List.ofSeq "Hello, world!"
        Expect.equal result 13 ""
      }
    ]

    testList "Problem 5 : Reverse a list" [
      test "integers" {
        let result =  myReverse <| [1;2;3;4]
        Expect.equal result [4; 3; 2; 1] ""
      }
      test "chars" {
        let result = myReverse <| List.ofSeq ("A man, a plan, a canal, panama!")
        let expected : char list = ['!'; 'a'; 'm'; 'a'; 'n'; 'a'; 'p'; ' '; ','; 'l'; 'a'; 'n'; 'a'; 'c'; ' ';
           'a'; ' '; ','; 'n'; 'a'; 'l'; 'p'; ' '; 'a'; ' '; ','; 'n'; 'a'; 'm'; ' ';
           'A']
        Expect.equal result expected ""
      }
    ]

    testList "Problem 6 : Find out whether a list is a palindrome" [
      test "integers negative" {
        let result = myIsPalindrome [1;2;3]
        Expect.isFalse result ""
      }
      test "integers positive" {
        let result = myIsPalindrome [1;2;4;8;16;8;4;2;1]
        Expect.isTrue result ""
      }  
      test "chars positive" {
        let result = myIsPalindrome <| List.ofSeq "madamimadam"
        Expect.isTrue result ""
      }
    ]

    testList "Problem 7 : Flatten a nested list structure" [
      test "one element" {
        let result = myFlatten (Elem 5)
        Expect.equal result [5] ""
      }
      test "empty" {
        let result = myFlatten (List [] : int NestedList)
        Expect.equal result [] ""
      }
      test "test" {
        let result = myFlatten (List [Elem 1; List [Elem 2; List [Elem 3; Elem 4]; Elem 5]])
        Expect.equal result [1;2;3;4;5] ""
      }
    ]

    testList "Problem 8 : Eliminate consecutive duplicates of list elements" [
      test "integers" {
        let result = myCompress [1;2;2;2;3;2;3;3]
        Expect.equal result [1;2;3;2;3] ""
      }
      test "chars" {
        let result = myCompress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
        Expect.equal result ["a";"b";"c";"a";"d";"e"] ""
      }
      test "empty" {
        let result = myCompress []
        Expect.equal result [] ""
      }
    ]

    testList "Problem 9 : Pack consecutive duplicates of list elements into sublists" [
      test "integers" {
        let result = myPack [1;1]
        let expectedResult = [[1;1]]
        Expect.equal result expectedResult ""
      }
      test "chars" {
        let result = myPack ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']
        let expectedResult = [['a'; 'a'; 'a'; 'a']; ['b']; ['c'; 'c']; ['a'; 'a']; ['d']; ['e'; 'e'; 'e'; 'e']]
        Expect.equal result expectedResult ""
      }
      test "empty" {
        let result = myPack []
        Expect.equal result [] ""
      }
    ]

    testList "Problem 10 : Run-length encoding of a list" [
      test "chars" {
        let result = myEncode <| List.ofSeq "aaaabccaadeeee"
        let expectedResult = [(4,'a');(1,'b');(2,'c');(2,'a');(1,'d');(4,'e')]
        Expect.equal result expectedResult ""
      }
    ]

    testList "Problem 11 : Modified run-length encoding" [
      test "chars" {
        let result = myEncodeModified <| List.ofSeq "aaaabccaadeeee"
        let expectedResult = [Multiple (4,'a'); Single 'b'; Multiple (2,'c'); Multiple (2,'a'); Single 'd'; Multiple (4,'e')]
        Expect.equal result expectedResult ""
      }
    ]

    testList "Problem 12 : Decode a run-length encoded list" [
      test "chars" {
        let result = myDecodeModified [Multiple (4,'a');Single 'b';Multiple (2,'c'); Multiple (2,'a');Single 'd';Multiple (4,'e')]
        let expectedResult = ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']
        Expect.equal result expectedResult ""
      }
    ]

    testList "Problem 14 : Duplicate the elements of a list" [
      test "integers" {
        let result = myDupli [1; 2; 3]
        Expect.equal result [1;1;2;2;3;3] ""
      }
    ]

    testList "Problem 15 : Replicate the elements of a list a given number of times" [
      test "chars" {
        let result = myRepli 3 (List.ofSeq "abc")
        Expect.equal result ['a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'c'; 'c'] ""
      }
    ]

    testList "Problem 16 : Drop every N'th element from a list" [
      test "chars ver. 1" {
        let result = myDropEvery 3 (List.ofSeq "abcdefghik")
        Expect.equal result  ['a'; 'b'; 'd'; 'e'; 'g'; 'h'; 'k'] ""
      }
      test "chars ver. 2" {
        let result = myDropEvery2 3 (List.ofSeq "abcdefghik")
        Expect.equal result  ['a'; 'b'; 'd'; 'e'; 'g'; 'h'; 'k'] ""
      }
    ]

    testList "Problem 17 : Split a list into two parts; the length of the first part is given" [
      test "chars" {
        let result = mySplit 3 (List.ofSeq "abcdefghik") 
        Expect.equal result (['a'; 'b'; 'c'], ['d'; 'e'; 'f'; 'g'; 'h'; 'i'; 'k']) ""
      }
    ]

    testList "Problem 18 : Extract a slice from a list" [
      test "chars" {
        let result = mySlice 3 7 ['a';'b';'c';'d';'e';'f';'g';'h';'i';'k']
        Expect.equal result ['c'; 'd'; 'e'; 'f'; 'g'] ""
      }
    ]

    testList "Problem 19 : Rotate a list N places to the left" [
      test "Forward" {
        let result = myRotate 3 ['a';'b';'c';'d';'e';'f';'g';'h']
        Expect.equal result ['d'; 'e'; 'f'; 'g'; 'h'; 'a'; 'b'; 'c'] ""
      }
      test "Backwards" {
        let result = myRotate -2 ['a';'b';'c';'d';'e';'f';'g';'h']
        Expect.equal result ['g'; 'h'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'] ""
      }
    ]

    testList "Problem 20 : Remove the K'th element from a list" [
      test "chars" {
        let result = myRemoveAt 1 <| List.ofSeq "abcd"
        Expect.equal result ('b', ['a'; 'c'; 'd']) ""
      }
    ]
  ]