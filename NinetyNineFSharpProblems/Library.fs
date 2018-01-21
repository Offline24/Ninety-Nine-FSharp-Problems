module NinetyNineFSharpProblems

/// Problem 1 : Find the last element of a list
let rec myLast list =
    match list with
        | [] -> invalidArg "list" "list can not be empty"
        | [element] -> element
        | _ :: tail -> myLast tail

/// Problem 2 : Find the last but one element of a list
let rec myButLast list =
    match list with
        | [] | [_] -> invalidArg "list" "list must contain at least two elements"
        | [element ; _] -> element
        | _ :: tail -> myButLast tail

/// Problem 3 : Find the K'th element of a list. The first element in the list is number 1
let myHead list =
    match list with
        | [] -> invalidArg "list" "list can not be empty"
        | head :: _ -> head
let rec myElementAt index list =
    match index with
        | x when x < 1 -> invalidArg "index" "index can not be less than 1"
        | 1 -> myHead list
        | _ -> myElementAt (index-1) list.[1..]

/// Problem 4 : Find the number of elements of a list
let rec myMap mapFunc list =
    match list with
        | [] -> []
        | head :: tail -> [mapFunc head] @ myMap mapFunc tail
let rec mySum list =
    match list with
        | [] -> invalidArg "list" "list can not be empty"
        | [element] -> element
        | head :: tail -> head + mySum tail
let myLength list = list |> myMap (fun _ -> 1) |> mySum

/// Problem 5 : Reverse a list
let rec myReverse list =
    match list with
        | [] -> []
        | head :: tail -> myReverse tail @ [head]

/// Problem 6 : Find out whether a list is a palindrome
let myIsPalindrome list = list = myReverse list

/// Problem 7 : Flatten a nested list structure
type 'a NestedList = List of 'a NestedList list | Elem of 'a
let rec myReduceBack reduceFun list =
    match list with
        | [] -> invalidArg "list" "list can not be empty"
        | [element] -> element
        | head :: tail -> reduceFun head (myReduceBack reduceFun tail)
let rec myReduce reduceFun list =
    match list with
        | [] -> invalidArg "list" "list can not be empty"
        | [element] -> element
        | head :: (head2 :: tail) -> [reduceFun head head2] @ tail |> myReduce reduceFun
let rec myFlatten (list: 'a NestedList) =
    let rec flat list =
        match list with
            | [] -> []
            | head :: tail ->
                let firstElement = match head with
                                    | Elem(element) -> [element]
                                    | List(list) -> flat list
                firstElement @ flat tail
    flat [list]

/// Problem 8 : Eliminate consecutive duplicates of list elements
let myCompress list =
    let reduceFun left right =
        match (left,right) with
            | (x,y) when myLast x = myLast y -> x
            | (x,y) -> x @ y
    match list with
        | [] -> []
        | list -> list |> myMap (fun x -> [x]) |> myReduce reduceFun

/// Problem 9 : Pack consecutive duplicates of list elements into sublists
let rec myWithoutLast list =
    match list with
        | [] | [_] -> []
        | [element; _] -> [element]
        | head :: tail -> [head] @ myWithoutLast tail
let myPack list =
    let reduceFun left right =
        match (left,right) with
            | (x,y) when myLast (myLast x) = myLast (myLast y) -> myWithoutLast x @ [myLast x @ [myLast (myLast x)]]
            | (x,y) -> x @ y
    match list with
        | [] -> []
        | list -> list |> myMap (fun x -> [[x]]) |> myReduce reduceFun

/// Problem 10 : Run-length encoding of a list
let myEncode list = list |> myPack |> myMap (fun x -> (myLength x, myLast x))

/// Problem 11 : Modified run-length encoding
type 'a Encoding = Multiple of int * 'a | Single of 'a
let myEncodeModified list =
    let mapFun item =
        match item with
            | (1, x) -> Single x
            | _ -> Multiple item
    list |> myEncode |> myMap mapFun

/// Problem 12 : Decode a run-length encoded list
let myDuplicate times element = [1 .. times] |> myMap (fun _ -> element)
let rec myDecodeModified list =
    let decode item =
        match item with
            | Single(x) -> [x]
            | Multiple(items, element) -> element |> myDuplicate items
    match list with
        | [] -> []
        | head :: tail -> decode head @ myDecodeModified tail

/// Problem 14 : Duplicate the elements of a list
let rec myConcat list =
    match list with
        | [] -> []
        | head :: tail -> head @ myConcat tail
let myDupli list = list |> myMap (fun x -> [x;x]) |> myConcat

/// Problem 15 : Replicate the elements of a list a given number of times
let myRepli times list = list |> myMap (fun x -> x |> myDuplicate times) |> myConcat

/// Problem 16 : Drop every N'th element from a list
let myMapWithIndex mapFunc list =
    let rec internalMapWithIndex i mapFunc list =
        match list with
            | [] -> []
            | head :: tail -> [mapFunc i head] @ internalMapWithIndex (i+1) mapFunc tail
    internalMapWithIndex 0 mapFunc list
let myDropEvery time list =
    list
    |> myMap (fun x -> [x])
    |> myMapWithIndex (fun i x -> match (i + 1) % time with | 0 -> [] | _ -> x)
    |> myConcat
let myFilterWithIndex filterFunc list =
    let rec internalFilterWithIndex i filterFunc list =
        match list with
            | [] -> []
            | head :: tail -> (match filterFunc i head with | true -> [head] | false -> []) @ internalFilterWithIndex (i+1) filterFunc tail
    internalFilterWithIndex 0 filterFunc list
let myDropEvery2 time list = list |> myFilterWithIndex (fun i _ -> (i+1)%time <> 0)

/// Problem 17 : Split a list into two parts; the length of the first part is given
let mySplit firstListLength list =
    let firstList = list |> myFilterWithIndex (fun i _ -> i < firstListLength)
    let secondList = list |> myFilterWithIndex (fun i _ -> i >= firstListLength)
    (firstList, secondList)

/// Problem 18 : Extract a slice from a list
let mySlice indexFrom indexTo list = list |> myFilterWithIndex (fun i _ -> i+1 >= indexFrom && i+1 <= indexTo)

/// Problem 19 : Rotate a list N places to the left
let myRotate rotation list =
    let slicePosition = match rotation with | x when x >= 0 -> x | x -> myLength list + x
    let (left,right) = list |> mySplit slicePosition
    right @ left

/// Problem 20 : Remove the K'th element from a list
let myRemoveAt elementAt list = 
    let result = list |> myFilterWithIndex (fun i _ -> i <> elementAt)
    let removedElement = list |> myElementAt (elementAt+1)
    (removedElement, result)
