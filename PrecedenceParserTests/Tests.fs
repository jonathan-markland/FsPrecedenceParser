module Tests

open Xunit
open TestAssistance

    // TODO: 1. It's not part of the test that ALL the input is consumed.
    // TODO: 2. If we do (1) then we should also test that unrecognised content
    //          is thrown back for the caller by the INFIX grammar.

[<Fact>]
let ``The empty-expression is NOT allowed.`` () =
    let underTest () = "" |> Parser.TdopTranslatedExpression |> ignore
    underTest |> ThrowsWithMessageContaining "Error at position 0:"

[<Fact>]
let ``Integer literal parses OK`` () =
    "1" |> TreeIs "1"

[<Fact>]
let ``Redundantly parenthesized integer literal parses OK`` () =
    "(1)" |> TreeIs "1"

[<Fact>]
let ``Minus One uses negation operator in this implementation`` () =
    "-1" |> TreeIs "Negate(1)"

[<Fact>]
let ``Redundantly parenthesized negation`` () =
    "(-1)" |> TreeIs "Negate(1)"

[<Fact>]
let ``One plus three`` () =
    "1+3" |> TreeIs "Add(1,3)"

[<Fact>]
let ``Six times four`` () =
    "6*4" |> TreeIs "Multiply(6,4)"

[<Fact>]
let ``Associativity of add`` () =
    "1+2+3" |> TreeIs "Add(1,Add(2,3))"

[<Fact>]
let ``Associativity of multiplication`` () =
    "4*6*8" |> TreeIs "Multiply(4,Multiply(6,8))"

[<Fact>]
let ``Add is lower precedence case 1`` () =
    "1+4*6*8" |> TreeIs "Add(1,Multiply(4,Multiply(6,8)))"

[<Fact>]
let ``Add is lower precedence case 2`` () =
    "4*6*8+1" |> TreeIs "Add(Multiply(4,Multiply(6,8)),1)"

[<Fact>]
let ``Minus one times five plus three`` () =
    "-1*5+3" |> TreeIs "Add(Multiply(Negate(1),5),3)"

[<Fact>]
let ``Parenthesis override precedence`` () =
    "-1*(5+3)" |> TreeIs "Multiply(Negate(1),Add(5,3))"

[<Fact>]
let ``Minus one plus five times three`` () =
    "-1+5*3" |> TreeIs "Add(Negate(1),Multiply(5,3))"

[<Fact>]
let ``Minus one plus minus five times three`` () =
    "-1+-5*3" |> TreeIs "Add(Negate(1),Multiply(Negate(5),3))"

[<Fact>]
let ``Minus one plus negation of all of five times three`` () =
    "-1+-(5*3)" |> TreeIs "Add(Negate(1),Negate(Multiply(5,3)))"

[<Fact>]
let ``Unnecessary parenthesation has no adverse effect`` () =
    "-1+(5*3)" |> TreeIs "Add(Negate(1),Multiply(5,3))"

[<Fact>]
let ``The unknown prefix operator`` () =
    let underTest () = "¬1" |> Parser.TdopTranslatedExpression |> ignore
    underTest |> ThrowsWithMessageContaining "Error at position 0:"

[<Fact>]
let ``An unknown infix operator is assumed to terminate the expression, and is the callers responsibility`` () =
    "1¬2" |> TreeIs "1"

[<Fact>]
let ``Function call`` () =
    "1(2)" |> TreeIs "FunctionCall(1,2)"

[<Fact>]
let ``Comma separated list of 2`` () =
    "1(2,3)" |> TreeIs "FunctionCall(1,Comma(2,3))"

[<Fact>]
let ``Comma separated list of three`` () =
    "1(2,3,4)" |> TreeIs "FunctionCall(1,Comma(2,Comma(3,4)))"

[<Fact>]
let ``Negate function call result`` () =
    "-1(2,3,4)" |> TreeIs "Negate(FunctionCall(1,Comma(2,Comma(3,4))))"

[<Fact>]
let ``Function call with negated parameter`` () =
    "1(2,-3,4)" |> TreeIs "FunctionCall(1,Comma(2,Comma(Negate(3),4)))"

[<Fact>]
let ``Function call with calculation parameter`` () =
    "1(2+3,4*5,6)" |> TreeIs "FunctionCall(1,Comma(Add(2,3),Comma(Multiply(4,5),6)))"
