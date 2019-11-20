module TestAssistance

open Parser
open ExpressionTree



/// Convert an expression tree to text format, intended for the test framework 'Fact's.
let rec ExpressionToText expression =
    let Unary nodeName expr = sprintf "%s(%s)" nodeName (expr |> ExpressionToText)
    let Binary nodeName expr1 expr2 = sprintf "%s(%s,%s)" nodeName (expr1 |> ExpressionToText) (expr2 |> ExpressionToText)
    match expression with
        | Constant c             -> c.ToString()
        | Negate expr            -> Unary "Negate" expr
        | Add (expr1,expr2)      -> Binary "Add" expr1 expr2
        | Multiply (expr1,expr2) -> Binary "Multiply" expr1 expr2
        | Comma (expr1,expr2)    -> Binary "Comma" expr1 expr2
        | FunctionCall (functionExpr, paramsExpr) -> Binary "FunctionCall" functionExpr paramsExpr



/// Assistance function for test 'Facts'.
/// Runs the input string against the TDOP parser-and-tree builder, serialises the output tree to text.
/// The result text is compared against the expectation text (from the calling test case).
let TreeIs expectation input =
    let actual = input |> TdopTranslatedExpression |> ExpressionToText
    if actual <> expectation then 
        failwith (sprintf "Input '%s' gave tree '%s' where '%s' expected." input actual expectation)



/// Assistance function for test 'Facts'.
/// Executes a function that we expect finish with an exception.
let ThrowsWithMessageContaining expectedExceptionTextPart functionUnderTest =  

    // TODO: There might be a way to do this function in the test library!

    let raiseFail =
        try
            functionUnderTest ()
            true
        with
            ex ->
                if ex.Message.Contains(expectedExceptionTextPart:string) then 
                    // Test passed.
                    false
                else
                    failwith ("The function under test threw an exception, but the message didn't contain the expected text.  The thrown exception message was: " + ex.Message)

    if raiseFail then
        failwith "The function under test did not throw any exception, where an exception was expected."



