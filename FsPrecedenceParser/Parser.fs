module Parser

    // Top-Down Operator Precedence parser

    // TODO: Aim to explain:
    // ---------------------
    // Why TDOP?  Ease of changing precedence enumeration.  No repeated grammar.
    // How to do literals
    // How to do prefix operators
    // Two infix operators:  1+2*3  1*2+3
    // How to select Left / right associativity for eg: 1+2+3
    // Comma separated lists are easy if comma is seen as an infix operator.
    // The subtlety that the infix phase does NOT eagerly raise an error if it doesn't see a recognised operator.
    // How to do a postfix operator
    // How to do parenthesis: the '(' is a prefix operator!  But we must make sure that ')' is NEVER part of the prefix or infix sets, and we must parse it explicitly in the open-bracket handler.
    // How to do a C language function call.
    // How to to C ternary operator


open System
open ExpressionTree



type Precedence = 
    | PrOuter 
    | PrComma 
    | PrAdd 
    | PrMultiply 
    | PrNegate 
    | PrFunctionCall



let CharAt index (str:string) =
    if index < str.Length then str.[index] else '\x00'  // If only .Net had NUL terminated strings!  Hint: NUL is never part of a lexeme character lookahead set.



/// Top-Down Operator Precedence parser
let TdopTranslatedExpression (sourceText:string) =

    // TODO: Hackishly, the sourceText is one-character-per-lexeme (for this demo),
    //       so no true lexical analysis done.  How to do lexical scanning is NOT part
    //       of this program because so many people cover that.

    // TODO: The Parser Actions (tree building in this case) are manually inlined into
    //       this example.  Actions could be separated, of course, in which case this
    //       whole thing could work like "List.fold" by threading a user-defined 
    //       accumulator type, rather than tree-builing.  This would then be a generic
    //       function.

    let mutable position = 0   // TODO: Remove mutable by threading the position parameter.


    let Expect ch =
        if (sourceText |> CharAt position) <> ch then
            failwith (sprintf "Error at position %d: Expected '%s'." position (ch.ToString()))
        position <- position + 1  // TODO:  When eliminating mutability, just return the updated position.


    let rec expressionAtLevel precedence =   // TODO: Threading the 'position' requires position as an input for these recursive routines.

        literalsAndPrefixOperators precedence

    and literalsAndPrefixOperators precedence =

        let ch = sourceText |> CharAt position   // TODO: Replace 'hack' CharAt and (ch = 'x') expressions with lexical analysis calls.

        // TODO: The following cases are crying out for a function to handle them.
        // TODO: A common theme in the following is an ExpressionTree node constructor function that relates to the lexeme in question.
        // TODO: If we generalise the case handling, the final case has additional custom parsing of the closing ')'.

        if Char.IsDigit(ch) then
            position <- position + 1
            let newTree = Constant ch
            infixAndPostfixOperators newTree precedence

        else if ch = '-' then
            position <- position + 1
            let newTree = Negate (expressionAtLevel PrNegate)
            infixAndPostfixOperators newTree precedence

        else if ch = '(' then
            position <- position + 1
            let newTree = expressionAtLevel PrOuter
            let result = infixAndPostfixOperators newTree precedence
            Expect ')'
            result
        
        else
            failwith (sprintf "Error at position %d: Unrecognised '%s'." position (ch.ToString()))  // TODO: Eliminate exception throwing in favour of a Result type.

    and infixAndPostfixOperators leftTree precedence =

        let ch = sourceText |> CharAt position

        // TODO: Like the prefix section there are clear generalisations that can be made here,
        //       except we would have special additional parsing for C language "? :" -- which we should support.

        if (ch = '+') && (PrAdd >= precedence) then
            position <- position + 1
            let rightTree = expressionAtLevel PrAdd
            let newTree = Add(leftTree, rightTree)
            infixAndPostfixOperators newTree precedence

        else if (ch = '*') && (PrMultiply >= precedence) then
            position <- position + 1
            let rightTree = expressionAtLevel PrMultiply
            let newTree = Multiply(leftTree, rightTree)
            infixAndPostfixOperators newTree precedence

        else if (ch = ',') && (PrComma >= precedence) then
            position <- position + 1
            let rightTree = expressionAtLevel PrComma
            let newTree = Comma(leftTree, rightTree)
            infixAndPostfixOperators newTree precedence

        else if (ch = '(') && (PrFunctionCall >= precedence) then
            position <- position + 1
            let rightTree = expressionAtLevel PrOuter
            let newTree = FunctionCall(leftTree, rightTree)
            Expect ')'
            infixAndPostfixOperators newTree precedence
            
        else
            leftTree  // It is NOT an error to fail to see an infix operator in the look-ahead.  They are optional!


    expressionAtLevel PrOuter


