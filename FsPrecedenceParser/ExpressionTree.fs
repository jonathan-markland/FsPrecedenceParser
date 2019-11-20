module ExpressionTree


/// Expression tree node
type ExpressionTree =
    | Constant of char // TODO: fudge
    | Negate   of ExpressionTree
    | Add      of ExpressionTree * ExpressionTree
    | Multiply of ExpressionTree * ExpressionTree
    | Comma    of ExpressionTree * ExpressionTree
    | FunctionCall of funcExpr:ExpressionTree * paramsExpr:ExpressionTree


