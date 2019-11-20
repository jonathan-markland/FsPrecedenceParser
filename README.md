Top-down Operator Precedence Parser
===================================

*This is a gem of a find for Compiler writers!*

A reminder to myself on how to do this.
Implementation in F#.
The TEST framework kicks everything off here.

20/11/2019 - I'm cutting out the lexical analysis phase
since that's something everybody can do any number of ways!
I've not settled on an F# implementation for lexical
analysis at the time of writing.

I've also left a load of TODOs in the code because there is
a temporary implementation that uses mutability for the 
parser position when this could easily be a Functional
Programming-style "threaded" parameter.

Furthermore, there is some significant self-similar themes
in the code which are crying out for a generic implementation,
but I'm leaving this expanded out just until I'm confident
about reducing that.

I must .Net-Core this.