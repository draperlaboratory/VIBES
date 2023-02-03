# vibes-as

This command line tool takes VIBES IR and a MiniZinc model as input.
It runs MiniZinc to solve for register allocation and scheduling.
Afterwards, it runs some peephole optimization passes on the VIBES IR.
Finally, it outputs a serialized assembly program.
