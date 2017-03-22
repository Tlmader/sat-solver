# sat-solver
**Introduction to Artificial Intelligence Project 2**

This project contains a simple function that tests whether or not a given propositional logic expression (already in conjunctive normal form) can be satisfied. It returns `t` when the given CNF expression is satisfiable and `nil` when the expression is not satisfiable.

Click [here](https://goo.gl/mYqBde) to open sat-solver.lisp directly in Coding Ground.

The format for a CNF expression is as follows:

* The symbol t is a valid expression, meaning *true*.
* The symbol nil is a valid expression, meaning *false*.
* A propositional variable is any non-numeric symbol except for `t` and `nil`.
* A negation is a list starting with not followed by exactly 1 variable.
* A literal is a propositional variable or a negation.
* A single literal by itself is a valid expression.
* A conjunction is a list starting with and followed by 1 or more clauses. It is a valid expression.
* A disjunction is a list starting with or followed by 1 or more literals. It is a valid expression.

Example: `(and a (not b) (or a (not c)))`

The example above is a conjunction with 3 clauses. The first clause is a single positive propositional literal, a. The second clause is a single negative propositional literal meaning not b. The third clause is a disjunction of two literals: the positive literal a and the negative literal not c. This expression is satisfiable. If we set `a=t`, `b=nil`, then all clauses will be satisfied (note that `c` can be `t` or `nil`).

The function does not return which variables must have which values in order to satisfy the expression; it simply returns `t` if the expression is satisfiable and `nil` if the expression is not satisfiable.
