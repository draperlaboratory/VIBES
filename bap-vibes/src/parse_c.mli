(**
    [c_patch_to_sexp_string] ingests C-like code and outputs a new string representing a
    vibes patch s-expression.
    - `int x, y, z;` at the top of your patch to convert to `(var-decls x y z)`
    -  Only if then elses of the form `if(cond_expr){goto l1;}else{goto l2;};` ->
        `(branch sexp_of_cond_expr (goto l1) (goto l2))`
    - `goto foo;` -> `(goto foo)`
    - `goto fallthrough;` -> `fallthrough`
    - Supports operators +, -, *,, /, = <<, ++, !=, <, >, <=, >=
    - Dereferencing `*a` -> `(load a)`
    - Array indexing `a[j]` -> `(load (+ a j))`
    - Constants like 0x7 are maintained as unadultered strings

    Example supported C:

    ```
    int x, y, z;
    x = 0x7;
    x = *y;
    if(x > 0){
        goto fred;
    } else{
        goto larry;
    }
    ```

    If FrontC fails to parse, it forwards the parse error. If FrontC parses but a construct
    is not supported, failwith is called and crashes the program.
*)
val c_patch_to_sexp_string : string -> (string, string) result