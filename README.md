# import.gen

Generate import calls.

Ever got bitten by conflicting symbols in the packages you attach via `library()`?
The [`import` package](https://github.com/smbache/import) offers a solution
by specifying exactly which symbols to load into a dedicated environment,
instead of attaching the whole package to the search path.
But what if you actually want all (or almost all) symbols from a package?


```r
import.gen::from(c("magrittr", "tidyr"), output = "cat")
```

```
## # The imports below were generated using the following call:
## # import.gen::from(pkgs = c("magrittr", "tidyr"))
## import::from("magrittr", "%<>%", "%>%", "%$%", "add", "and", 
##     "debug_fseq", "debug_pipe", "divide_by", "divide_by_int", 
##     "equals", "extract", "extract2", "freduce", "functions", 
##     "inset", "inset2", "is_greater_than", "is_in", "is_less_than", 
##     "is_weakly_greater_than", "is_weakly_less_than", "mod", "multiply_by", 
##     "multiply_by_matrix", "n'est pas", "not", "or", "raise_to_power", 
##     "set_attr", "set_attributes", "set_colnames", "set_names", 
##     "set_rownames", "subtract", "%T>%", "tamper", "undebug_fseq", 
##     "use_series")
## import::from("tidyr", "expand", "expand_", "extract", "extract_", 
##     "extract_numeric", "gather", "gather_", "separate", "separate_", 
##     "seq_range", "spread", "spread_", "unite", "unite_", "unnest", 
##     "unnest_")
```

By default, the generated code is copied to the clipboard
so that the developer can paste it at the appropriate place right away.

The package is at a very early stage but works as advertised above.
