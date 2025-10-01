type expression =
Var of Id.t
| Int of int
| Add of expression * expression
| Fun of boundN
| App of expression * expression list

and boundN = { bound : Id.t list;
               body : expression; }
