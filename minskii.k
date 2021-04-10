module MINSKII-SYNTAX
    imports ID
    imports DOMAINS-SYNTAX

    //SHAPE SYNTAX

    syntax PtC ::= "(" Int "," Int ")"
                

    syntax Pt ::= PtC
                | "(" AExp "," AExp ")"   [seqstrict]

    
    syntax Shp ::= Pt
                 | Pt "->" Shp    [seqstrict]
                 | "(" Shp ")"    [strict]

    syntax Shps ::= List{Shp, ","}

    syntax ShpExp ::= Shp | Id
                    | "concat" "(" ShpExp "," ShpExp ")"         [strict]
                    | "splice" "(" ShpExp "," Int "," Int ")"    [strict]
                    | "insert" "(" ShpExp "," Pt "," Int ")"     [strict]
                    | "delete" "(" ShpExp "," Int ")"            [strict]
                    | "(" ShpExp ")"                            [bracket]
    
    //NON-SHAPE SYNTAX

    syntax Id ::= "main" [token]

    syntax AExp  ::= Int | Id
                   | "(" AExp ")"    [bracket]
                   | AExp "*" AExp   [seqstrict]
                   > AExp "/" AExp   [seqstrict]
                   > AExp "+" AExp   [seqstrict]
                   > AExp "-" AExp   [seqstrict]
                   > "-" AExp        [strict]

    syntax BExp  ::= Bool | Id
                   | Exp "==" Exp    [seqstrict, non-assoc]
                   | Exp "!=" Exp    [seqstrict, non-assoc]
                   | Exp "<=" Exp    [seqstrict, non-assoc]
                   | Exp "<" Exp     [seqstrict, non-assoc]
                   | Exp ">=" Exp    [seqstrict, non-assoc]
                   | Exp ">" Exp     [seqstrict, non-assoc]
                   | "!" Exp         [strict]
                   | Exp "&&" Exp    [strict(1), left]
                   | Exp "||" Exp    [seqstrict]

    syntax Exp ::= AExp | BExp | ShpExp | String | Id
                 | "(" Exp ")"                 [bracket]
                 | "++" Exp                    [strict]
                 > Exp "[" Exps "]"            [strict]
                 > Exp "(" Exps ")"            [strict]
                 | "sizeOf" "(" Exp ")"        [strict]
                 | "len" "(" Exp ")"           [strict]
                 > Exp "=" Exp                 [strict(2), right]

    //LISTS SYNTAX

    syntax Bottom
    syntax Bottoms ::= List{Bottom,","}

    syntax Ints ::= Bottoms | List{Int, ","}
    syntax Ids  ::= Bottoms | List{Id,","}
    syntax Exps ::= List{Exp,","}              [strict]
    syntax Val
    syntax Vals ::= List{Val,","}

    //BLOCK SYNTAX

    syntax Block ::= "{" "}"
                   | "{" Stmt "}"

    syntax Stmt  ::= Block
                   | "var" Exps ";"
                   | "lnlow" "(" Exps ")" ";"
                   | "lnhigh" "(" Exps ")" ";" 
                   | "ln" "(" Exps ")" ";"
                   | Exp ";"                                 [strict]
                   | "if" "(" BExp ")" Block                 [strict(1)]
                   | "if" "(" BExp ")" Block "else" Block    [avoid, strict(1)]
                   | "while" "(" BExp ")" Block
                   | "for" "(" Stmt BExp ";" Exp ")" Block   [seqstrict]
                   | "def" Id "(" Ids ")" Block
                   | "draw" "(" ShpExp "," Id ")" ";"        [strict]
                   | "print" "(" Exps ")" ";"                [strict]
                   | "background" "(" Exps ")" ";"           [strict]
                   | "display" "(" Exp ")" ";"               [strict]
                   | "return" Exp ";"                        [strict]
                   | "return" ";"
                   > Stmt Stmt                               [left]

endmodule

module MINSKII
    imports MINSKII-SYNTAX
    imports DOMAINS

    syntax KResult ::= Val | Vals

    syntax Val ::= Int | Bool | String | Shp | PtC
                 | lambda(Ids, Stmt)
                 | array(Int, Int)

    syntax Exp ::= Val
    syntax Vals ::= Bottoms | Ints
    syntax KResult ::= Val | Vals
    syntax Exps ::= Vals | Ids | Ints

    syntax ControlCell
    syntax ControlCellFragment

    configuration <T color="yellow">
                        <k color="green"> $PGM:Stmt ~> execute </k>
                        <env color="red"> .Map </env>
                        <store color="blue"> .Map </store>
                        <control>
                            <fstack> .List </fstack>
                        </control>
                        <genv color="pink"> .Map </genv>
                        <nextLoc color="gray"> 0 </nextLoc>
                        <output color="brown" stream="stdout"> .List </output>
                    </T>

    //DRAWING SEMANTICS

    
    //Shape Length

    /*
    rule len(_:Pt) => 1
    rule len((_:Pt -> S:Shp)) => 1 + len(S)

    //Shape functions
    syntax Id ::= "$S" | "$SA" | "$SB"

    rule var Z = concat(S1:Shp, S2:Shp); => Z = S1 -> S2;

    rule var Z = splice(S:Shp, B:Int, A:Int); => {
        var $SB = beforePos(S, B);
        var $SA = afterPos(S, A);

        Z = $SB -> $SA;
    }

    rule var Z = insert(S:Shp, P:Pt, I:Int); => {
        var $SB = beforePos(S, I);
        var $SA = afterPos(S, I);

        Z = $SB -> P;
        Z = Z -> $SA;
    }

    rule var Z = delete(S:Shp, I:Int); => {
        var $SB = beforePos(S, I);
        var $SA = afterPos(S, I);

        Z = $SB -> $SA;
    }

    //Shape functions auxilliaries
    syntax Shp ::= "beforePos" "(" ShpExp "," AExp ")"
    syntax Shp ::= "afterPos" "(" ShpExp "," AExp ")"

    rule beforePos((P:Pt -> _:Shp), 2) => P
    rule beforePos((P:Pt -> S:Shp), I) => P -> beforePos(S, I - 1)

    rule afterPos((S:Shp -> P:Pt), len(S:Shp)) => P
    rule afterPos((S:Shp -> P:Pt), I) => afterPos(S, I + 1) -> P

    */

    //shape drawing
    syntax Id ::= "$dx" | "$dy" | "$x" | "$y" | "$p" | "$slope" | "$i" | "$q"

    rule draw((Y:AExp, X:AExp), C:Id); => C[X][Y] = "+";
    rule draw((Y1:AExp, X1:AExp) -> (Y2:AExp, X2:AExp), C:Id); => {
        if((Y2 == Y1) && (X2 == X1)) {
            ln(X1, Y1, X2, Y2, "+", C);
        } else {
            if(Y2 == Y1) {
                ln(X1, Y1, X2, Y2, "|", C);
            } else {
                if(X2 == X1) {
                    ln(X1, Y1, X2, Y2, "-", C);
                } else {
                    var $x = X2 - X1;
                    var $y = Y2 - Y1;
                    var $slope = ($x / $y) * 100;

                    if((($x > 0) && ($y > 0)) || (($x < 0) && ($y < 0))) {
                        ln(X1, Y1, X2, Y2, "\\", C);
                    } else {
                        ln(X1, Y1, X2, Y2, "/", C);
                    }
                }
            }
        }
    }

    rule draw(P1:Pt -> P2:Pt -> S:Shp, C:Id); => {
        draw(P1 -> P2, C);
        draw(P2 -> S, C);
    }

    //Bresenham's line algorithm

    rule lnlow(X:AExp, Y:AExp, Z:AExp, T:AExp, S:String, C:Id); => {
        var $dx, $dy, $i, $y, $p, $q;    

        $dx = Z - X;
        $dy = T - Y;
        $y = Y;
        $i = 1;
        
        if($dy < 0) {
            $i = -1;
            $dy = -$dy;
        }
        $p = 2 * $dy - $dx;

        for(var $x = X; $x <= Z; ++$x) {
            C[$x][$y] = S;
            if($p > 0) {
                $y = $y + $i;
                $q = $dy - $dx;
                $p = $p + 2*$q;
            } else {
                $p = $p + 2*$dy;
            }
        } 
    }

    rule lnhigh(X:AExp, Y:AExp, Z:AExp, T:AExp, S:String, C:Id); => {
        var $dx, $dy, $i, $x, $p, $q;    

        $dx = Z - X;
        $dy = T - Y;
        $x = X;
        $i = 1;

        if($dx < 0) {
            $i = -1;
            $dx = -$dx;
        }
        $p = 2 * $dx - $dy;

        for(var $y = Y; $y <= T; ++$y) {
            C[$x][$y] = S;
            if($p > 0) {
                $x = $x + $i;
                $q = $dx - $dy;
                $p = $p + (2*$q);
            } else {
                $p = $p + 2*$dx;
            }
        }
    }

    rule ln(X:AExp, Y:AExp, Z:AExp, T:AExp, S:String, C:Id); => {
        var $dy, $dx;

        if((T - Y) < 0) {
            $dy = Y - T;
        } else {
            $dy = T - Y;
        }
        
        if((Z - X) < 0) {
            $dx = X - Z;
        } else {
            $dx = Z - X;
        }

        if($dy < $dx) {
            if(X > Z) {
                lnlow(Z, T, X, Y, S, C);
            } else {
                lnlow(X, Y, Z, T, S, C);
            }
        } else {
            if(Y > T) {
                lnhigh(Z, T, X, Y, S, C);
            } else {
                lnhigh(X, Y, Z, T, S, C);
            }
        }
    }

    //ARITHMETIC SEMANTICS

    rule I1 / I2 => I1 /Int I2  requires I2 =/=Int 0
    rule I1 + I2 => I1 +Int I2
    rule I1 * I2 => I1 *Int I2
    rule I1 - I2 => I1 -Int I2
    rule -I => 0 -Int I

    rule I1 <= I2 => I1 <=Int I2
    rule I1 < I2 => I1 <Int I2
    rule I1 >= I2 => I1 >=Int I2
    rule I1 > I2 => I1 >Int I2

    rule ! B => notBool B
    rule true && B => B
    rule false && _ => false
    rule true || _ => true
    rule false || B => B

    rule I1:Exp == I2:Exp => I1 ==K I2
    rule I1:Exp != I2:Exp => I1 =/=K I2

    //BLOCKS & STATEMENTS

    rule {} => .                               [structural]
    rule <k> {S} => S ~> setEnv(Env) ...</k>
         <env> Env </env>                      [structural]

    rule S1:Stmt S2:Stmt => S1 ~> S2                             [structural]
    rule if (true)  S else _ => S                                
    rule if (false) _ else S => S          
    rule if (CND) S => if (CND) S else {}                        [macro]                      
    rule while (CND) S => if (CND) {S while (CND) S} else {}     [structural]
    rule for (STR CND; STP) {S} => {STR while (CND) {S STP;}}    [macro]
    rule for (STR CND; STP) {} => {STR while (CND) {STP;}}       [macro]
    rule var E1:Exp, E2:Exp, Es:Exps; => var E1; var E2, Es;     [macro-rec]

    //ASSIGNMENTS & LOOKUP

    syntax KItem ::= "undefined"

    rule <k> var X:Id; => . ...</k>
         <env> Env => Env[X <- L] </env>
         <store>... .Map => L |-> undefined ...</store>
         <nextLoc> L => L +Int 1 </nextLoc>

    rule <k> X:Id => V ...</k>
         <env>... X |-> L ...</env>
         <store>... L |-> V:Val ...</store>   [lookup]

    context (HOLE => lvalue(HOLE)) = _
    rule <k> loc(L) = V:Val => V ...</k>
         <store>... L |-> (_ => V) ...</store>   [assignment]

    rule var X:Id = I; => var X; X = I;

    //ARRAYS

    rule <k> var X:Id[N:Int]; => . ...</k>
         <env> Env => Env[X <- L] </env>
         <store>... .Map => L |-> array(L +Int 1, N)
                            (L +Int 1) ... (L +Int N) |-> undefined ...</store>
         <nextLoc> L => L +Int 1 +Int N </nextLoc>
       requires N >=Int 0

    syntax Id ::= "$1" | "$2"
    rule var X:Id[N1:Int, N2:Int, Vs:Vals]; => var X[N1];
         {
            for(var $1 = 0; $1 <= N1 - 1; ++$1) {
               var $2[N2, Vs];
               X[$1] = $2;
            }
         } [structural]

    //One line Matrix output
    syntax Id ::= "$l1" | "$l2" | "$k" | "$l"
    rule display(C); => {
         var $l1 = sizeOf(C);
         var $l2 = sizeOf(C[0]);
         for(var $k = 0; $k < $l1; ++$k) {
            for(var $l = 0; $l < $l2; ++$l) {
                print(C[$k][$l]);
            }
            print("\n");
         }
    } 

    rule background(C, S); => {
         var $l1 = sizeOf(C);
         var $l2 = sizeOf(C[0]);
         for(var $k = 0; $k < $l1; ++$k) {
            for(var $l = 0; $l < $l2; ++$l) {
                C[$k][$l] = S;
            }
         }
    } 
    
    rule V:Val[N1:Exp, N2:Exp] => V[N1][N2]   [structural, anywhere]

    rule array(L, _)[N:Int] => lookup(L +Int N) [structural, anywhere]
    rule sizeOf(array(_, N)) => N

    //FUNCTIONS - only declarable at the top (global env)

    rule <k> def F(Xs) S => . ... </k>
         <env> Env => Env[F <- L] </env>
         <store>... .Map => L |-> lambda(Xs, S) ...</store>
         <nextLoc> L => L +Int 1 </nextLoc>

    syntax KItem ::= (Map,K,ControlCellFragment)

    rule <k> lambda(Xs,S)(Vs:Vals) ~> K => mkDecls(Xs,Vs) S return; </k>
         <control>
             <fstack> .List => ListItem((Env,K,C)) ...</fstack>
             C
         </control>
         <env> Env => GEnv </env>
         <genv> GEnv </genv>

    rule <k> return(V:Val); ~> _ => V ~> K </k>
         <control>
             <fstack> ListItem((Env,K,C)) => .List ...</fstack>
             (_ => C)
         </control>
         <env> _ => Env </env>

    syntax Val ::= "null"
    rule return; => return null;   [macro]

    //AUX

    //Execution
    syntax KItem ::= "execute"
    rule <k> execute => main(.Exps); </k>
         <env> Env </env>
         <genv> .Map => Env </genv>  [structural]

    //printing
    rule <k> print(V:Val, Es => Es); ...</k>
         <output>... .List => ListItem(V) </output>   [print]
    rule print(.Vals); => .                           [structural]

    //Lookup by location
    syntax Exp ::= lookup(Int)
    rule <k> lookup(L) => V ...</k>
         <store>... L |-> V:Val ...</store>   [lookup]

    //Lvalue and Loc
    syntax Exp ::= "lvalue" "(" K ")"
    syntax Val ::= "loc" "(" Int ")"

    rule <k> lvalue(X:Id => loc(L)) ...</k>
         <env>... X |-> L:Int ...</env>       [structural]

    context lvalue(_::Exp[HOLE::Exps])
    context lvalue(HOLE::Exp[_::Exps])

    rule lvalue(lookup(L:Int) => loc(L))  [structural]

    //Variable Increment
    context ++(HOLE => lvalue(HOLE))
    rule <k> ++loc(L) => I +Int 1 ...</k>
         <store>... L |-> (I => I +Int 1) ...</store>  [increment]

    //Environment
    syntax KItem ::= setEnv(Map)
    rule <k> setEnv(Env) => . ...</k> 
         <env> _ => Env </env>           [structural]
    
    rule (setEnv(_) => .) ~> setEnv(_)   [structural]

    rule _:Val; => .

    //Declaring multiple positions
    syntax Map ::= Int "..." Int "|->" K   [function]
    rule N...M |-> _ => .Map  requires N >Int M
    rule N...M |-> K => N |-> K (N +Int 1)...M |-> K  requires N <=Int M

    //MkDecls
    syntax Stmt ::= mkDecls(Ids,Vals)   [function]
    rule mkDecls((X:Id, Xs:Ids), (V:Val, Vs:Vals)) => var X = V; mkDecls(Xs, Vs)
    rule mkDecls(.Ids, .Vals) => {}

endmodule
