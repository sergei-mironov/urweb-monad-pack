
structure MT = State.Trans(struct con m = Basis.transaction end)


fun test (a1:int) (a2:int) : transaction page = 
  s <- MT.eval {H = (return ()), B=<xml/>} (
      s1 <- MT.get {};
      MT.set {H = (s1.H; alert ("arg1: " ^ (show a1))), B=<xml/>};
      s2 <- MT.get {};
      MT.set {H = (s2.H; alert ("arg2: " ^ (show a2))), B=<xml/>};
      return {}
    );
  return
    <xml>
      <head>
      </head>
      <body onload={s.H}>
        {s.B}
        Should popup 2 alerts showing both test arguments {[a1]} and {[a2]}
      </body>
    </xml>

fun main {} = test 33 42
