(*
  Should show 2 alerts, but shows only the last one ("222")
*)

structure MT = State.Trans(struct con m = Basis.transaction end)

fun main {} : transaction page = 
  s <- MT.eval {H = (return ())} (
      s1 <- MT.get {};
      MT.set { H = (s1.H; alert "111")};
      s2 <- MT.get {};
      MT.set { H = (s2.H; alert "222")};
      return {}
    );
  return
    <xml>
      <head>
      </head>
      <body onload={s.H}>
        Urweb-monad test
      </body>
    </xml>

