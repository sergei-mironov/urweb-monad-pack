(*
  Should show 2 alerts, but shows only the last one ("222")
*)

structure MT = State.Trans(struct con m = Basis.transaction end)

fun testtrans {} : transaction xbody =
  r <- MT.run {Val=0} (
      s1 <- return 1;
      s2 <- return (s1 + 1);
      x <- MT.get {};
      MT.set {Val=x.Val+1};
      MT.lift (debug "Hello, inner monad");
      return (s2+2)
    );
  let
    val (s,v) = r
  in
    return <xml>State {[s.Val]} Val {[v]}</xml>
  end


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

