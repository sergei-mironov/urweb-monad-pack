
structure MT = State.Trans(struct con m = Basis.transaction end)
structure MO = State.Trans(struct con m = Basis.option end)

fun testopt (b:bool) : transaction xbody =
  let
    val o = MO.run {Val=0} (
      s1 <- return 1;
      s2 <- return (s1 + 1);
      x <- MO.get {};
      MO.set {Val=x.Val+1};
      MO.lift(
        case b of
            True => None
          | False => Some {});
      MO.modify (fn v => {Val=v.Val + 1});
      return (s2+2)
      )
  in
    return 
      (case o of
          Some (s,v) => <xml>State {[s.Val]} Val {[v]}</xml>
        | None => <xml>Nope!</xml>
      )
  end

fun testtrans {} : transaction xbody =
  r <- MT.run {Val=0} (
      x <- MT.get {};
      MT.set {Val=x.Val+1};
      x <- MT.get {};
      MT.set {Val=x.Val+1};
      MT.lift (debug "Hello, inner monad");
      x <- MT.get {};
      return (x.Val+1)
    );
  let
    val (s,v) = r
  in
    return <xml>State {[s.Val]} Val {[v]}</xml>
  end

fun main {} : transaction page = 
    o1 <- testopt True;
    o2 <- testopt False;
    t <- testtrans {};
    return
      <xml>
        <head>
        </head>
        <body>
          Urweb-monad test
          <h3> Wrapped transaction </h3>
          {t}
          <h3> Wrapped option (Should fail)</h3>
          {o1}
          <h3> Wrapped option (Should return something)</h3>
          {o2}
        </body>
      </xml>

