
type st = {Val:int}

datatype state a = State of (st -> (st * a))

fun unState [a] ((State f): state a) : (st -> (st*a)) = f

fun mrun [a]  (s:st) (m:state a) : (st * a) = (unState m) s

fun mreturn [a] (r:a) : state a = State (fn st => (st,r))

fun mbind [a] [b] (m1 : state a) (m2 : a -> state b) : state b =
  State (fn (s : st) =>
    let
      val (s', va) = (unState m1) s
    in
      unState (m2 va) s'
    end)

val monad = mkMonad { Return = @@mreturn , Bind = @@mbind }

fun get {} : state st = State (fn s => (s,s))

fun set (s:st) : state {} = State (fn _ => (s,{}))

fun main {} : transaction page = 
  let
    val (s,v) = mrun {Val=0} (
      s1 <- return 1;
      s2 <- return (s1 + 1);
      x <- get {};
      set {Val=x.Val+1};
      return (s2+2)
      )
  in
    return
      <xml>
        <head>
        </head>
        <body>
          Urweb-monad test
          <br/>
          State value is {[v]}
          <br/>
          State itself is {[s.Val]}
        </body>
      </xml>
  end

