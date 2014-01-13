
datatype state st a = State of (st -> (st * a))

fun unState [st] [a] ((State f): state st a) : (st -> (st*a)) = f

fun mrun [st] [a] (s:st) (m:state st a) : (st * a) = (unState m) s

fun mreturn [st] [a] (r:a) : state st a = State (fn st => (st,r))

fun mbind [st] [a] [b] (m1 : state st a) (m2 : a -> state st b) : state st b =
  State (fn (s : st) =>
    let
      val (s', va) = (unState m1) s
    in
      unState (m2 va) s'
    end)

fun get [st] {} : state st st = State (fn s => (s,s))

fun set [st] (s:st) : state st {} = State (fn _ => (s,{}))

val monad = fn [st ::: Type] => mkMonad { Return = @@mreturn [st], Bind = @@mbind [st] }

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

