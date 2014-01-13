
datatype state st a = State of (st -> transaction (st * a))

fun unState [st] [a] ((State f): state st a) : (st -> transaction (st*a)) = f

fun mrun [st] [a] (s:st) (m:state st a) : transaction (st * a) = (unState m) s

fun mreturn [st] [a] (r:a) : state st a = State (fn st => return (st,r))

fun mbind [st] [a] [b] (m1 : state st a) (m2 : a -> state st b) : state st b =
  State (fn (s:st) =>
    r <- (unState m1) s;
    case r of
      (s', va) => unState (m2 va) s')

fun get [st] {} : state st st = State (fn s => return (s,s))

fun set [st] (s:st) : state st {} = State (fn _ => return (s,{}))

val monad = fn [st ::: Type] => mkMonad { Return = @@mreturn [st], Bind = @@mbind [st] }

fun lift [st] [a] (ma:transaction a) : state st a = 
  State (fn (s:st) =>
    a <- ma;
    return (s,a)
    )

fun main {} : transaction page = 
  r <- mrun {Val=0} (
      s1 <- return 1;
      s2 <- return (s1 + 1);
      x <- get {};
      set {Val=x.Val+1};
      lift (debug "Hello, inner monad");
      return (s2+2)
    );
  let
    val (s,v) = r
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

