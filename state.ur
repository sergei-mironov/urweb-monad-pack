
functor Trans(S :
sig
  con m :: Type -> Type
  val monad_m : monad m
end) :

sig

  datatype state st a = State of (st -> S.m (st * a))

  val unState : st ::: Type -> a ::: Type -> state st a -> (st -> S.m (st*a))

  val run : st ::: Type -> a ::: Type -> st -> state st a -> S.m (st*a)

  val eval : st ::: Type -> a ::: Type -> st -> state st a -> S.m st

  val get : st ::: Type -> {} -> state st st

  val set : st ::: Type -> st -> state st {}

  val modify : st ::: Type -> (st -> st) -> state st {}

  val lift : st ::: Type -> a ::: Type -> S.m a -> state st a

  val monad_state : st ::: Type -> monad (state st)

end =

struct

  datatype state st a = State of (st -> S.m (st * a))

  fun unState [st] [a] ((State f): state st a) : (st -> S.m (st*a)) = f

  fun run [st] [a] (s:st) (m:state st a) : S.m (st * a) = (unState m) s

  fun eval [st] [a] (s:st) (m:state st a) : S.m st =
    x <- ((unState m) s);
    let val (a,_) = x in return a end

  fun mreturn [st] [a] (r:a) : state st a = State (fn st => return (st,r))

  fun mbind [st] [a] [b] (m1 : state st a) (m2 : a -> state st b) : state st b =
    State (fn (s:st) =>
      r <- (unState m1) s;
      case r of
        (s', va) => unState (m2 va) s')

  fun get [st] {} : state st st = State (fn s => return (s,s))

  fun set [st] (s:st) : state st {} = State (fn _ => return (s,{}))

  fun modify [st] (f:st -> st) : state st {} = State (fn s => return (f s,{}))

  val monad_state = fn [st ::: Type] => mkMonad { Return = @@mreturn [st], Bind = @@mbind [st] }

  fun lift [st] [a] (ma:S.m a) : state st a = 
    State (fn (s:st) =>
      a <- ma;
      return (s,a)
      )

end

