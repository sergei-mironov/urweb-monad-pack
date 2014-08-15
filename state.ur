
functor Trans(S :
sig
  con m :: Type -> Type
  val monad_m : monad m
end) :

sig

  type state st a = st -> S.m (st * a)

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

  type state st a = st -> S.m (st * a)

  fun unState [st] [a] (f: state st a) : (st -> S.m (st*a)) = f

  fun run [st] [a] (s:st) (m:state st a) : S.m (st * a) = m s

  fun eval [st] [a] (s:st) (m:state st a) : S.m st =
    x <- (m s);
    let val (a,_) = x in return a end

  fun mreturn [st] [a] (r:a) : state st a = (fn (s:st) => return (s,r))

  fun mbind [st] [a] [b] (m1 : state st a) (m2 : a -> state st b) : state st b =
    (fn (s:st) =>
      r <- m1 s;
      case r of
        (s', va) => m2 va s')

  fun get [st] {} : state st st = (fn (s:st) => return (s,s))

  fun set [st] (s:st) : state st {} = (fn _ => return (s,{}))

  fun modify [st] (f:st -> st) : state st {} = (fn s => return (f s,{}))

  val monad_state = fn [st ::: Type] => mkMonad { Return = @@mreturn [st], Bind = @@mbind [st] }

  fun lift [st] [a] (ma:S.m a) : state st a = 
    (fn (s:st) =>
      a <- ma;
      return (s,a)
    )

end

