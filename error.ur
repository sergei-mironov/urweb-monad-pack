
datatype either l r = ELeft of l | ERight of r

functor Trans(S :
sig
  con m :: Type -> Type
  val monad_m : monad m
end) :

sig

  con error :: Type -> Type -> Type

  val unError : e ::: Type -> a ::: Type -> error e a -> S.m (either e a)

  val run : e ::: Type -> a ::: Type -> error e a -> S.m (either e a)

  val fail : e ::: Type -> a ::: Type -> e -> error e a

  val monad_error : e ::: Type -> monad (error e)

end =

struct

  con error = fn e => fn a => S.m (either e a)

  fun unError [e] [a] (m:error e a) : S.m (either e a) = m

  val run [e] [a] (m:error e a) = unError m

  val fail [e] [a] (err:e) = (return (ELeft err))

  fun mreturn [e] [a] (r:a) : error e a = (return (ERight r))

  fun mbind [e] [a] [b] (m1 : error e a) (m2 : a -> error e b) : error e b =
    ( r <- unError m1;
      case r of
        | ELeft e => return (ELeft e)
        | ERight a => unError (m2 a))

  val monad_error = fn [e ::: Type] => (@@mkMonad [error e] { Return = @@mreturn [e], Bind = @@mbind [e] })

end

