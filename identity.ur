
functor Trans(S :
sig
  con m :: Type -> Type
  val monad_m : monad m
end) :

sig

  datatype identity a = Identity of (S.m a)

  val run : a ::: Type -> identity a -> S.m a

  val monad_identity : monad (identity)

end =

struct

  datatype identity a = Identity of (S.m a)

  val run [a] ((Identity m):identity a) = m

  fun mreturn [a] (r:a) = Identity (return r)

  fun mbind [a] [b] (m1 : identity a) (m2 : a -> identity b) : identity b =
    Identity (
      a <- run m1;
      run (m2 a))

  val monad_identity = mkMonad { Return = @@mreturn, Bind = @@mbind }

end

