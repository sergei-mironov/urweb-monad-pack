
datatype pure a = Pure of a

fun run [a ::: Type] ((Pure r):pure a) : a = r

fun mreturn [a] (r:a) = Pure r

fun mbind [a] [b] (m1 : pure a) (m2 : a -> pure b) : pure b = m2 (run m1)

val monad_pure = mkMonad { Return = @@mreturn, Bind = @@mbind }

