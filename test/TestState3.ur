
structure MT = State.Trans(struct con m = Basis.transaction end)

fun forM_ [m ::: (Type -> Type)] (_ : monad m) [a] (ls:list a) (f:a -> m {}) : m {} =
    let
        fun mapM' ls =
            case ls of
              | []      => return {}
              | x :: ls => f x; mapM' ls
    in
        mapM' ls
    end

table t : {Id : int, Nam: string}

fun checked [x ::: {Type}] [x~[Id=int]] (l : list (source bool * record (x++[Id=int]))) : transaction (list int) =
  List.mapPartialM (fn (s,r) =>
    v <- get s;
    case v of
      |True => return (Some r.Id)
      |False => return None) l

val null_meta  = { Sig = [] , Xml = <xml/> }

task initialize = fn _ =>
  dml(DELETE FROM t WHERE Id >= 0);
  dml(INSERT INTO t(Id,Nam) VALUES(0,"Name1"));
  dml(INSERT INTO t(Id,Nam) VALUES(1,"Name2"));
  dml(INSERT INTO t(Id,Nam) VALUES(2,"Name3"));
  dml(INSERT INTO t(Id,Nam) VALUES(3,"Name3"));
  dml(INSERT INTO t(Id,Nam) VALUES(4,"Name4"))

fun rename (ids : list int) : transaction {} =
  forM_ ids (fn id =>
    dml(UPDATE t SET Nam = "aaaa" WHERE Id = {[id]}));
  return {}

fun main {} : transaction page =
  m' <- query (SELECT * FROM t) (fn r m =>
    s <- source False;
    return {Sig = (s, r.T) :: m.Sig, Xml =
      <xml>
        <br/>
        <div>
          <ccheckbox source={s}/>
          {[r.T.Nam]}
        </div>
        {m.Xml}
      </xml>}) null_meta;

  return
  <xml>
    <body>
      {m'.Xml}
      <button value="Update selected" onclick={fn _ =>
        c <- checked m'.Sig;
        rpc(rename c);
        redirect(url (main {}))
      }/>
    </body>
  </xml>
