structure E = Error.Trans(struct con m = Basis.transaction end)

fun tes1 {} : transaction xbody =
  let
  in
    o <- E.run (
      s1 <- return 1;
      s2 <- return (s1 + 1);
      return (s2+2)
      );

    return 
      (case (o:Error.either string int) of
        | Error.ELeft e => <xml>Error: {[e]}</xml>
        | Error.ERight a => <xml>Result: {[a]}</xml>
      )
  end

fun tes2 i : transaction xbody =
  let
  in
    o <- E.run (
      s1 <- return i;
      if ((mod i 2) = 0) then
        @@E.fail [string] [int] "oooops"
      else
        return {};
      E.lift (debug "lift test");
      s2 <- return (s1 + 1);
      return (s2+2)
      );

    return 
      (case (o:Error.either string int) of
        | Error.ELeft e => <xml>Error: {[e]}</xml>
        | Error.ERight a => <xml>Result: {[a]}</xml>
      )
  end

fun main {} : transaction page = 
    i <- rand;
    o1 <- tes1 {};
    o2 <- tes2 i;
    return
      <xml>
        <head>
        </head>
        <body>
          Urweb-error test
          <h3>Test1</h3> {o1} <br/>
          <h3>Test2</h3> {o2} <br/>
        </body>
      </xml>

