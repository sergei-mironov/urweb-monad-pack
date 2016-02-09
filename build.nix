libraries :

let

  uwb = import <urweb-build> libraries;

in with uwb;

rec {

  monad-pack = mkLib {
    name = "MonadPack";

    statements = [
      (src1 ./error.ur)
      (src1 ./identity.ur)
      (src1 ./state.ur)
      (src1 ./pure.ur)
    ];
  };

  tests = let
    mkTest = src : mkExe {
      name = "Test";
      dbms = "sqlite";

      libraries = {
        inherit monad-pack;
      };

      statements = [
        (sys "option")
        (sys "list")
        (src1 src)
      ];
    };
  in
    map mkTest [ ./test/Test4.ur ./test/TestError1.ur ./test/TestState1.ur
    ./test/TestState2.ur ./test/TestState3.ur ./test/XmlGenDemo.ur ];

}

