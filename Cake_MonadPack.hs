module Cake_MonadPack where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import Cake_MonadPack_P

lib = do
  uwlib (file "lib.urp") $ do
    ur (single (file "error.ur"))
    ur (single (file "state.ur"))
    ur (single (file "identity.ur"))
    ur (single (file "pure.ur"))

main = writeMake (file "Makefile") $ do

  apps <- forM (["Test4.ur", "TestError1.ur", "TestState1.ur", "TestState2.ur",
                "TestState3.ur", "XmlGenDemo.ur"]) $ \f -> do
    let src = (file $ "test"</> f)
    uwapp "-dbms sqlite" (src.="urp") $ do
      database ("dbname="++((takeBaseName f) .= "db"))
      sql (src .= "sql")
      library lib
      ur (sys "option")
      ur (sys "list")
      ur (single src)

  rule $ do
    phony "run"
    shell [cmd|$(last apps)|]

  rule $ do
    phony "lib"
    depend lib

  rule $ do
    phony "all"
    depend lib
    depend apps
