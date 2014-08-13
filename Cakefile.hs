module Cakefile where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import Cakefile_P

instance IsString File where fromString = file

project = do

  u <- uwlib "lib.urp" $ do
    ur (single "error.ur")
    ur (single "state.ur")
    ur (single "identity.ur")
    ur (single "pure.ur")

  apps <- forM ["Test4.ur", "TestError1.ur", "TestState1.ur", "TestState2.ur", "TestState3.ur"] $ \f -> do
    let src = (file $ "test"</> f)
    uwapp "-dbms sqlite" (src.="urp") $ do
      database ("dbname="++(takeBaseName f))
      sql ((file f).="sql")
      library u
      ur (sys "option")
      ur (sys "list")
      ur (single src)
      debug

  rule $ do
    phony "run"
    shell [cmd|$(last apps)|]

  rule $ do
    phony "clean"
    unsafeShell [cmd|rm -rf .cake3 $(map tempfiles apps)|]

  rule $ do
    phony "all"
    depend u
    depend apps

main = do
  writeMake (file "Makefile") (project)
  writeMake (file "Makefile.devel") (selfUpdate >> project)

