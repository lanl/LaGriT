
outx3dgen may differ with added exodus output and reporting
diff the output files

bash:
  for f in *.exo ; do echo $f; ncdump $f > $f.ascii; done
  for f in *.exo.ascii; do echo $f; diff $f reference/$f; done
