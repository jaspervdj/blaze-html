bench-html:
	ghc --make -O2 -fforce-recomp -main-is HtmlBenchmarks benchmarks/HtmlBenchmarks.hs
	./benchmarks/HtmlBenchmarks --resamples 10000

bench-constructor-html:
	ghc --make -O2 -fforce-recomp -ilib/binary-0.5.0.2/src -main-is Main benchmarks/bigtable/constructor.hs
	./benchmarks/bigtable/constructor --resamples 10000

bench-closure-constructor-html:
	ghc --make -O2 -fforce-recomp -ilib/binary-0.5.0.2/src -main-is Main benchmarks/bigtable/closure-constructor.hs
	./benchmarks/bigtable/closure-constructor --resamples 10000

core-closure-constructor-html:
	ghc-core -- --make -O2 -fforce-recomp -ilib/binary-0.5.0.2/src -main-is Main benchmarks/bigtable/closure-constructor.hs

bench-builder:
	ghc --make -O2 -fforce-recomp -ilib/binary-0.5.0.2/src -main-is Utf8Builder benchmarks/Utf8Builder.hs
	./benchmarks/Utf8Builder --resamples 10000

benchmarkserver:
	ghc --make -threaded -O2 -fforce-recomp -idoc/examples -ibenchmarks -main-is BenchmarkServer doc/examples/BenchmarkServer.lhs

snapbenchmarkserver:
	ghc --make -threaded -O2 -fforce-recomp -idoc/examples -ibenchmarks -main-is SnapBenchmarkServer doc/examples/SnapBenchmarkServer.hs

bench-new-builder:
	ghc --make -O2 -fforce-recomp -ilib/binary-0.5.0.2/src -main-is Data.Binary.NewBuilder lib/binary-0.5.0.2/src/Data/Binary/NewBuilder.hs
	./lib/binary-0.5.0.2/src/Data/Binary/NewBuilder --resamples 10000

core-new-builder:
	ghc-core -- --make -O2 -fforce-recomp -ilib/binary-0.5.0.2/src -main-is Data.Binary.NewBuilder lib/binary-0.5.0.2/src/Data/Binary/NewBuilder.hs

bench-bigtable-non-haskell:
	ruby benchmarks/bigtable/erb.rb
	ruby benchmarks/bigtable/erubis.rb
	php -n benchmarks/bigtable/php.php

test:
	runghc -itests tests/TestSuite.hs
