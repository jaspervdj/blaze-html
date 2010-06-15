
bench-html:
	ghc --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src -main-is Utf8Html benchmarks/Utf8Html.hs
	./benchmarks/Utf8Html --resamples 10000

bench-constructor-html:
	ghc --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src -main-is Main benchmarks/bigtable/constructor.hs
	./benchmarks/bigtable/constructor --resamples 10000

bench-closure-constructor-html:
	ghc --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src -main-is Main benchmarks/bigtable/closure-constructor.hs
	./benchmarks/bigtable/closure-constructor --resamples 10000

core-closure-constructor-html:
	ghc-core -- --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src -main-is Main benchmarks/bigtable/closure-constructor.hs

bench-builder:
	ghc --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src -main-is Utf8Builder benchmarks/Utf8Builder.hs
	./benchmarks/Utf8Builder --resamples 10000

bench-bigtableserver:
	ghc --make -threaded -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src doc/examples/BigTableServer.lhs

bench-new-builder:
	ghc --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src -main-is Data.Binary.NewBuilder lib/binary-0.5.0.2/src/Data/Binary/NewBuilder.hs
	./lib/binary-0.5.0.2/src/Data/Binary/NewBuilder --resamples 10000

core-new-builder:
	ghc-core -- --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src -main-is Data.Binary.NewBuilder lib/binary-0.5.0.2/src/Data/Binary/NewBuilder.hs



