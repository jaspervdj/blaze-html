
bench-html:
	ghc --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src -main-is Utf8Html benchmarks/Utf8Html.hs
	./benchmarks/Utf8Html --resamples 10000

bench-builder:
	ghc --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src -main-is Utf8Builder benchmarks/Utf8Builder.hs
	./benchmarks/Utf8Builder --resamples 10000

bench-bigtableserver:
	ghc --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src -ibenchmarks -main-is BigTableServer benchmarks/BigTableServer.hs
