
bench-html:
	ghc --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src benchmarks/Utf8Html.hs
	./benchmarks/Utf8Html --resamples 10000 bigTable

bench-builder:
	ghc --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src benchmarks/Utf8Builder.hs
	./benchmarks/Utf8Builder --resamples 10000
