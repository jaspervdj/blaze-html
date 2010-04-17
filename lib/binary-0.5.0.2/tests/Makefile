all: compiled
     
interpreted:
	runhaskell QC.hs 1000

compiled:
	ghc --make -fhpc -O QC.hs -o qc -no-recomp -threaded
	./qc 500 +RTS -qw -N2

bench:: Benchmark.hs MemBench.hs CBenchmark.o
	ghc --make -O2 -fliberate-case-threshold=1000 -fasm Benchmark.hs CBenchmark.o -o bench -fforce-recomp
	./bench 100

bench-nb::
	ghc --make -O2 -fliberate-case-threshold=1000 NewBenchmark.hs -fasm -o bench-nb
	./bench-nb 

CBenchmark.o: CBenchmark.c
	gcc -O3 -c $< -o $@

hugs:
	runhugs -98 QC.hs  


HeapUse: HeapUse.hs
	ghc --make -O $^ -fasm -o $@

heap: HeapUse
	./HeapUse +RTS -M10M -t/dev/stderr -RTS

clean:
	rm -f *.o *.hi qc bench bench-nb *~

.PHONY: clean bench bench-nb
