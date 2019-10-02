################################################################################
# Configuration
################################################################################

GHC = ghc
GHCI = ghci
GHC_FLAGS = -O2 -fforce-recomp -idoc/examples -ibenchmarks -isrc -itests

BENCHMARK_FLAGS = --resamples 10000

################################################################################
# Code generation
################################################################################

# Generate the actual HTML combinators
combinators:
	runghc -isrc src/Util/GenerateHtmlCombinators.hs

# Copy the docs the website directory
website-docs:
	cabal haddock
	rm -rf website/docs
	cp -r dist/doc/html/blaze-html website/docs

################################################################################
# Tests
################################################################################

# Run the tests
test:
	$(GHC) $(GHC_FLAGS) -fhpc --make tests/TestSuite.hs
	rm -f TestSuite.tix
	./tests/TestSuite

# HPC
test-hpc:
	hpc markup --destdir=hpc TestSuite

################################################################################
# Benchmarks
################################################################################

benchmark:
	$(GHC) $(GHC_FLAGS) --make -main-is RunHtmlBenchmarks benchmarks/RunHtmlBenchmarks.hs
	./benchmarks/RunHtmlBenchmarks $(BENCHMARK_FLAGS) -u results.csv

benchmark-server:
	$(GHC) $(GHC_FLAGS) --make -threaded -main-is BenchmarkServer doc/examples/BenchmarkServer.lhs

snap-benchmark-server:
	$(GHC) $(GHC_FLAGS) --make -threaded -main-is SnapBenchmarkServer doc/examples/SnapBenchmarkServer.lhs

benchmark-bigtable-non-haskell:
	ruby benchmarks/bigtable/erb.rb
	ruby benchmarks/bigtable/erubis.rb
	php -n benchmarks/bigtable/php.php

# Cleanup
clean:
	rm -rf doc/examples/BenchmarkServer
	rm -rf benchmarks/HtmlBenchmarks
	find . -name '*.o' -o -name '*.hi' | xargs rm
