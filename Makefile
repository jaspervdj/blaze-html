################################################################################
# Configuration
################################################################################

GHC = ghc
GHCI = ghci
GHC_FLAGS = -O2 -fforce-recomp -idoc/examples -ibenchmarks -itests

BENCHMARK_FLAGS = --resamples 10000

################################################################################
# Code generation
################################################################################

# Generate the actual HTML combinators
combinators:
	runghc Util/GenerateHtmlCombinators.hs

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
	$(GHC) $(GHC_FLAGS) -fhpc --make -main-is TestSuite tests/TestSuite.hs
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

################################################################################
# Switching cabal files
################################################################################

# The current target used
CURRENT=$(shell ls *.cabal | sed 's/\.cabal//')

# Auxiliary
hide-cabal-files:
	cabal clean
	mv ${CURRENT}.cabal ${CURRENT}.cabal.${CURRENT}
	mv Setup.hs Setup.hs.${CURRENT}

blaze-html: hide-cabal-files
	mv blaze-html.cabal.blaze-html blaze-html.cabal
	mv Setup.hs.blaze-html Setup.hs

blaze-from-html: hide-cabal-files
	mv blaze-from-html.cabal.blaze-from-html blaze-from-html.cabal
	mv Setup.hs.blaze-from-html Setup.hs

################################################################################
# Switching cabal files
################################################################################

# Cleanup
clean:
	rm -rf doc/examples/BenchmarkServer doc/examples/*.hi
	rm -rf benchmarks/HtmlBenchmarks benchmarks/*.hi
	rm -rf Text/Blaze/*.hi Text/Blaze/Html4/*.hi Text/Blaze/Html5/*.hi Text/Blaze/Renderer/*.hi Text/*.hi
	rm -rf Text/Blaze/*.o Text/Blaze/Html4/*.o Text/Blaze/Html5/*.o Text/Blaze/Renderer/*.o Text/*.o
