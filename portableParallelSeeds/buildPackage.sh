rm -rf portableParallelSeeds.svnex

svn export portableParallelSeeds portableParallelSeeds.svnex
cd portableParallelSeeds.svnex/vignettes
lyx -e pdf2 pps.lyx
lyx -e sweave pps.lyx

## cp -f pps.pdf ../inst/doc

cd ../..

R --vanilla -f runRoxygen2.R

R CMD build portableParallelSeeds.svnex

read -p "enter name of tarball: " result

R CMD check --as-cran $result
