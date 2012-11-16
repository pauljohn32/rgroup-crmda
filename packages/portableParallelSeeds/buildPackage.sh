#!/bin/sh


### 2012-11-1 adapts ideas from
### http://stackoverflow.com/questions/4380849/roxygen-package-building-and-use-rd2-true

PACKAGE="portableParallelSeeds"

VERSION=$(awk -F": +" '/^Version/ { print $2 }' ${PACKAGE}/DESCRIPTION)

rm -rf ${PACKAGE}.gitex

mkdir ${PACKAGE}.gitex
cd ${PACKAGE}
git archive master | tar -x -C "../${PACKAGE}.gitex"
cd ..
cd ${PACKAGE}.gitex/vignettes

lyx -e pdf2 pps.lyx
lyx -e sweave pps.lyx
cp -f pps.pdf ../../${PACKAGE}/vignettes

cd ../..

R --vanilla <<EOR
library(roxygen2)
roxygenize("${PACKAGE}.gitex")

EOR


rsync -ra ${PACKAGE}.gitex/man ${PACKAGE}

R CMD build ${PACKAGE}.gitex


read -p "Run check: OK? (y or n)" result

if [ $result = "y" ];  then
R CMD check --as-cran ${PACKAGE}_${VERSION}.tar.gz
fi 

read -p "Install: OK? (y or n)" result
if [ $result = "y" ]; then
R CMD INSTALL ${PACKAGE}_${VERSION}.tar.gz
fi


read -p "Erase git temporary: OK? (y or n)" result
if [ $result = "y" ]; then
rm -rf ${PACKAGE}.gitex
fi


read -p "Erase Rcheck temporary: OK? (y or n)" result
if [ $result = "y" ]; then
rm -rf ${PACKAGE}.Rcheck
fi
