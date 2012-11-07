#!/bin/sh


### 2012-11-1 adapts ideas from
### http://stackoverflow.com/questions/4380849/roxygen-package-building-and-use-rd2-true

PACKAGE="portableParallelSeeds"

VERSION=$(awk -F": +" '/^Version/ { print $2 }' ${PACKAGE}/DESCRIPTION)

rm -rf ${PACKAGE}.svnex

svn export ${PACKAGE} ${PACKAGE}.svnex
cd ${PACKAGE}.svnex/vignettes

lyx -e pdf2 pps.lyx
lyx -e sweave pps.lyx
cp -f pps.pdf ../../${PACKAGE}/vignettes

cd ../..

R --vanilla <<EOR
library(roxygen2)
roxygenize("${PACKAGE}.svnex")

EOR

R CMD build ${PACKAGE}.svnex

read -p "Run check: OK? (y or n)" result

if [ $result = "y" ];  then
R CMD check --as-cran ${PACKAGE}_${VERSION}.tar.gz
fi 

read -p "Install: OK? (y or n)" result
if [ $result = "y" ]; then
R CMD INSTALL ${PACKAGE}_${VERSION}.tar.gz
fi