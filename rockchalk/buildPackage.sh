rm -rf rockchalk.svnex

svn export rockchalk rockchalk.svnex
cd rockchalk.svnex/vignettes

lyx -e pdf2 rockchalk.lyx
cp -f rockchalk.pdf ../inst/doc
cd ../..

R --vanilla -f runRoxygen2.R


R CMD build rockchalk.svnex

read -p "enter name of tarball: " result

R CMD check --as-cran $result
