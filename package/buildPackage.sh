PACKAGE="semTable"

VERSION=$(awk -F": +" '/^Version/ { print $2 }' ${PACKAGE}/DESCRIPTION)

cd ${PACKAGE}/vignettes
lyx -f -e sweave semTable.lyx
cd ../../


rm -rf ${PACKAGE}.gitex;

mkdir ${PACKAGE}.gitex
cd ${PACKAGE}

##cd vignettes
##lyx -f -e sweave semTable.lyx;
##cd ..

## copies UNCOMMITTED but TRACKED files.
git ls-files . | tar cT - | tar -x -C "../${PACKAGE}.gitex"
cd ..

# cd ${PACKAGE}.gitex/vignettes

# cp -f variablekey.pdf ../inst/doc
# cd ../..

R --vanilla -f runRoxygen2.R


/usr/lib/R/bin/R --no-site-file --no-environ --no-save --no-restore --quiet  \
                 CMD build "${PACKAGE}.gitex"  --no-resave-data --no-manual \
                 --compact-vignettes="both"


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


echo "Consider upload to KRAN"
