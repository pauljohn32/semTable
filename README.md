# The semTable package

These functions were in kutils until December, 2017.

Because the semTable function was a very large undertaking, and
it is valuable separate from the other pieces in kutils, we decided
to make this a free-standing package.

Release versions of the package will be uploaded in CRAN.


The work is in the package directory. There is a build
script that works for me in Ubuntu Linux

Here are some rules.

1. Line length should be less than 80, and this is
absolutely required in sections for documentation.

2. Use correct Roxygen markup.
We don't have an idiot's guide for this yet, so please
consult Websites. These have been helpful:

[http://r-pkgs.had.co.nz/man.html#text-formatting][]

3. examples sections should be included with functions, these
should run. Can be IN code right before function or as
separate files saved under inst/examples. See rockchalk
for examples like that.

# Here is a rant

The usual problem is that code revisions or changes in Roxygen2 markup
are not technically correct, so that the package build fails.  Before
proposing patches (merge requests), we ask contributors to run a
test build to make sure their changes do not break the package.

We **insist** the user *run the script buildPackage.sh* to completion
before pushing changes back to repository.

Here is an example of a successuful run:

```bash
pauljohn@delllap-16:package$ ./buildPackage.sh

R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> options(repos = c("http://rweb.crmda.ku.edu/kran", "http://rweb.crmda.ku.edu/cran"))
> library(roxygen2)
> roxygenize("semTable")
Loading required package: foreign
Loading required package: kutils
Loading required package: xtable
Loading required package: lavaan
This is lavaan 0.5-23.1097
lavaan is BETA software! Please report any bugs.
Loading required package: plyr
> roxygenize("semTable.gitex")
Writing NAMESPACE
Writing detectNested.Rd
Writing compareCFA.Rd
Writing semTable.Rd
Writing markupConvert.Rd
Writing testtable.Rd
Writing dts.Rd
Writing dms.Rd
Writing deduper.Rd
Writing starsig.Rd
Writing removeMatches.Rd
Writing modifyVector.Rd
Writing mgsub.Rd
Writing shorten.Rd
Writing stringbreak.Rd
Writing padW0.Rd
Writing writeCSV.Rd
Writing getFiles.Rd
>
>
* checking for file ‘semTable.gitex/DESCRIPTION’ ... OK
* preparing ‘semTable’:
* checking DESCRIPTION meta-information ... OK
* checking for LF line-endings in source and make files and shell scripts
* checking for empty or unneeded directories
* building ‘semTable_1.0.tar.gz’
Warning: invalid uid value replaced by that for user 'nobody'
Warning: invalid gid value replaced by that for user 'nobody'

Run check: OK? (y or n)y
* using log directory ‘/home/pauljohn/GIT/CRMDA/software/semTable/package/semTable.Rcheck’
* using R version 3.4.3 (2017-11-30)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--as-cran’
* checking for file ‘semTable/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘semTable’ version ‘1.0’
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Paul Johnson <pauljohn@ku.edu>’

New submission
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘semTable’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
** found \donttest examples: check also with --run-donttest
* checking PDF version of manual ... OK
* DONE

Status: 1 NOTE
See
  ‘/home/pauljohn/GIT/CRMDA/software/semTable/package/semTable.Rcheck/00check.log’
for details.


Install: OK? (y or n)
```
