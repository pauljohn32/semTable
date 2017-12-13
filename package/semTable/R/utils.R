##' Delete trailing slash
##'
##' This function cleans up a path string by removing the trailing
##' slash.  This is necessary on MS Windows, \code{file.exists(fn)} fails
##' if "/" is on end of file name. Deleting the trailing slash is thus
##' required on Windows and it is not harmful on other platforms.
##'
##' All usages of \code{file.exists(fn)} in R should be revised
##' to be multi-platform safe by writing \code{file.exists(dts(fn))}.
##'
##' This version also removes repeated adjacent slashes, so that
##' \code{"/tmp///paul//test/"} becomes \code{"/tmp/paul/test"}.
##' @param name A path
##' @return Same path with trailing "/" removed.
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
dts <- function(name) gsub("/$", "", dms(name))
NULL

##' Delete multiple slashes, replace with one
##'
##' Sometimes paths end up with "/too//many//slashes".
##' While harmless, this is untidy. Clean it up.
##' @param name A character string to clean
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
dms <- function(name) gsub("(/)\\1+", "/", name)
NULL


##' Removes redundant words from beginnings of character strings
##'
##' In Qualtrix data, we sometimes find repeated words in column names.
##' This changes a vector c("Philadelphia_Philadelphia_3", "Denver_Denver_4")
##' to c("Philadelphia_3", "Denver_4")
##' 
##' See \url{https://stackoverflow.com/questions/43711240/r-regular-expression-match-omit-several-repeats}
##' @param x Character vector
##' @param sep Delimiter. A regular expression indicating the point at
##'     which to split the strings before checking for
##'     duplicates. Default will look for repeat separated by comma,
##'     underscore, or one space character.
##' @param  n Limit on number of duplicates to remove. Default, NULL,
##'      means delete all duplicates at the beginning of a string.
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' x <- c("Philadelphia_Philadelphia_3", "Denver_Denver_4",
##'         "Den_Den_Den_Den_Den_Den_Den_5")
##' deduper(x)
##' deduper(x, n = 2)
##' deduper(x, n = 3)
##' deduper(x, n = 4)
##' x <- c("Philadelphia,Philadelphia_3", "Denver Denver_4")
##' ## Shows comma also detected by default
##' deduper(x)
##' ## Works even if delimiter is inside matched string,
##' ## or separators vary
##'  x <- c("Den_5_Den_5_Den_5,Den_5 Den_5")
##' deduper(x)
##' ## generate vector
##' x <- replicate(10, paste(sample(letters, 5), collapse = ""))
##' n <- c(paste0("_", sample(1:10, 5)), rep("", 5))
##' x <- paste0(x, "_", x, n, n)
##' x
##' deduper(x)
##'
##' @return Cleaned up vector.
deduper <- function(x, sep = ",_\\s-", n = NULL) {
    gsub(paste0("^(.*?)(?:[", sep, "]\\1){1,", n, "}"),
         "\\1", x, perl = TRUE)
}
NULL





##' How many stars would we need for this p value?
##'
##' Regression table makers need to know how many stars
##' to attach to parameter estimates. This takes
##' p values and a vector which indicates how many stars
##' are deserved.  It returns a required number of asterixes.
##' Was named "stars" in previous version, but renamed due to
##' conflict with R base function \code{stars}
##'
##' Recently, we have requests for different symbols. Some people want
##' a "+" symbol if the p value is smaller than 0.10 but greater than
##' 0.05, while some want tiny smiley faces if p is smaller than
##' 0.001. We accomodate that by allowing a user specified vector of
##' symbols, which defaults to c("*", "**", "***")
##' @param pval P value
##' @param alpha alpha vector, defaults as c(0.05, 0.01, 0.001).
##' @param symbols The default is c("*", "**", "***"), corresponding
##'     to mean that p values smaller than 0.05 receive one star,
##'     values smaller than 0.01 get two stars, and so forth.  Must be
##'     same number of elements as alpha. These need not be asterixes,
##'     could be any character strings that users desire. See example.
##' @return a character vector of symbols (eg asterixes), same length as pval
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' starsig(0.06)
##' starsig(0.021)
##' starsig(0.001)
##' alpha.ex <- c(0.10, 0.05, 0.01, 0.001)
##' symb.ex <- c("+", "*", "**", ":)!")
##' starsig(0.07, alpha = alpha.ex, symbols = symb.ex)
##' starsig(0.04, alpha = alpha.ex, symbols = symb.ex)
##' starsig(0.009, alpha = alpha.ex, symbols = symb.ex)
##' starsig(0.0009, alpha = alpha.ex, symbols = symb.ex)
##'
starsig <-
    function(pval, alpha = c(0.05, 0.01, 0.001),
             symbols = c("*", "**", "***"))
{
    if (length(alpha) != length(symbols)) {
        messg <- "alpha vector must have same number of elements as symbols vector"
        stop(messg)
    }
    if(is.vector(pval) && !is.numeric(pval)) pval <- as.numeric(pval)
    nstars <- sapply(pval, function(x) sum(abs(x) < alpha))
    sapply(nstars, function(x) if(!is.na(x) && x > 0) symbols[x] else " ")
}
NULL


##' Remove elements if they are in a target vector, possibly replacing with NA
##'
##' If a vector has c("A", "b", "c") and we want to remove "b" and
##' "c", this function can do the work. It can also replace "b" and
##' "c" with the NA symbol.
##'
##' If elements in y are not members of x, they are silently ignored.
##'
##' The code for this is not complicated, but it is
##' difficult to remember.  Here's the recipe to remove
##' elements y from x: \code{x <- x[!x \%in\% y[y \%in\% x]]}. It is
##' easy to get that wrong when in a hurry, so we use this function
##' instead.  The \code{padNA} was an afterthought, but it helps sometimes.
##'
##' @param x vector from which elements are to be removed
##' @param y shorter vector of elements to be removed
##' @param padNA Default FALSE, Should removed items be replaced with NA values?
##' @return a vector with elements in y removed
##' @author Ben Kite <bakite@@ku.edu> and Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' x <- c("a", "b", "c", "d", "e", "f")
##' y <- c("e", "a")
##' removeMatches(x, y)
##' y <- c("q", "r", "s")
##' removeMatches(x, y)
removeMatches <- function(x, y, padNA = FALSE){
    if (padNA) {
        x[x %in% y] <- NA
    } else {
        x <- x[!x %in% y[y %in% x]]
    }
    x
}
NULL

##' Use new information to update a vector. Similar in concept to
##' R's modify list
##'
##' Original purpose was to receive 2 named vectors, x and y, and copy
##' "updated" named values from y into x. If x or y are not named,
##' however, this will do something useful.
##' \itemize{
##' \item Both vectors are named: values in x for which y names match will be
##'     updated with values from y. If \code{augment} is true, then named
##'     values in y that are not present in x will be added to x.
##' \item If neither vector is named: returns a new vector with x as the values
##'     and y as the names. Same as returning \code{names(x) <- y}.
##' \item If x is not named, y is named: replaces elements in x with values of y
##'     where suitable (x matches names(y)). For matches, returns x = y[x]
##'     if names(y) include x.
##' \item If x is named, y is not named: returns y, but with names from x. Lengths
##'     of x and y must be identical.
##' \item If y is NULL or not provided, x is returned unaltered.
##' }
##' @param x vector to be updated, may be named or not.
##' @param y possibly a named vector. If unnamed, must match
##'     length of x. If named, and length is shorter than x, then
##'     name-value pairs in x will be replaced with name-value pairs
##'     with y. If names in y are not in x, the augment argument
##'     determines the result.
##' @param augment If TRUE, add new items in x from y. Otherwise,
##'     ignore named items in y that are not in x.
##' @param warnings Defaults as FALSE. Show warnings about augmentation
##'     of the target vector.
##' @export
##' @return an updated vector
##' @author Paul Johnson
##' @examples
##' x <- c(a = 1, b = 2, c = 3)
##' y <- c(b = 22)
##' modifyVector(x, y)
##' y <- c(c = 7, a = 13, e = 8)
##' ## If augment = TRUE, will add more elements to x
##' modifyVector(x, y, augment = TRUE)
##' modifyVector(x, y)
##' x <- c("a", "b", "c")
##' y <- c("income", "education", "sei")
##' ## Same as names(x) <- y
##' modifyVector(x, y)
##' x <- c("a", "b", "c")
##' y <- c(a = "happy")
##' modifyVector(x, y)
##' y <- c(a = "happy", g = "glum")
##' ## Will give error unless augment = TRUE
##' modifyVector(x, y, augment = TRUE)
modifyVector <- function(x, y, augment = FALSE, warnings = FALSE){
    if (missing(x) || is.null(x)) stop("modifyVector: x should not be null")
    if (missing(y) || is.null(y)) return(x)
    ## neither has names, so values of y are names for x
    if (is.null(names(x)) && is.null(names(y))){
        if (length(x) == length(y)){
            names(x) = y
        } else{
            MESSG <- paste("if neither x nor y has names,",
                           "then x and y must be of same length")
            stop(MESSG)
        }
        return(x)
    }
    
    ## x has names, but y does not,  y is new values of x
    ## but x keeps old names
    if (!is.null(names(x)) && is.null(names(y))){
        if (length(x) == length(y)){
            x.names <- names(x)
            x <- y
            names(x) <- x.names
        } else{
            MESSG <- paste("if y has no names,",
                           "x and y must be of same length")
            stop(MESSG)
        }
        return(x)
    }
    
    ## x has no names, but y does.  Assume user meant
    ## to replace x *values* by y *names* and values. 
    if (is.null(names(x)) && !is.null(names(y))){
        ## x has no names, so we will give x its values
        ## as its names. 
        names(x) <- x
        return(modifyVector(x, y, augment))
    }
    
    ## x and y both have names, y may be different in length
    x.names <- names(x)
    y.names <- names(y)
    inboth <- intersect(y.names, x.names)
    yunique <- y[!y.names %in% x.names]
    
    x[inboth] <- y[inboth]
    
    if(augment){
        x <- c(x, yunique)
    } else {
        if(length(yunique) > 0 && warnings){
            MESSG <- paste("if augment = FALSE, elements in y",
                           "with names not in names(x) will be discarded")
            warning(MESSG)
        }
    }            
    x
}
NULL


##' apply a vector of replacements, one after the other.
##'
##' This is multi-gsub.  Use it when it is necessary to process
##' many patterns and replacements in a given order on a vector.
##'
##' @param pattern vector of values to be replaced. A vector filled
##'     with patterns as documented in the \code{gsub} pattern
##'     argument
##' @param replacement vector of replacements, otherwise same as
##'     \code{gsub}.  Length of replacement must be either 1 or same
##'     as pattern, otherwise an error results.
##' @param x the vector in which elements are to be replaced, same as
##'     \code{gsub}
##' @param ... Additional arguments to be passed to gsub
##' @return vector with pattern replaced by replacement
##' @author Jared Harpole <jared.harpole@@gmail.com> and Paul Johnson
##'     <pauljohn@@ku.edu>
##' @export
##' @examples
##' x <- c("Tom", "Jerry", "Elmer", "Bugs")
##' pattern <- c("Tom", "Bugs")
##' replacement <- c("Thomas", "Bugs Bunny")
##' (y <- mgsub(pattern, replacement, x))
##' x[1] <- "tom"
##' (y <- mgsub(pattern, replacement, x, ignore.case = TRUE))
##' (y <- mgsub(c("Elmer", "Bugs"), c("Looney Characters"), x, ignore.case = TRUE))
mgsub <- function(pattern, replacement, x, ... ){
    if (length(pattern) != length(replacement)) {
        if (length(replacement) == 1) {
            replacement <- rep(replacement, length(pattern))
        } else {
            messg <- paste("replacement must either be 1 element or the same number of elements as pattern")
            stop(messg)
        }
    }
    for (i in seq_along(pattern)){
        x <- gsub(pattern[i], replacement[i], x, ...)
    }
    x
}



##' Reduce each in a vector of strings to a given length
##'
##' This is a simple "chop" at k characters, no fancy truncation at
##' spaces or such. Optionally, this will make unique the resulting
##' truncated strings. That way, truncation at character 4 of
##' "Washington" and "Wash" and "Washingham" will not result in 3
##' values of "Wash", but rather "Wash", "Wash.1", and "Wash.2"
##' @param x character string
##' @param k integer limit on length of string. Default is 20
##' @param unique Default FALSE
##' @return vector of character variables no longer than k
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' x <- c("Washington", "Washingham", "Washmylaundry")
##' shorten(x, 4)
##' shorten(x, 4, unique = TRUE)
shorten <- function(x, k = 20, unique = FALSE){
    if(!is.character(x)) stop("shorten: x must be a character variable")
    y <- substr(x, 1, k)
    if (unique) y <- make.unique(y)
    y
}


##' Insert "\\n" after the k'th character in a string. This IS vectorized,
##' so can receive just one or many character strings in a vector.
##'
##' If a string is long, insert linebreak "\\n"
##'
##' If x is not a character string, x is returned without alteration. And
##' without a warning
##' @param x Character string.
##' @param k Number of characters after which to insert "\\n". Default is 20
##' @return Character with "\\n" inserted
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' x <- "abcdef ghijkl mnopqrs tuvwxyz abc def ghi jkl mno pqr stv"
##' stringbreak(x, 10)
##' stringbreak(x, 20)
##' stringbreak(x, 25)
##' x <- c("asdf asdfjl asfdjkl asdfjklasdfasd", "qrweqwer qwerqwerjklqw erjqwe")
##' stringbreak(x, 5)
stringbreak <- function(x, k = 20){
    if (!is.character(x)) return(x)
    breakOneString <- function(y, k){
        ylength <- nchar(y)
        if (ylength <= k) return (y)

        yseq <- seq(1, ylength, by = k)

        ## iterate on successive pairs of yseq, but exclude last one
        res <- ""
        for(i in seq_along(yseq[-length(yseq)])){
            res <- paste0(res, paste0(substr(y, yseq[i], (yseq[i+1] - 1)), "\n"))
        }
        if (yseq[i] < ylength) res <- paste0(res, substr(y, yseq[i + 1], ylength))
        res
    }
    vapply(x, breakOneString, k = k, character(1))
}



##' Insert 0's in the front of existing digits or characters so that
##' all elements of a vector have the same number of characters.
##'
##' The main purpose was to correct ID numbers in studies that are
##' entered as integers with leading 0's as in 000001 or 034554.  R's
##' default coercion of integers will throw away the preceding 0's,
##' and reduce that to 1 or 34554. The user might want to re-insert
##' the 0's, thereby creating a character vector with values "000001"
##' and "045665".
##'
##' If x is an integer or a character vector, the result is the
##' more-or-less expected outcome (see examples). If x is numeric,
##' but not an integer, then x will be rounded to the lowest integer.
##'
##' The previous versions of this function failed when there were
##' missing values (NA) in the vector x.  This version returns NA for
##' those values.
##'
##' One of the surprises in this development was that sprintf() in R
##' does not have a known consequence when applied to a character
##' variable. It is platform-dependent (unredictable). On Ubuntu Linux
##' 16.04, for example \code{sprintf("\%05s", 2)} gives back
##' \code{" 2"}, rather than (what I expected) \code{"00002"}. The
##' problem is mentioned in the documentation for \code{sprintf}. The
##' examples show this does work now, but please test your results.
##' @param x vector to be converted to a character variable by
##'     inserting 0's at the front. Should be integer or character,
##'     floating point numbers will be rounded down. All other
##'     variable types will return errors.
##' @param n Optional parameter.  The desired final length of
##'     character vector.  This parameter is a guideline to determine
##'     how many 0's must be inserted.  This will be ignored if
##'     \code{n} is smaller than the number of characters in the
##'     longest element of \code{x}.
##' @return A character vector
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' x1 <- c(0001, 0022, 3432, NA)
##' padW0(x1)
##' padW0(x1, n = 10)
##' x2 <- c("1", "22", "3432", NA)
##' padW0(x2)
##' ## There's been a problem with strings, but this works now.
##' ## It even preserves non-leading spaces. Hope that's what you want.
##' x3 <- c("1", "22 4", "34323  42", NA)
##' padW0(x3)
##' x4 <- c(1.1, 334.52, NA)
##' padW0(x4)
padW0 <- function (x, n = 0) {
    if (!is.numeric(x) && !is.character(x))
        stop("padW0 only accepts integer or character values for x")

    xismiss <- is.na(x)
    ## springf trouble with characters, not platform independent
    if(is.numeric(x)) {
        if (is.double(x)) x <- as.integer(x)
        maxlength <- max(nchar(x), n, na.rm = TRUE)
        vtype <- "d"
        res <- sprintf(paste0("%0", maxlength, vtype), x)
    } else {
        maxlength <- max(nchar(x), n, na.rm = TRUE)
        xlength <- vapply(x, nchar, integer(1))
        padcount <- maxlength - xlength
        pads <- vapply(padcount, function(i){paste0(rep("0", max(0, i, na.rm=TRUE)), collapse = "")}, character(1))
        res <- paste0(pads, x)
    }

    res[xismiss] <- NA
    res
}


##' Write CSV files with quotes same as MS Excel 2013 or newer
##'
##' R's write.csv inserts quotes around all elements in a character
##' vector (if quote = TRUE).  In contrast, MS Excel CSV export no
##' longer inserts quotation marks on all elements in character
##' variables, except when the cells include commas or quotation
##' marks.  This function generates CSV files that are, so far as we
##' know, exactly the same "quoted style" as MS Excel CSV export
##' files.
##'
##' This works by manually inserting quotation marks where necessary and
##' turning FALSE R's own method to insert quotation marks.
##' @param x a data frame
##' @param file character string for file name
##' @param row.names Default FALSE for row.names
##' @importFrom utils write.table
##' @return the return from write.table, using revised quotes
##' @export
##' @author Paul Johnson
##' @examples
##' set.seed(234)
##' x1 <- data.frame(x1 = c("a", "b,c", "b", "The \"Washington, DC\""),
##'       x2 = rnorm(4), stringsAsFactors = FALSE)
##' x1
##' fn <- tempfile(pattern = "testcsv", fileext = ".csv")
##' writeCSV(x1, file = fn)
##' readLines(fn)
##' x2 <- read.table(fn, sep = ",", header = TRUE, stringsAsFactors = FALSE)
##' all.equal(x1,x2)
writeCSV <- function(x, file, row.names = FALSE){
    xischar <- colnames(x)[sapply(x, is.character)]
    for(jj in xischar){
        x[ , jj] <- gsub('"', '""', x[ , jj], fixed = TRUE)
        needsquotes <- grep('[\",]', x[ ,jj])
        x[needsquotes, jj] <- paste0("\"", x[needsquotes, jj], "\"")
    }
    write.table(x, file = file, sep = ",", quote = FALSE,
                row.names = row.names)
}
NULL


##' Retrieve theme files
##'
##' A wrapper for file.copy that retieves files from the dn = "theme"
##' directory of a package
##' @param fn Vector of file names
##' @param dn "theme", or directory name used in package inst for
##'     theme information.
##' @param pkg Package name, default "crmda"
##' @return TRUE if succeeded
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export 
##' @examples
##' getFiles(c("jayhawk.pdf", "CRMDAlogo.pdf"))
getFiles <- function(fn, dn = "theme", pkg = "crmda"){
    fc <- function(x, dn){
        if(!dir.exists(dn)) stop(paste("getFiles fc dir", dn, "does not exist"))
        if(file.exists(file.path(dn, x))) return(TRUE)
        result <- file.copy(from = system.file(file.path(dn, x), package = pkg), to = dn)
    }
    ## Check no dir exists and no file named theme exists
    if(!dir.exists(dn)){
        if(!file.exists(dn)) dcreate <- dir.create(dn, recursive = TRUE, showWarnings = FALSE)
        else stop("getFiles cannot create theme directory")
    }
    result <- vapply(fn, fc, dn = dn, logical(1))
    if(any(!result))stop("getFiles failed")
    TRUE
}
