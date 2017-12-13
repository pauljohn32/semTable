##' Discern nesting pattern of SEM coefficients
##'
##' Receives a list of models and orders them by best guess at intended nesting
##' @param models A List of lavaan-fitted SEM models
##' @export
##' @return matrix indicating nesting relationships
##' @author Ben Kite <bakite@@ku.edu>
detectNested <- function(models){
    dfval <- ifelse("df.scaled" %in% lavaan::fitMeasures(models[[1]]), "df.scaled", "df")
    dfs <- lapply(models, lavaan::fitMeasures, dfval)
    dfsrank <- rank(unlist(dfs))
    names(dfsrank) <- names(models)
    sortedmods <- sort(dfsrank)
    nested <- matrix(NA, length(sortedmods), length(sortedmods))
    diag(nested) <- 0
    colnames(nested) <- names(sortedmods)
    rownames(nested) <- names(sortedmods)
    ## Columns represent the possible larger model, rows inform about
    ## whether or not the model is nested in the column
    for (i in rownames(nested)){
        for (j in colnames(nested)){
            if(i != j){
                dfnest <- dfsrank[i] > dfsrank[j]
                iparams <- data.frame(models[[i]]@ParTable)[,c("lhs", "op", "rhs")]
                jparams <- data.frame(models[[j]]@ParTable)[,c("lhs", "op", "rhs")]
                iparams <- apply(iparams, 1, paste, collapse = "")
                jparams <- apply(jparams, 1, paste, collapse = "")
                iparams <- iparams[!iparams %in% iparams[grep("==", iparams)]]
                jparams <- jparams[!jparams %in% jparams[grep("==", jparams)]]
                parnest <- prod(jparams %in% iparams)
                nested[i,j] <- ifelse(dfnest*parnest == 1, 1, 0)
            }
        }
    }
    pairs <- matrix(NA, nrow(nested), 2)
    rownames(pairs) <- rownames(nested)
    colnames(pairs) <- c("nested", "parent")
    ## Make nested pairs
    for(i in rownames(nested)){
        pairs[i,"nested"] <- i
        nestedin <- which(nested[i,] == 1)
        if (length(nestedin) > 0){
            pairs[i,"parent"] <- names(nestedin[max(nestedin)])
        }
    }
    pairs
}

##' Compare CFA Tables
##'
##' @param models list of lavaan cfa models. The models should be
##'     named so as to refer to them in other arguments.
##' @param fitmeas vector of fit measures on which to compare the
##'     models. By default, fitmeas = c("chisq", "df", "pvalue",
##'     "rmsea", "cfi", "tli", "srmr", "aic", "bic", "srmr", "aic",
##'     "bic", "srmr_mplus").  Fit measures that are requested but not
##'     found within the objects are ignored.
##' @param nesting character string indicating the nesting structure
##'     of the models.  Must only contain model names and the symbols
##'     ">" and "+" (separated by spaces).  The parent model should be
##'     on the left, separated by ">". When multiple models are nested
##'     in the same parent, they are separated by the "+" sign. Please
##'     see examples.
##' @param scaled should scaled versions of the fit measures will be
##'     returned.  The defaul value is TRUE.
##' @param chidif should the nested models be compared by using the
##'     anova function? The anova function may pass the model
##'     comparison on to another lavaan function.  The results are
##'     added to the last three columns of the comparison table. The
##'     default value is TRUE.
##' @param file Default is NULL, no file created. If output file is
##'     desired, provide a character string for the file name.
##' @author Ben Kite
##' @export
##' @importFrom stats anova update
##' @importFrom plyr mapvalues
##' @importFrom xtable xtable
##' @importFrom kutils mgsub
##' @examples
##' \donttest{
##' ## These run longer than 5 seconds
##' library(lavaan)
##' library(xtable)
##' set.seed(123)
##' genmodel <- "f1 =~ .7*v1 + .7*v2 + .7*v3 + .7*v4 + .7*v5 + .7*v6
##' f1 ~~ 1*f1"
##' genmodel2 <- "f1 =~ .7*v1 + .7*v2 + .7*v3 + .7*v4 + .7*v5 + .2*v6
##' f1 ~~ 1*f1"
##'
##' dat1 <- simulateData(genmodel, sample.nobs=300)
##' dat2 <- simulateData(genmodel2, sample.nobs=300)
##' dat1$group <- 0
##' dat2$group <- 1
##' dat <- rbind(dat1, dat2)
##'
##' congModel <- "
##'               f1 =~ 1*v1 + v2 + v3 + v4 + v5 + v6
##'     		  f1 ~~ f1
##'     		  f1 ~0*1
##'     		 "
##' weakModel <- "
##'               f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + c(L5,L5)*v5 + c(L6,L6)*v6
##'     		  f1 ~~ f1
##'     		  f1 ~0*1
##'     		"
##' partialweakModel <- "
##'               f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + c(L5,L5)*v5 + v6
##'     		  f1 ~~ f1
##'     		  f1 ~0*1
##'     		"
##' partialweakModel2 <- "
##'               f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + v5 + v6
##'     		  f1 ~~ f1
##'     		  f1 ~0*1
##'     		"
##' partialstrongModel1 <- "
##'               f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + c(L5,L5)*v5 + v6
##'     		  f1 ~~ f1
##'     		  f1 ~ c(0,NA)*1
##'     		  v1 ~ c(I1,I1)*1
##'     		  v2 ~ c(I2,I2)*1
##'     		  v3 ~ c(I3,I3)*1
##'     		  v4 ~ c(I4,I4)*1
##'     		  v5 ~ c(I5,I5)*1
##'     		  v6 ~ c(I6,I6)*1
##'     		"
##' cc1 <- cfa(congModel, data=dat, group="group", meanstructure=TRUE, estimator="MLR")
##' cc2 <- cfa(weakModel, data=dat, group="group", meanstructure=TRUE, estimator="MLR")
##' cc21 <- cfa(partialweakModel, data=dat, group="group", meanstructure=TRUE, estimator="MLR")
##' cc3 <- cfa(partialstrongModel1, data=dat, group="group", meanstructure=TRUE, estimator="MLR")
##'
##' models <- list(cc1, cc2, cc21, cc3)
##' compareCFA(models, nesting=NULL)
##'
##' models <- list(Configural=cc1, Metric=cc2, PartialMetric=cc21, Scalar=cc3)
##' compareCFA(models, nesting = "Configural > Metric + PartialMetric > Scalar")
##'
##' compareCFA(models, fitmeas = c("chisq", "df", "cfi", "rmsea", "tli"),
##' nesting = "Configural > Metric + PartialMetric > Scalar")
##'
##' ## Creates output file
##' ## compareCFA(models, fitmeas=c("chisq", "df", "cfi", "rmsea", "tli"),
##' ## nesting = "Configural > Metric + PartialMetric > Scalar", file="table.tex")
##' }
compareCFA <- function(models,
                       fitmeas = c("chisq", "df",  "pvalue", "rmsea", "cfi", "tli", "srmr", "aic", "bic"),
                       nesting = NULL, scaled = TRUE, chidif = TRUE, file = NULL){
    if (is.null(names(models))){
        names(models) <- paste0("Model", seq(1, length(models)))
    }
    estimators <- lapply(models, function(x) x@Options$estimator)
    if (length(unlist(unique(estimators))) > 1L){
        stop(paste("The models provided do not have the same estimator.",
                   "This function cannot handle models with different estimators."))
    }
    if(is.null(nesting)){
        nestedPairs <- detectNested(models)
    }else{
        xx <- unlist(strsplit(nesting, " "))
        ops <- c("+", ">")
        mods <- names(models)
        xx %in% c(ops, mods)
        if (prod(xx %in% c(ops, mods)) != 1){
            stop(paste("There is something wrong with your nesting argument.",
                       "The only legal entries are \">\", \"+\", and the names",
                       "of models provided in the models list, and these entries",
                       "must be separated by spaces!"))
        }
        yy <- grep(">", xx)
        parents <- xx[c(yy-1)]
        nested <- list()
        for (i in parents){
            nested[[i]] <-  strsplit(unlist(strsplit(paste0(xx[which(xx == i):length(xx)], collapse = ""), ">"))[2], "\\+")
        }
        pairs <- cbind("nested" = names(nested[1]), "parent" = NA)
        for (i in 1:length(nested)){
            pairs <- rbind(pairs, cbind("nested" = nested[[i]][[1]], "parent" = names(nested)[i]))
        }
        nestedPairs <- pairs
    }
    fitmeas_f <- if(scaled) c(fitmeas, paste0(fitmeas, ".scaled")) else  fitmeas
    observed <- names(lavaan::fitMeasures(models[[1]], fitmeas_f[fitmeas_f %in% names(lavaan::fitMeasures(models[[1]]))]))
    for (i in fitmeas){
        if(paste0(i, ".scaled") %in% observed){
            observed[which(observed == i)] <- paste0(i, ".scaled")
        }
    }
    observed <- observed[!duplicated(observed)]
    orderedMods <- list()
    for (i in nestedPairs[,"nested"]){
        orderedMods[[i]] <- models[[i]]
    }
    modelsums <- lapply(orderedMods, function(x) lavaan::fitMeasures(x, observed))
    sumtable <- do.call(rbind, modelsums)
    sumtable <- apply(sumtable, 2, round, 3)
    sumtable <- data.frame(sumtable)
    if(chidif){
        sumtable[,c("dchi", "ddf", "npval")] <- "-"
        letter <- 1
        noteinfo <- list()
        for (i in rownames(sumtable)){
            comparison <- nestedPairs[which(nestedPairs[,"nested"] == i),]
            if (!is.na(comparison["parent"])){
                tmp <- round(lavaan::anova(models[[comparison["nested"]]], models[[comparison["parent"]]])[2,c("Chisq diff", "Df diff", "Pr(>Chisq)")], 3)
                sumtable[i, c("dchi", "ddf", "npval")] <- tmp
                sumtable[i, "dchi"] <- paste0(sumtable[i, "dchi"], letters[letter])
                noteinfo[[letter]] <- paste0(letters[letter], " = ", comparison["nested"], " .vs ", comparison["parent"])
                letter <- letter + 1
            }
        }
        output <- list(sumtable, unlist(noteinfo))
    }else{
        output <- list(sumtable)
    }
    if(!is.null(file)){
        tableinfo <- output[[1]]
        names(tableinfo) <- gsub(".scaled", "", names(tableinfo))
        texcode <- print(xtable(tableinfo), print.results = FALSE)
        name_old <- c("^chisq$", "^pvalue$", "^dchi$", "^ddf$", "^npval$")
        name_new <- c("$\\\\chi^{2}$", "\\\\textit{p}-value", "$\\\\Delta\\\\chi^{2}$", "$\\\\Delta df$", "\\\\textit{p}")
        texcode <- mgsub(name_old, name_new, texcode)

        if(length(output) > 1){
            texcode <- paste0(texcode, "\n", paste0(output[[2]], collapse = ", "), "\n\n")
        }
        write(texcode, file = file)
    }
    if(!is.null(file)){
        cat(texcode)
        invisible(texcode)
    } else {
        return(output)
    }
}




