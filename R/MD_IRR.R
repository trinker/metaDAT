#' Percent Inter-Rater Reliabilitiy
#'
#' Percent inter-rater reliabilitiy by variable.
#' 
#' @param meta.obj.1 An MDlist (an object from \code{read.MD}) for time 1 
#' (primary coding).
#' @param meta.obj.2 An MDlist (an object from \code{read.MD}) for time 2 
#' (secondary coding).
#' @param exclude.vars A character vector of variables to exclude from reliability 
#' check.
#' @param cor.var An optional argument for the MDlist variable that is a list of 
#' correlation matrices (can detect and handle correlation variables that are in 
#' character correlation triangle format).
#' @param comp.var The variable to compare for discrepencies between the two MDlists.  
#' Default is "coder".  This argument is particularly useful when there are more 
#' than two coders.
#' @param file Optional connection, or a character string naming the file to print to.
#' @param order.by.comp.var logical.  If TRUE orders the study disagreements by the 
#' comparison variable.
#' @return Returns a list:
#' \item{Variable_Agreement_Rate}{A dataframe of percentage agreement/disagreement 
#' for each variable.  Final row is overall agreement rate.} 
#' \item{Disagreements}{A list of disagreements of study variables between comparison 
#' groups (time 1 and time 2) per study.  If correlations are being compared the 
#' function outputs an agreement matrix.  Agreement is marked as \code{-} and 
#' disagreement as \code{X}.}
#' @note This function assumes double coding, that is all studies have been coded by 
#' two different coders.
#' @keywords inter-rater reliability
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat1 <- read.MD(path)
#' path2 <- system.file("extdata/bibTest2.bib", package = "metaDAT")
#' dat2 <- read.MD(path2)
#' MD_IRR(dat1, dat2, cor.var = "correlation_triangle")
#' # MD_IRR(dat1, dat2, cor.var = "correlation_triangle", file = "foo.txt")
#' # unlink("foo.txt", recursive = TRUE, force = FALSE) #delete previous file
MD_IRR <- function(meta.obj.1, meta.obj.2, exclude.vars = c("author", "ID", "time"), 
    cor.var = NULL, comp.var = "coder", file = NULL, order.by.comp.var = TRUE) {
    metas <- list(meta.obj.1, meta.obj.2)
    studs <- lapply(metas, function(x) sort(names(x)))
    if (!all(studs[[1]] == studs[[2]])) {
        stop("Study names for meta.obj.1 & meta.obj.2 do not correspond.")
    }
    vars <- unique(unlist(lapply(metas, MDnames)))
    vars <- vars[!vars %in% c(exclude.vars, comp.var, cor.var)]
    output <- invisible(lapply(vars, function(x) {
        var1 <- MDget(metas[[1]], vars = x, s.names = TRUE)
        var2 <- MDget(metas[[2]], vars = x, s.names = TRUE)
        comps <- unlist(lapply(seq_along(var1), function(i) {       
            identical(var1[i], var2[i])
        }))
        agree <- 100 * round(sum(comps)/length(comps), 4)
        list(agree = agree, disagree = 100 - agree, raw = comps)
    }))
    names(output) <- vars
    rates <- lapply(output, "[", 1:2)
    raws <- unlist(lapply(output, "[", 3), recursive = FALSE)
    names(raws) <- gsub(".raw", "", names(raws))
    if(!is.null(cor.var)) {
        cors <- invisible(lapply(metas, function(x) {
            MDget(x, vars = cor.var, s.names = TRUE)
        }))
        type <- rapply(cors, class) %in% c("matrix", "full_matrix", "incomplete_matrix") 
        if (sum(type) == 0) {
            cors <- lapply(cors, function(x) lapply(x, MDlist2matrix))
        } else {
            if (sum(type) == length(cors)) {
                cors <- cors
            } else { 
                stop("different matrix types supplied to cor.var")
            }
        }
        vect.list <- invisible(lapply(cors, function(x) lapply(x, matrix2vectors)))
        vect.list <- invisible(lapply(vect.list, function(x) lapply(x, function(z) {
            z[, "vars"] <- paste2(z[, 1:2], sep = "_")    
            z
        })))
        cor.agree <- invisible(lapply(seq_along(vect.list[[1]]), function(i){
            mvar <- merge(vect.list[[1]][[i]], vect.list[[2]][[i]][, 3:4], "vars")
            cor.ag <- invisible(sapply(1:nrow(mvar), function(i) {
                identical(mvar[i, 4], mvar[i, 5])
            }))
            names(cor.ag) <- mvar[, 1]
            mvar[, "agree"] <- cor.ag
            mvar[, "agree2"] <- ifelse(cor.ag, "-", "X")
            mvar
        }))
        names(cor.agree) <- studs[[1]]
        agree.mats <- invisible(lapply(cor.agree, function(x) {
            if (all(x[, 6])) { 
                return(NULL) 
            }
            z <- vectors2matrix(x[, -c(1, 4:6)])
            diag(z) <- "-"
            noquote(z)
        }))
        unagree.cors <- agree.mats[!sapply(agree.mats, is.null)]
        agree.logical <- do.call(rbind, cor.agree)[, "agree"]
        agree <- 100 * round(sum(agree.logical)/length(agree.logical), 4)
        cor.rate <- list(agree = agree, disagree = 100 - agree)
        rates[["correlations"]] <- cor.rate
        raws[["correlations"]] <- unlist(lapply(cor.agree, "[", 6))        
    }
    rates <- do.call(rbind, rates)
    raws <- unlist(raws)
    overall <- 100*round(sum(raws)/length(raws), 4)
    overall[2] <- 100 - overall
    names(overall) <- colnames(rates)
    rates <- rbind(rates, overall)
    meta.obj.1b <- invisible(lapply(meta.obj.1, function(x) {
        keeps <- x[vars]
        keeps[!sapply(keeps, is.null)]
    }))
    meta.obj.2b <- invisible(lapply(meta.obj.2, function(x) {
        keeps <- x[vars]
        keeps[!sapply(keeps, is.null)]
    }))
    metas2 <- list(meta.obj.1b, meta.obj.2b)
    stud.comp <- invisible(lapply(seq_along(studs[[1]]), function(i) {
        x <- metas2[[1]][[i]]
        y <- metas2[[2]][[i]]
        xdat <- data.frame(vars = names(x), value = unlist(x), row.names=NULL)
        ydat <- data.frame(vars = names(y), value = unlist(y), row.names=NULL)
        mdat <- merge(xdat, ydat, "vars")
        stud.agree <- invisible(sapply(1:nrow(mdat), function(i) {
            identical(mdat[i, 2], mdat[i, 3])
        }))
        names(stud.agree) <- mdat[, 1]
        stud.agree <- stud.agree[!stud.agree]
        if (identical(unname(stud.agree), logical(0))) {
            NULL
        } else {
            names(stud.agree)
        }
    }))
    names(stud.comp) <- studs[[1]]
    stud.comp <- invisible(lapply(stud.comp, function(x) {
       if (is.null(x)) {
           NULL
       } else {
           paste(x, collapse = ", ")
       }
    }))
    if(!is.null(cor.var)) {
        stud.comp <- invisible(lapply(seq_along(stud.comp), function(i) list(variables = stud.comp[[i]], 
            correlations = agree.mats[[i]])))
        names(stud.comp) <- studs[[1]]
    }
    if(!is.null(comp.var)) {
        nms <- sapply(metas, function(x) MDget(x, comp.var))
        comp <- data.frame(studies = studs[[1]], nms)
        names(stud.comp) <- paste2(comp, sep="__")    
        if (order.by.comp.var) {
            comp <- comp[order(comp$X1, comp$X2, comp$studies),]
            stud.comp <- stud.comp[paste2(comp, sep="__")]
        }    
    }
    stud.comp <- stud.comp[!sapply(stud.comp, function(x) {
            all(sapply(x, is.null))
        })]
    stud.comp <- sapply(stud.comp, function(x) {
        x[!sapply(x, is.null)]
    })
    OUT <- list(Variable_Agreement_Rate = rates, Disagreements = stud.comp)
    if (!is.null(file)) {
        if(!(c(grepl("/", file) & grepl("/", file)))) {
            file2 <- paste0(getwd(), "/", file)    
        } else {
            file2 <- file
        }
        cat("Variable Agreement Rate:\n\n", file = file2)
        sink(append = TRUE, file=file2, type = "output")
        print(OUT[[1]])
        sink()
        cat(paste0("\n", paste(rep("=", 50), collapse="")), "\n\n", 
            file = file2, append = TRUE) 
        cat("Disagreements:\n\n", file = file2, append = TRUE) 
        sink(append = TRUE, file = file2, type = "output")
        print(OUT[[2]])
        sink()
    }
    OUT
}








