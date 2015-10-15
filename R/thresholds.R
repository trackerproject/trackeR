## FIXME: include example for variable as a data frame.
#' Thresholding for variables in \code{trackeRdata} objects.
#' 
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param variable A vector containing the names of the variables to which thresholding is applied. See Details.
#' @param lower A vector containing the corresponding lower thresholds.
#' @param upper A vector containing the corresponding upper thresholds.
#' @param ... Currently not used.
#' @details Argument \code{variable} can also be a data frame containing the variable names, lower, and upper thresholds.
#' @export
threshold.trackeRdata <- function(object, variable, lower, upper, ...){
    ## if variable is NULL, just update attribute, leave data unchanged
    if (is.null(variable)) {
        operations <- getOperations(object)
        operations$threshold <- NULL
        attr(object, "operations") <- operations
        return(object)
    }
    
    ## new thresholds
    if (is.data.frame(variable)){
        th <- variable
    } else {
        th <- data.frame(variable = variable, lower = lower, upper = upper)
    }

    ## compare with existing thresholds
    operations <- getOperations(object)
    if (!is.null(operations$threshold)){
        th <- merge(th, operations$threshold, by = "variable", all = TRUE)
        th$lower <- apply(th[,c("lower.x", "lower.y")], 1, max, na.rm = TRUE)
        th$upper <- apply(th[,c("upper.x", "upper.y")], 1, min, na.rm = TRUE)
    }

    ## apply thresholds
    for (i in 1:nrow(th)){
        v <- as.character(th$variable[i])
        for (session in seq_along(object)){
            wL <- which(object[[session]][,v] < th$lower[i])
            object[[session]][wL,v] <- NA ## th$lower[i] ## set to boundary value or to NA?
            wU <- which(object[[session]][,v] > th$upper[i])
            object[[session]][wU,v] <- NA ## th$upper[i]
        }
    }

    ## update attribute
    operations$threshold <- th[, c("variable", "lower", "upper")]
    attr(object, "operations") <- operations
    
    return(object)
}


generateDefaultThresholds <- function(cycling = FALSE, ...){
    th <- generateBaseUnits(cycling)
    th <- th[-which(th$variable == "duration"),]
    ## FIXME: tighter limits?
    if (cycling) {
        th$lower <- c(-90, -180, -500, 0, 0, 0, 0, 0)
        th$upper <- c(90, 180, 9000, Inf, 250, 100, Inf, Inf) 
    } else {
        th$lower <- c(-90, -180, -500, 0, 0, 0, 0, 0)
        ##th$upper <- c(90, 180, 9000, Inf, 250, 20, Inf, Inf)
        th$upper <- c(90, 180, 9000, Inf, 250, 12.5, Inf, Inf)
    }
    class(th) <- c("trackeRthresholds", class(th))
    return(th)
}

