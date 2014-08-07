rankhospital <- function(stateChr, outcomeChr, rankObj) {
    # --- Init loading outcome data
    outcomeDfr <- read.csv("outcome-of-care-measures.csv")

    # --- Coerce character into numeric
    suppressWarnings(outcomeDfr[, 11] <- as.numeric(outcomeDfr[, 11]))
    suppressWarnings(outcomeDfr[, 17] <- as.numeric(outcomeDfr[, 17]))
    suppressWarnings(outcomeDfr[, 23] <- as.numeric(outcomeDfr[, 23]))

    # --- Create a data frame of freq by state Remove row.names
    tableDfr <- data.frame(State = names(tapply(outcomeDfr$State, outcomeDfr$State, 
        length)), Freq = tapply(outcomeDfr$State, outcomeDfr$State, length))
    rownames(tableDfr) <- NULL

    # --- Create a data frame of possible inputs and respective columns
    inputDfr <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
        Col = c(11, 17, 23))

    # --- Check that state and outcome are valid
    if (nrow(tableDfr[tableDfr$State == stateChr, ]) == 0) 
        stop("invalid state")
    if (nrow(inputDfr[inputDfr$Outcome == outcomeChr, ]) == 0) 
        stop("invalid outcome")

    # --- Return hospital name in that state with the ranked THIRTY(30)-day
    # death rate Create a data frame with given ONE (1) state Determine the
    # relevant column Reorder the new data frame from best to worst
    stateDfr <- outcomeDfr[outcomeDfr$State == stateChr, ]
    colNum <- inputDfr[inputDfr$Outcome == outcomeChr, 2]
    stateDfr <- stateDfr[complete.cases(stateDfr[, colNum]), ]
    stateDfr <- stateDfr[order(stateDfr[, colNum], stateDfr$Hospital.Name), 
        ]

    # --- Convert 'best' and 'worst' to numeric 'Worst' code is not valid if
    # omit NA from results Determine the relevant row
    if (rankObj == "best") 
        rankObj <- 1
    if (rankObj == "worst") 
        rankObj <- nrow(stateDfr)
    # if( rankObj=='worst' ) rankObj <- tableDfr[tableDfr$State==stateChr, 2]
    suppressWarnings(rankNum <- as.numeric(rankObj))

    # --- Return value is a character Return data frame for debug
    return(stateDfr[rankNum, ]$Hospital.Name)
    # return(stateDfr)
}
