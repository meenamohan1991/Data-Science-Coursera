rankall <- function(outcomeChr, rankObj = "best") {
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

    # --- Check that outcome is valid
    if (nrow(inputDfr[inputDfr$Outcome == outcomeChr, ]) == 0) 
        stop("invalid outcome")

    # --- Assert create an empty vector Add column rank for debug
    nameChr <- character(0)
    # rankChr <- character(0)

    # --- Return hospital name in that state with the ranked THIRTY(30)-day
    # death rate Create a data frame with given ONE (1) state Determine the
    # relevant column Reorder the new data frame from best to worst
    for (stateChr in tableDfr$State) {
        stateDfr <- outcomeDfr[outcomeDfr$State == stateChr, ]
        colNum <- inputDfr[inputDfr$Outcome == outcomeChr, 2]
        stateDfr <- stateDfr[complete.cases(stateDfr[, colNum]), ]
        stateDfr <- stateDfr[order(stateDfr[, colNum], stateDfr$Hospital.Name), 
            ]

        # --- Convert 'best' and 'worst' to numeric Determine the relevant row
        if (rankObj == "best") 
            rankNum <- 1 else if (rankObj == "worst") 
            rankNum <- nrow(stateDfr) else suppressWarnings(rankNum <- as.numeric(rankObj))

        # --- Append hospital name to character vector
        nameChr <- c(nameChr, stateDfr[rankNum, ]$Hospital.Name)
        # rankChr <- c( rankChr, rankNum )
    }

    # --- Return value is a data frame (hospital, state)
    return(data.frame(hospital = nameChr, state = tableDfr$State))
}
