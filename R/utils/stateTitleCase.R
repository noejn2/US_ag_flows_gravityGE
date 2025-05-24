# Function to return state initials or full state names based on a toggle
stateTitleCase <- function(x, statesINI_list, states_list) {
    if (is.null(x)) x <- FALSE # Default to FALSE if input is NULL
    if (x) return(statesINI_list) # Return state initials if toggle is TRUE
    return(states_list) # Return full state names if toggle is FALSE
}
