# Combined function: updates both trade and welfare outputs for state name/initial toggling
mappingOfStates <- function(
    output,
    state_toggle,
    lookup_table,
    simulation_results = TRUE
) {
    map_states <- function(data, column, direction) {
        if (direction == "to_short") {
            data <- dplyr::left_join(
                data,
                lookup_table,
                by = setNames("full_name", column)
            )
            data[[column]] <- data$short_name
            data$short_name <- NULL
        } else if (direction == "to_full") {
            data <- dplyr::left_join(
                data,
                lookup_table,
                by = setNames("short_name", column)
            )
            data[[column]] <- data$full_name
            data$full_name <- NULL
        } else {
            stop("Invalid direction. Use 'to_short' or 'to_full'.")
        }
        data
    }
    if (simulation_results) {
        if (state_toggle) {
            output$new_trade <- map_states(output$new_trade, "orig", "to_short")
            output$new_trade <- map_states(output$new_trade, "dest", "to_short")
            output$new_welfare <- map_states(
                output$new_welfare,
                "orig",
                "to_short"
            )
        } else {
            output$new_trade <- map_states(output$new_trade, "orig", "to_full")
            output$new_trade <- map_states(output$new_trade, "dest", "to_full")
            output$new_welfare <- map_states(
                output$new_welfare,
                "orig",
                "to_full"
            )
        }
        return(output)
    }

    if (state_toggle) {
        output <- map_states(output, "orig", "to_short")
        output <- map_states(output, "dest", "to_short")
    } else {
        output <- map_states(output, "orig", "to_full")
        output <- map_states(output, "dest", "to_full")
    }
    return(output)
}
