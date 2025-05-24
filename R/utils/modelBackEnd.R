modelBackEnd <- function(
    trade_data,
    bitrade,
    productivity
) {
    theta_elasticity <- 2.962 # Elasticity parameter

    # Mapping from percentages to model values
    mappingModelNumbers <- function(x) 1 + (x / 100) # Convert percentage to model value
    bitrade <- mappingModelNumbers(bitrade) # Apply mapping to bitrade
    bitrade <- -1 * theta_elasticity * log(bitrade)
    productivity <- mappingModelNumbers(productivity) # Apply mapping to productivity

    # Convert bilateral trade data (matrix) to a vector for processing
    trade_data$bitrade <- as.vector(bitrade)

    # Convert productivity data to a data frame and associate it with origin states
    productivity_df <- as.data.frame(t(productivity)) # Convert to data frame
    productivity_df$orig <- unique(trade_data$orig) # Add origin states
    names(productivity_df) <- c("productivity", "orig") # Rename columns

    # Merge trade data with productivity data by origin states
    trade_data <- merge(trade_data, productivity_df, by = "orig", all.x = TRUE)
    # Run the gravityGE model with the merged data

    simulation_results <- gravityGE::gravityGE(
        trade_data = trade_data, # Input trade data
        theta = theta_elasticity, # Elasticity parameter
        beta_hat_name = "bitrade", # Column name for bilateral trade
        a_hat_name = "productivity", # Column name for productivity
        multiplicative = FALSE # Use additive model
    )

    # Return the simulation results
    return(simulation_results)
}
