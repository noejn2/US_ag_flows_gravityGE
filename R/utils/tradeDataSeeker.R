# Function to load trade data from an RDS file
tradeDataSeeker <- function() {
    # Check if the trade data file exists
    if (!file.exists("data/trade_data.rds"))
        stop("Trade data file not found. Please check the file path.") # Stop if file is missing

    # Load and return the trade data
    return(readRDS("data/trade_data.rds"))
}
