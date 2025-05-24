# Helper function to determine join key for shapefile and plotting values
getJoinKey <- function(plotting_values) {
    if ("AL" %in% plotting_values$State) {
        c("Code" = "State")
    } else {
        c("Name" = "State")
    }
}
