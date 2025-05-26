# Calculating net trade
netTrade <- function(x) {
    n <- x
    n[lower.tri(n)] <- x[lower.tri(x)] - x[upper.tri(x)]
    n[upper.tri(n)] <- x[upper.tri(x)] - x[lower.tri(x)]
    n[n < 0] <- 0
    n
}
