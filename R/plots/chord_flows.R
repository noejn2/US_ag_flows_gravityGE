chord_flows <- function(
    plotting_values,
    focus_pool,
    other_selection,
    net_exports,
    self_selection
) {
    plotting_values$Origin[
        !plotting_values$Origin %in% focus_pool
    ] <- "Other"
    plotting_values$Destination[
        !plotting_values$Destination %in% focus_pool
    ] <- "Other"

    # Define color palette: 8 distinct colors + 1 for 'Other'
    color_palette <- c(
        "#1b9e77", # green
        "#d95f02", # orange
        "#7570b3", # purple
        "#e7298a", # pink
        "#66a61e", # olive
        "#e6ab02", # yellow
        "#a6761d", # brown
        "#666666", # gray
        "#cccccc" # light gray for 'Other'
    )

    # Get unique regions (up to 8) and always put 'Other' last
    unique_regions <- unique(c(
        plotting_values$Origin,
        plotting_values$Destination
    ))
    unique_regions <- unique_regions[unique_regions != "Other"]
    # unique_regions <- head(unique_regions, 8)
    region_levels <- c(unique_regions, "Other")

    # Set factor levels to ensure color mapping is consistent
    plotting_values$Origin <- factor(
        plotting_values$Origin,
        levels = region_levels
    )
    plotting_values$Destination <- factor(
        plotting_values$Destination,
        levels = region_levels
    )

    plotting_values <- plotting_values %>%
        dplyr::group_by(Origin, Destination) %>%
        dplyr::summarise(
            Trade = sum(Trade),
            simTrade = sum(simTrade)
        ) %>%
        dplyr::ungroup()

    plotting_values <- plotting_values %>%
        dplyr::select(
            Origin,
            Destination,
            Trade,
            simTrade
        ) %>%
        dplyr::group_by(Origin, Destination) %>%
        dplyr::summarise(simTrade = sum(simTrade), .groups = "drop")

    # Choose to keep or not the 'Other' category
    if (!other_selection) {
        plotting_values <- plotting_values %>%
            dplyr::filter(Origin != "Other") %>%
            dplyr::filter(Destination != "Other")
    }

    # Visualize trade flows using a chord diagram
    # Prepare matrix for chordDiagram
    trade_matrix <- plotting_values %>%
        tidyr::pivot_wider(
            names_from = Destination,
            values_from = simTrade,
            # values_from = net,
            values_fill = 0
        ) %>%
        as.data.frame()

    # Ensure unique row names
    trade_matrix <- trade_matrix %>%
        dplyr::group_by(Origin) %>%
        dplyr::summarise(
            across(everything(), sum),
            .groups = "drop"
        )

    rownames_ls <- trade_matrix$Origin
    trade_matrix <- trade_matrix[, -1]
    trade_matrix <- as.matrix(trade_matrix)
    dimnames(trade_matrix) <- list(
        exporter = rownames_ls,
        importer = rownames_ls
    )

    if (!net_exports) trade_matrix <- netTrade(trade_matrix)
    if (!self_selection) diag(trade_matrix) <- 0

    # Assign colors to regions, always mapping 'Other' to last color in palette
    if (!other_selection) {
        n_regions <- length(region_levels) - 1
        region_colors <- color_palette[seq_len(n_regions)]
    } else {
        n_regions <- length(region_levels)
        region_colors <- color_palette[c(
            seq_len(n_regions),
            length(color_palette)
        )]
    }

    # Plot chord diagram
    return(chorddiag::chorddiag(
        trade_matrix,
        groupColors = region_colors
    ))
}
