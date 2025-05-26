bars_plot <- function(
    plotting_values,
    focus_pool,
    state_toggle,
    input
) {
    # Define the color palette
    color_palette <- c(
        "#1b9e77", # green
        "#d95f02", # orange
        "#7570b3", # purple
        "#e7298a", # pink
        "#66a61e", # olive
        "#e6ab02", # yellow
        "#a6761d", # brown
        "#666666" # gray
    )

    # Prepare data for plotting
    plotting_data <- plotting_values %>%
        dplyr::filter(State %in% focus_pool) %>%
        dplyr::mutate(
            Variable = round(get(input$variable_selector), 2)
        ) %>%
        dplyr::arrange(State) # Alphabetical order

    # Assign colors alphabetically
    state_levels <- sort(unique(plotting_data$State))
    color_map <- setNames(
        color_palette[seq_along(state_levels)],
        state_levels
    )
    plotting_data$State <- factor(
        plotting_data$State,
        levels = state_levels
    )

    ggplot2::ggplot(
        plotting_data,
        ggplot2::aes(
            x = reorder(State, Variable),
            y = Variable,
            fill = State
        )
    ) +
        ggplot2::geom_col(
            width = 0.65,
            show.legend = FALSE,
            color = "grey30",
            alpha = 0.9
        ) +
        ggplot2::geom_text(
            ggplot2::aes(label = paste0(Variable, "%")),
            vjust = -0.5,
            size = 5,
            fontface = "bold",
            color = "black"
        ) +
        ggplot2::scale_fill_manual(values = color_map) +
        ggplot2::labs(
            title = paste(
                "Change in",
                input$variable_selector,
                "(%)"
            ),
            x = NULL,
            y = NULL
        ) +
        ggplot2::theme_minimal(base_size = 15) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = if (state_toggle) 0 else 40,
                hjust = 1,
                vjust = 1,
                size = 14,
                face = "bold"
            ),
            axis.text.y = ggplot2::element_text(size = 13),
            axis.title.y = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(
                size = 16,
                face = "bold",
                hjust = 0.5
            ),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(
                fill = "white",
                color = NA
            ),
            plot.background = ggplot2::element_rect(
                fill = "white",
                color = NA
            )
        ) +
        ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.18))
        )
}
