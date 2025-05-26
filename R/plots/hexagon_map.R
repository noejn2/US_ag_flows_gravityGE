hexagon_map <- function(
    plotting_values,
    focus_pool,
    clicked_info,
    shp_file,
    input
) {
    # Join the filtered values with the shapefile data
    shp_file_joined <- dplyr::left_join(
        shp_file,
        plotting_values,
        by = getJoinKey(plotting_values)
    ) %>%
        dplyr::mutate(
            State = if ("AL" %in% plotting_values$State) {
                Code
            } else {
                Name
            }
        ) # Add a new column for state names

    # Plot the map with the selected variable
    shp_file_joined %>%
        dplyr::mutate(
            "Variable" = round(
                get(input$variable_selector),
                2
            ),
            line_size = ifelse(
                State %in%
                    c(
                        focus_pool,
                        clicked_info$Name,
                        clicked_info$Code
                    ),
                1.5,
                .3
            ),
            line_color = ifelse(
                State %in% focus_pool,
                # Use the same color palette as bars, mapped to State
                c(
                    "#1b9e77",
                    "#d95f02",
                    "#7570b3",
                    "#e7298a",
                    "#66a61e",
                    "#e6ab02",
                    "#a6761d",
                    "#666666"
                )[match(State, sort(unique(focus_pool)))],
                "grey70"
            )
        ) %>%
        ggplot2::ggplot() +
        ggplot2::geom_sf(
            ggplot2::aes(
                fill = Variable,
                linewidth = line_size,
                color = line_color
            )
        ) +
        ggplot2::scale_linewidth_identity() +
        ggplot2::geom_sf_text(
            ggplot2::aes(label = Code),
            size = 3,
            color = "black"
        ) +
        ggplot2::scale_fill_gradient2(
            low = "brown",
            mid = "lightgray",
            high = "darkgreen",
            midpoint = 0,
            na.value = "grey90",
            guide = ggplot2::guide_colorbar(
                position = "inside",
                title = paste(
                    "Change in",
                    input$variable_selector,
                    "(%)"
                ),
                title.position = "top",
                title.hjust = 0.5,
                barwidth = 20,
                barheight = 0.5
            )
        ) +
        ggplot2::scale_color_identity() +
        ggplot2::coord_sf(
            xlim = c(-123, -68),
            ylim = c(20, 60),
            expand = FALSE
        ) +
        ggplot2::labs(
            title = NULL,
            x = NULL,
            y = NULL,
            caption = "Click a state for details."
        ) +
        ggplot2::theme_minimal(base_size = 16) +
        ggplot2::theme(
            aspect.ratio = 0.7,
            legend.position = c(0.4, 0.9),
            legend.direction = "horizontal",
            legend.title = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size = 13),
            legend.background = ggplot2::element_rect(
                fill = "transparent",
                color = NA
            ),
            legend.key = ggplot2::element_rect(
                fill = "transparent",
                color = NA
            ),
            axis.text.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            plot.title = ggplot2::element_blank(),
            plot.caption = ggplot2::element_text(
                size = 12,
                hjust = 0.5,
                color = "#888888",
                margin = ggplot2::margin(t = 10)
            ),
            plot.margin = ggplot2::margin(10, 10, 30, 10),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(
                fill = "white",
                color = NA
            ),
            plot.background = ggplot2::element_rect(
                fill = "white",
                color = NA
            )
        ) +
        ggplot2::annotate(
            "text",
            x = -119,
            y = 25,
            label = if (!is.null(clicked_info)) {
                paste0(
                    clicked_info$Name,
                    " (",
                    clicked_info$Code,
                    ")",
                    "\n",
                    input$variable_selector,
                    ": ",
                    if (is.numeric(clicked_info$Expenditure)) {
                        round(
                            clicked_info[[
                                input$variable_selector
                            ]],
                            2
                        )
                    } else {
                        0
                    },
                    " %"
                )
            } else {
                ""
            },
            size = 5.5,
            fontface = "bold",
            hjust = 0,
            vjust = 1,
            color = "#34495e"
        )
}
