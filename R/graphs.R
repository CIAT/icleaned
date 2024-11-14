
wrap_labels <- function(labels, width) {
  # Split labels by spaces and insert line breaks if they exceed the width
  sapply(labels, function(x) {
    paste(strwrap(x, width = width), collapse = "\n")
  })
}

# Define the color palette
color_palette <- c(
  "#005275", "#94C859", "#593D23", "#E5D98B", "#80C5E2", "#009ADB",
  "#28B463", "#C70039", "#900C3F", "#581845", "#1F618D", "#FF5733",
  "#D68910", "#7D3C98", "#F4D03F", "#27AE60", "#2E86C1", "#AF7AC5",
  "#CD6155", "#2874A6"
)

# iCLEANED theme
cleaned_theme <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "#CDCDCD"),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"),
    axis.line = element_line(colour = "#707070", linewidth = 0.5),
    axis.text.y = element_text(
      hjust = 1, # Align text to the right (for x-axis)
      face = "plain", # Normal (not bold or italic)
      size = 12, # Font size in points
      color = "#000000", # Text color
      lineheight = 1.2, # Line height (equivalent to 12px/10px)
      margin = margin(r = 15)
    ),
    axis.text.x = element_text(
      hjust = 1, # Align text to the right (for y-axis)
      face = "plain", # Normal (not bold or italic)
      size = 12, # Font size in points
      color = "#000000", # Text color
      lineheight = 1.2,
      margin = margin(t = 8)
    ),
  )

#' Build char plot of GHG emission
#' 
#' @param data_table data table to plot
plot_ghg_emissions <- function(data_table, unit, png = FALSE) {
  
  # Dynamically wrap x-axis labels
  data_table$sources_and_sinks_formatted <- wrap_labels(
    data_table$sources_and_sinks, width = 10
  )
  
  data_table$bar_tooltip <- paste(
    '<div class="text-center px-1" style="font-size: 13px;">
    <span class="lead fw-bold text-center" style="font-size: 13px;">', 
    round(data_table$t_CO2e_per_ha, 2), "</span>", "<i> ", unit, " </i><br>",
    '<span class="text-center">', data_table$sources_and_sinks, "</span></div>"
  )
  
  # Adding interactive tooltips to the graph
  p <- data_table %>% 
    ggplot(aes(x = sources_and_sinks_formatted, y = t_CO2e_per_ha)) +
    geom_bar_interactive(
      aes(tooltip = bar_tooltip), 
      stat = "identity", fill = "#005275", width = 0.6
    ) + 
    labs(x = "", y = "", title = "") +
    cleaned_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  # Conditionally apply scale_y_continuous if all values are non-negative
  if (all(data_table$t_CO2e_per_ha >= 0)) {
    p <- p + scale_y_continuous(
      expand = expansion(mult = c(0, 0.1))  # No space below, 10% space at top
    )
  } else {
    p <- p + geom_hline(
      yintercept = 0, linetype = "FF", color = "#707070", linewidth = 0.5
    ) # Dashed line at y = 0
  }
  
  if (png) {
    p <- p + labs(title = "GHG Emissions", y = unit)
  }
  
  # Create the interactive plot
  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(
        hidden = c("selection", "zoom", "misc")
      ),
      ggiraph::opts_tooltip(
        use_fill = TRUE
      )
    ),
    width_svg = 7,
    height_svg = 7
  )
}


#' Build char plot of N Balance
#' 
#' @param data_table data table to plot
plot_n_balance <- function(data_table, unit, png = FALSE) {
  
  # Dynamically wrap x-axis labels
  data_table$feed_formatted <- wrap_labels(data_table$feed, width = 13)
  
  data_table$bar_tooltip <- paste(
    '<div class="text-center px-1" style="font-size: 13px;">
    <span class="lead fw-bold text-center" style="font-size: 13px;">', 
    round(data_table$nbalance_kg_n_total, 2), "</span>", "<i> ", unit,
    " </i> <br>", '<span class="text-center">', data_table$feed, "</span></div>"
  )
  
  # Check the number of bars (unique x-axis values)
  num_bars <- length(unique(data_table$feed))
  
  # Set bar width based on the number of bars
  bar_width <- if (num_bars == 1) {
    0.1  # Narrow width for 1 bar
  } else if (num_bars >= 2 && num_bars <= 5) {
    0.27  # Slightly wider for 2-5 bars
  } else if (num_bars >= 6 && num_bars <= 12) {
    0.5  # Moderate width for 6-12 bars
  } else {
    0.4  # Default width for 13-20 bars
  }
  
  p <- data_table %>%
    ggplot2::ggplot(aes(x = feed_formatted, y = nbalance_kg_n_total)) +
    geom_bar_interactive(
      aes(tooltip = bar_tooltip), 
      stat = "identity", fill = "#005275", width = bar_width
    ) +
    geom_bar(stat = "identity", fill = "#005275", width = bar_width) +
    labs(x = "", y = "", title = "") +
    cleaned_theme +
    theme(
      axis.text.x = element_text(
        angle = 50, hjust = 1, vjust = 1, lineheight = 0.7
      )
    )
  
  # Conditionally apply scale_y_continuous if all values are non-negative
  if (all(data_table$nbalance_kg_n_total >= 0)) {
    p <- p + scale_y_continuous(
      expand = expansion(mult = c(0, 0.1))  # No space below, 10% space at top
    )
  } else {
    p <- p + geom_hline(
      yintercept = 0, linetype = "FF", color = "#707070", linewidth = 0.5
    ) # Dashed line at y = 0
  }
  
  # if PNG then add title and unit
  if (png) {
    p <- p + labs(title = "N Balance", y = unit)
  }
  
  # Create the interactive plot
  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(
        hidden = c("selection", "zoom", "misc")
      ),
      ggiraph::opts_tooltip(
        use_fill = TRUE
      )
    ),
    width_svg = 7,
    height_svg = 7
  )
}

#' Build char plot of land required
#' 
#' @param data_table data table to plot
plot_land_req <- function(data_table, unit, png = FALSE) {
  
  # Dynamically wrap x-axis labels
  data_table$feed_formatted <- wrap_labels(data_table$feed, width = 13)
  
  # Get unique season names
  unique_seasons <- unique(data_table$season_name)
  
  # Assign colors to each season based on the order of unique seasons
  season_colors <- setNames(
    color_palette[seq_along(unique_seasons)], unique_seasons
  )
  
  data_table$bar_tooltip <- paste(
    '<div class="text-center px-1" style="font-size: 13px;">
    <span class="lead fw-bold text-center" style="font-size: 13px;">', 
    round(data_table$area_feed_total, 2), "</span>", "<i> ", unit, " </i> <br>",
    '<span class="text-center">', data_table$feed, "</span> <br>",
    'Season: <span class="lead fw-bold text-center" style="font-size: 13px;">',
    data_table$season_name, "</span></div>"
  )
  
  # now I wnat to add the season name in the bar tooltip
  
  # Check the number of bars (unique x-axis values)
  num_bars <- length(unique(data_table$feed))
  
  # Set bar width based on the number of bars
  bar_width <- if (num_bars == 1) {
    0.1  # Narrow width for 1 bar
  } else if (num_bars >= 2 && num_bars <= 5) {
    0.27  # Slightly wider for 2-5 bars
  } else if (num_bars >= 6 && num_bars <= 12) {
    0.5  # Moderate width for 6-12 bars
  } else {
    0.4  # Default width for 13-20 bars
  }
  
  p <- data_table %>%
    ggplot(aes(x = feed_formatted, y = area_feed_total, fill = season_name)) +
    geom_bar_interactive(
      aes(tooltip = bar_tooltip), 
      stat = "identity", width = bar_width
    ) +
    labs(x = "", y = "", fill = "Seasons", title = "") +
    cleaned_theme +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        angle = 50, hjust = 1, vjust = 1, lineheight = 0.7
      )
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.1))  # No space below, 10% space at top
    ) +
    scale_fill_manual(values = season_colors)  # Set bar colors
  
  # if PNG then add title, unit and legend
  if (png) {
    p <- p + labs(title = "Land Required", y = unit) + 
      theme(legend.position = "right")
  }
  
  
  # Create the interactive plot
  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(
        hidden = c("selection", "zoom", "misc")
      ),
      ggiraph::opts_tooltip(
        use_fill = TRUE
      )
    ),
    width_svg = 7,
    height_svg = 7
  )
}

#' Build char plot of Water per Feed
#' 
#' @param data_table data table to plot
plot_water_feed <- function(data_table, unit, png = FALSE) {
  
  feed_water_percentage <- data_table %>%
    mutate(percentage = feed_water_use / sum(feed_water_use)) %>%
    pull(percentage)
  
  data_table$bar_tooltip <- paste(
    '<div class="text-center px-1" style="font-size: 13px;">
    <span class="lead fw-bold text-center" style="font-size: 13px;">', 
    round(feed_water_percentage * 100, 2), "</span>", "<i> ", unit, "</i> <br>",
    '<span class="text-center">', data_table$feed, "</span></div>"
  )
  
  # Create the interactive plot
  p <- data_table %>%
    ggplot(aes(x = "", y = feed_water_use, fill = feed)) +
    geom_col_interactive(
      aes(
        color = feed, 
        tooltip = bar_tooltip # Add tooltips
      )
    ) +
    coord_polar("y", start = 0) +
    labs(x = "", y = "", title = "") +
    
    geom_text_interactive(
      aes(x = 1.65, 
          label = scales::percent(feed_water_use / sum(feed_water_use)), 
          color = feed, 
          tooltip = bar_tooltip),  # Add tooltips for text
      position = position_stack(vjust = 0.5),
      size = 6,
      family = "Merriweather",
      fontface = "bold"
    ) +
    # Apply the palette to the bar fill
    scale_fill_manual(values = color_palette) +
    # Apply the palette to the text color
    scale_color_manual(values = color_palette)
  
  # if PNG then add title, unit and legend
  if (png) {
    p <- p + labs(x = "", y = "", title = "Water Use per Feed") + 
      theme(
        legend.position = "bottom",
        axis.text.x = element_blank(), # Remove x-axis text for the pie chart
        axis.text.y = element_blank()
      )
    p
  } else {
    p <- p + theme_void() +
      theme(
        legend.position = "none", # Remove legend
        axis.text.x = element_blank(), # Remove x-axis text for the pie chart
        axis.text.y = element_blank(),
        plot.margin = unit(c(-.75, -.75, -.75, -.75), "cm")
      )
  }
  
  # Create the interactive plot
  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(
        hidden = c("selection", "zoom", "misc")
      ),
      ggiraph::opts_tooltip(
        use_fill = TRUE
      )
    ),
    width_svg = 11,
    height_svg = 8
  )
}

#' Build char plot for scenario comparison
#' 
#' @param data_table data table to plot
plot_comparison <- function(
    datos, tt, y_title, title, unit, png = FALSE, relative = FALSE
) {
  
  # Check the number of bars (unique x-axis values)
  num_bars <- length(unique(datos[, 2]))
  
  datos$bar_tooltip <- paste(
    '<div class="text-center px-1" style="font-size: 13px;">
    <span class="lead fw-bold text-center" style="font-size: 13px;">', 
    round(datos[[tt]], 2), "</span> <i>",
    ifelse(relative, "%", unit),
    "</i> <br>",
    '<span class="text-center">', datos[["scenario"]], "</span></div>"
  )
  
  # Set bar width based on the number of bars
  bar_width <- if (num_bars == 1) {
    0.1  # Narrow width for 1 bar
  } else if (num_bars >= 2 && num_bars <= 5) {
    0.2  # Slightly wider for 2-5 bars
  } else if (num_bars >= 6 && num_bars <= 12) {
    0.5  # Moderate width for 6-12 bars
  } else {
    0.4  # Default width for 13-20 bars
  }
  
  # divide values by 100 if relative
  if (relative) {
    datos[[tt]] <- datos[[tt]] / 100
  }
  
  p <- ggplot(datos, aes_string("scenario", y = tt)) + 
    geom_bar_interactive(
      aes(tooltip = bar_tooltip), 
      stat = "identity", fill = "#005275", width = bar_width
    ) +
    labs(x = "", y = "", title = "") +
    cleaned_theme +
    theme(axis.text.x = element_text(hjust = 0.5, vjust = 1))
  
  # Conditionally apply scale_y_continuous if all values are non-negative
  if (all(datos[, tt] >= 0, na.rm = TRUE)) {
    p <- p + scale_y_continuous(
      expand = expansion(mult = c(0, 0.1)),  # No space below, 10% space at top
      labels = ifelse(relative, scales::percent, scales::comma)
    )
  } else {
    p <- p +
      geom_hline(
        yintercept = 0, linetype = "FF", color = "#707070", linewidth = 0.5
      ) + # Dashed line at y = 0
      scale_y_continuous(labels = ifelse(
        relative, scales::percent, scales::comma
      ))
  }
  
  # if plot is made for png, then add title and unit
  if (png) {
    p <- p + labs(title = title, y = unit)
  }
  
  # Create the interactive plot
  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(
        hidden = c("selection", "zoom", "misc")
      ),
      ggiraph::opts_tooltip(
        use_fill = TRUE
      )
    ),
    width_svg = 7,
    height_svg = 7
  )
}
