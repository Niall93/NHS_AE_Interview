library(ggplot2)

custom_ggplot_theme <- function() {
  theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major.x = element_blank(),  # Remove x-gridlines
      panel.grid.minor.x = element_blank(),  # Remove x-gridlines
      panel.grid.major.y = element_line(color = "lightgrey"),  # Keep y-gridlines
      panel.grid.minor.y = element_blank(),  # Remove minor y-gridlines
      axis.line = element_line(color = "darkgrey"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      axis.title.y = element_text(angle = 0, hjust = 0, vjust = 1),  # Y-axis label at top-left
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
}

custom_fill_colors <- c("#12436D", "#28A197", "#801650", "#F46A25", "#3D3D3D", "#A285D1")

apply_custom_theme <- function(p) {
  p +
    custom_ggplot_theme() +
    scale_y_continuous(expand = expansion(mult = c(0, 0)))  # Y-axis starts at 0
}

# Example usage
example_plot <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  labs(y = "Y Axis Label", x = "Cylinders", title = "Example Plot")

# Apply the custom theme and color scheme
example_plot <- apply_custom_theme(example_plot)

# Print the plot
print(example_plot)
