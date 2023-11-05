theme_int_dosing = function(base_size = 12,
                            aspect=.9,
                            #base_family = "Arial",
                            legend_direction = "vertical",
                            legend = c("right", "top", "bottom", "left", "none")) {
  library(ggthemes)
  if (!is.numeric(legend))
    legend <- match.arg(legend)
  
  quart_line <- base_size / 4
  axis.line <- element_line(color = "#222222")
  plot.margin <- margin(quart_line, quart_line, quart_line, quart_line, "mm")
  .theme <- theme_foundation(base_size = base_size)
    .theme + theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.title = element_text(size = rel(1), color = "#222222", face = "bold", hjust = 0.5),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = plot.margin,
    axis.line = axis.line,
    axis.text = element_text(color = "#424242", size = rel(.6)),
    axis.title = element_text(size = rel(.8), color = "#323232"),
    axis.ticks = element_line(color = "#222222"),
    legend.key.size = unit(rel(25), 'pt'),
    legend.key = element_blank(),
    legend.position = legend,
    legend.direction = legend_direction,
    legend.background = element_blank(),
    legend.text = element_text(color = "#222222", size = rel(.83)),
    legend.title = element_text(color = "#222222", size = base_size),
    aspect.ratio = aspect
  )
}