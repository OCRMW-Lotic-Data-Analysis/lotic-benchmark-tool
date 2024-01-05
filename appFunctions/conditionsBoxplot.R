conditionsBoxplot <- function(benchmark) {
  
  selectedVars <- c(benchmark, paste0(v, "Condition"))
  
  plotdat <- dat %>% st_drop_geometry() %>% select(any_of(selectedVars)) %>%
    pivot_longer(cols = -selectedVars[2], names_to = "indicator", values_to = "value") %>%
    dplyr::rename(condition = selectedVars[2])
  
  p <- ggplot(plotdat, aes(x = indicator, y = value)) + 
    ggdist::stat_halfeye(
      adjust = .5, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
    geom_boxplot(
      width = .25, 
      outlier.shape = NA
    ) +
    geom_point(
      aes(color = condition),
      size = 3,
      alpha = .7,
      position = position_jitter(
        seed = 1, width = .1
      )
    ) + 
    coord_cartesian(xlim = c(1.2, 1.3), clip = "off") +
    theme_minimal()
  
  p
}