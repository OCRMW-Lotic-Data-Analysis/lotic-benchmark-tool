conditions_boxplot <- function(reachCond, benchmark) {
  
  selectedVars <- c(benchmark, paste0(benchmark, "Condition"))
  
  plotdat <- reachCond %>% 
    st_drop_geometry() %>% 
    select(any_of(selectedVars)) %>%
    pivot_longer(cols = -selectedVars[2], names_to = "indicator", values_to = "value") %>%
    dplyr::rename(condition = selectedVars[2]) %>%
    mutate(condition = factor(condition, levels = c("Minimal", "Moderate", "Major")))
  
  p <- ggplot(plotdat, aes(x = indicator, y = value)) + 
    # ggdist::stat_halfeye(
    #   adjust = .5, 
    #   width = .6, 
    #   .width = 0, 
    #   justification = -.3, 
    #   point_colour = NA) + 
     geom_boxplot(
       width = .25, 
       outlier.shape = NA
     ) +
    geom_point(
      aes(fill = condition),
      color = "black",
      stroke = 0.3,
      size = 3,
      alpha = .7,
      position = position_jitter(
        seed = 1, width = .1
      )
    ) + 
    scale_fill_manual(values = c("Minimal" = "green2", 
                                 "Moderate" = "yellow2", 
                                 "Major" = "red2"),
                      name = "Condition") +
    # scale_color_manual(
    #   values = c("green", "yellow", "red"),
    #   labels = c("Minor", "Moderate", "Major", 
    #   drop = FALSE)) +
    expand_limits(y=-1) +
    #ylim(c(-1, NA), clip = "off") +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = c(0.8, 0.8))
  
  ggplotly(p, height = 700, width = 700)
}

