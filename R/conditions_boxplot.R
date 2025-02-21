# Interactive boxplot for summary page
conditions_boxplot <- function(reachCond, benchmark, showDensity) {
  
selectedVars <- c(benchmark, paste0(benchmark, "Condition"),
                  "EvaluationID", "StreamName")

plotdat <- reachCond %>% 
  st_drop_geometry() %>% 
  select(any_of(selectedVars)) %>%
  pivot_longer(cols = -c(selectedVars[2:4]), names_to = "indicator", values_to = "value") %>%
  dplyr::rename(condition = selectedVars[2]) %>%
  mutate(condition = factor(condition, levels = c("Minimal", "Moderate", "Major")),
         tooltiptext = paste0("EvaluationID: ", EvaluationID, 
                              "\n Stream Name: ", StreamName, 
                              "\n Condition: ", condition, 
                              "\n Value: ", value))

p <- ggplot(plotdat, aes(x = indicator, y = value)) + 
  geom_boxplot_interactive(
    width = .25, 
    outlier.shape = NA,
    show.legend = FALSE) +  # hide boxplot legend items
  geom_point_interactive(
    aes(tooltip = tooltiptext, data_id = EvaluationID, fill = condition),
    color = "black", # color = border (stroke), fill = inside of point when both are used.  
    shape = 21,
    stroke = 0.3,
    size = 3,
    alpha = .7,
    position = position_jitter(
      seed = 1, width = .1)) +
  scale_fill_manual_interactive(values = c(Minimal = "#00a9e6", 
                                           Moderate = "#e6e600", 
                                           Major = "#895a44"),
                                data_id = c(Minimal = "Minimal", Moderate = "Moderate", Major = "Major"),
                                name = "Condition") +
  labs(x = "", y = "") +
  coord_cartesian(xlim = c(1.3, 1.35)) +
  theme_bw() +
  theme(legend.position = "right",
        legend.text.align = 0)  # left justified text

if (showDensity == TRUE) {
  p <- p + ggdist::stat_slab(
    adjust = .5,
    width = .6,
    justification = -.3,
    slab_linewidth= 0.5,
    color = "gray50",
    fill = "gray85") 
}

girplot <- girafe(ggobj = p)
girafe_options(girplot, 
               opts_toolbar(hidden = c("lasso_select", "lasso_deselect")),
               opts_selection(type = "none"))
}
