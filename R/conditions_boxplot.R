# Interactive boxplot for summary page
conditions_boxplot <- function(reachCond, benchmark, showDensity) {
  
selectedVars <- c(benchmark, paste0(benchmark, "Condition"),
                  "PointID", "StreamName")

plotdat <- reachCond %>% 
  st_drop_geometry() %>% 
  select(any_of(selectedVars)) %>%
  pivot_longer(cols = -c(selectedVars[2:4]), names_to = "indicator", values_to = "value") %>%
  dplyr::rename(condition = selectedVars[2]) %>%
  mutate(condition = factor(condition, levels = c("Minimal", "Moderate", "Major")),
         tooltiptext = paste0("PointID: ", PointID, 
                              "\n Stream Name: ", StreamName, 
                              "\n Condition: ", condition, 
                              "\n Value: ", value))

p <- ggplot(plotdat, aes(x = indicator, y = value)) + 
  geom_boxplot_interactive(
    width = .25, 
    outlier.shape = NA,
    show.legend = FALSE) +  # hide boxplot legend items
  geom_point_interactive(
    aes(tooltip = tooltiptext, data_id = PointID, fill = condition),
    color = "black", # color = border (stroke), fill = inside of point when both are used.  
    shape = 21,
    stroke = 0.3,
    size = 3,
    alpha = .7,
    position = position_jitter(
      seed = 1, width = .1)) +
  scale_fill_manual_interactive(values = c(Minimal = "green2", 
                                           Moderate = "yellow2", 
                                           Major = "red2"),
                                data_id = c(Minimal = "Minimal", Moderate = "Moderate", Major = "Major"),
                                name = "Condition") +
  labs(x = "", y = "") +
  coord_cartesian(xlim = c(1.3, 1.35)) +
  theme_minimal() +
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

girafe(ggobj = p)
}
