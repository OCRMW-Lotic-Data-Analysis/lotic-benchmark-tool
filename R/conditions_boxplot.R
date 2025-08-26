# Interactive boxplot for summary page
# Interactive boxplot for summary page
conditions_boxplot <- function(reachCond, benchmark, showDensity) {

# Data prep
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
                              "\n Value: ", value)) %>%
  drop_na()
# ggplot base
p <- ggplot(plotdat, aes(x = indicator, y = value)) + 
  geom_boxplot_interactive(
    width = .25, 
    outlier.shape = NA,
    show.legend = FALSE,
    fill = "white",          
    color = "gray40",         
    alpha = 0.6) +            
  geom_point_interactive(
    aes(tooltip = tooltiptext, data_id = EvaluationID, fill = condition),
    color = "grey30", # color = border (stroke), fill = inside of point when both are used.         
    shape = 21,
    stroke = 0.5,             
    size = 3,               
    alpha = .85,              
    position = position_jitter(seed = 1, width = .1)) +
  scale_fill_manual_interactive(
    values = c(Minimal = "#00a9e6", 
               Moderate = "#e6e600", 
               Major = "#895a44"),
    data_id = c(Minimal = "Minimal", Moderate = "Moderate", Major = "Major"),
    name = "Condition") +
  labs(x = "", 
       y = "",
      tag = paste("n = ", nrow(plotdat))) +
  coord_cartesian(xlim = c(1.3, 1.35)) +
  theme_minimal() +
  theme(
    # Legend
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.8, "cm"),
    legend.margin = margin(l = 20),
    # Panel
    plot.margin = margin(t = 15, r = 5, b = 15, l = 5, unit = "pt"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 1),
    # Text
    axis.text = element_text(size = 11, color = "black"),
    plot.tag = element_text(size = 10),
    plot.tag.position = c(0.25, 0.01),
  )

if (showDensity == TRUE) {
  p <- p + ggdist::stat_slab(
    adjust = .5,
    width = .6,
    justification = -.3,
    slab_linewidth= 0.5,
    color = "gray50",
    fill = "gray85") 
}

# covert to ggiraph
girplot <- girafe(ggobj = p,
  options = list(
    opts_toolbar(hidden = c("lasso_select", "lasso_deselect")),
    opts_selection(type = "none"),
    opts_tooltip(
      css = "background-color: #d5d5d5ff;
        color: black; 
        font-family: 'Arial', sans-serif; 
        font-size: 14px;
        padding: 5px; 
        border-radius: 8px; 
        border: 1px solid #34495e;
        box-shadow: 0 4px 8px rgba(0,0,0,0.3);
        opacity: 0.95;"
      )
    )
  )
  girplot
}
