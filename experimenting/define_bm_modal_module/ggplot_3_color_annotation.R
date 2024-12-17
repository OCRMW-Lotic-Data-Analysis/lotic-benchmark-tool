library(ggplot2)

p <- ggplot() +
  annotate("rect", xmin = 0, xmax = 30, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
  # annotate("rect", xmin = 30, xmax = 75, ymin = 0, ymax = 1, fill = "#e6e600", color = "black") +
  annotate("rect", xmin = 75, xmax = 100, ymin = 0, ymax = 1, fill = "#895a44", color = "black") +
  
  
  #annotate("text", x = 30, y = 1.2, label = "Some text") +
  #annotate("text", x = 75, y = 1.2, label = "Some text") +

  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, 10), expand = c(0,0),
                     sec.axis = dup_axis(breaks = c(30,75))) +
  
  # scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, 10), expand = c(0,0)) +
  
  
  #scale_y_continuous(limits = c(0,1.1), expand = c(0,0)) +
  
  #scale_y_continuous(limits = c(0,1.0)) +
  
  #labs(x = NULL, y = NULL) +
  coord_cartesian(
    ylim=c(0,1),
    clip = "off",
    expand = FALSE) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(t = 10,  # Top margin
                         r = 10,  # Right margin
                         b = 10,  # Bottom margin
                         l = 10)) # Left margin) # Left margin



p + annotate("rect", xmin = 30, xmax = 75, ymin = 0, ymax = 1, fill = "#e6e600", color = "black") 




smpl <- tibble::tribble(
                 ~Indicator, ~ModerateBenchmark1, ~MajorBenchmark1, ~ModerateBenchmark2, ~MajorBenchmark2,     ~IncreaserDecreaser, ~MajorToModerateRel1, ~MinimalToModerateRel1, ~MinimalToModerateRel2, ~MajorToModerateRel2, ~MajorToMinimalRel1, ~MajorToMinimalRel2, ~ConditionCategoryNum,
  "PctBankCoveredStableMIM",                 50L,              85L,                  NA,               NA, "Decreases with stress",                 "<=",                   ">=",                     NA,                   NA,                  NA,                  NA,                    3L,
      "PctFinesLessThan2mm",                 58L,              75L,                  NA,               NA, "Increases with stress",                 ">=",                   "<=",                     NA,                   NA,                  NA,                  NA,                    3L,
  "PctBankCoveredStableOld",                 70L,              30L,                  NA,               NA, "Decreases with stress",                 "<=",                   ">=",                     NA,                   NA,                  NA,                  NA,                    3L,
  "PctBankCoveredStableOld",                  NA,              40L,                  NA,               NA, "Decreases with stress",                   NA,                     NA,                     NA,                   NA,                "<=",                  NA,                    2L,
   "FloodplainConnectivity",                 1.1,              2.3,                  NA,               NA, "Increases with stress",                 ">=",                   "<=",                     NA,                   NA,                  NA,                  NA,                    3L
  )


custBM <- smpl[5,]

indicatorMetadata <- read.csv("./indicator_metadata.csv", colClasses = "character") 
metadata <- indicatorMetadata 
selindic <- "PctBankCoveredStableOld"
MajorBenchmark1 <- 10 #input$MajorBenchmark1
ModerateBenchmark1 <- 20 #input$ModerateBenchmark1
ConditionCategoryNum <- 3 #input$ConditionCategoryNum
#IncreaserDecreaser

bmDefVisual <- function(metadata = indicatorMetadata, 
                        definedBM = custBM){
  
  
}
