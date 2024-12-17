library(dplyr)
library(scales)
library(ggplot2)
library(patchwork)

smpl <- tibble::tribble(
  ~Indicator, ~ModerateBenchmark1, ~MajorBenchmark1, ~ModerateBenchmark2, ~MajorBenchmark2,     ~IncreaserDecreaser, ~MajorToModerateRel1, ~MinimalToModerateRel1, ~MinimalToModerateRel2, ~MajorToModerateRel2, ~MajorToMinimalRel1, ~MajorToMinimalRel2, ~ConditionCategoryNum,
  "PctBankCoveredStableMIM",                 50L,              85L,                  NA,               NA, "Decreases with stress",                 "<=",                   ">=",                     NA,                   NA,                  NA,                  NA,                    3L,
  "PctFinesLessThan2mm",                 58L,              75L,                  NA,               NA, "Increases with stress",                 ">=",                   "<=",                     NA,                   NA,                  NA,                  NA,                    3L,
  "PctBankCoveredStableOld",                 70L,              30L,                  NA,               NA, "Decreases with stress",                 "<=",                   ">=",                     NA,                   NA,                  NA,                  NA,                    3L,
  "PctBankCoveredStableOld",                  NA,              40L,                  NA,               NA, "Decreases with stress",                   NA,                     NA,                     NA,                   NA,                "<=",                  NA,                    2L,
  "FloodplainConnectivity",                 1.1,              2.3,                  NA,               NA, "Increases with stress",                 ">=",                   "<=",                     NA,                   NA,                  NA,                  NA,                    3L
)

smpl <- tibble::tribble(
                 ~Indicator, ~ModerateBenchmark1, ~MajorBenchmark1, ~ModerateBenchmark2, ~MajorBenchmark2,                   ~IncreaserDecreaser, ~MajorToModerateRel1, ~MinimalToModerateRel1, ~MinimalToModerateRel2, ~MajorToModerateRel2, ~MajorToMinimalRel1, ~MajorToMinimalRel2, ~ConditionCategoryNum,
  "PctBankCoveredStableMIM",                  70,               50,                  NA,               NA,               "Decreases with stress",                 "<=",                   ">=",                     NA,                   NA,                  NA,                  NA,                    3L,
      "PctFinesLessThan2mm",                  NA,             21.4,                  NA,               NA,               "Increases with stress",                   NA,                     NA,                     NA,                   NA,                ">=",                  NA,                    2L,
     "PctBankOverheadCover",                  14,              6.5,                  NA,               NA,               "Decreases with stress",                 "<=",                   ">=",                     NA,                   NA,                  NA,                  NA,                    3L,
   "FloodplainConnectivity",                 1.3,              1.5,                  NA,               NA,               "Increases with stress",                 ">=",                   "<=",                     NA,                   NA,                  NA,                  NA,                    3L,
                       "pH",                   7,              6.5,                 8.5,               9L, "Decreases and increases with stress",                 "<=",                   ">=",                   "<=",                 ">=",                  NA,                  NA,                    3L,
   "PctNoxiousWoodySpecies",                   1,                5,                  NA,               NA,               "Increases with stress",                  ">",                    "<",                     NA,                   NA,                  NA,                  NA,                    3L,
  "PctBankCoveredStableOld",                  70,               50,                  NA,               NA,               "Decreases with stress",                 "<=",                   ">=",                     NA,                   NA,                  NA,                  NA,                    3L,
              "InstantTemp",                  17,             21.7,                  NA,               NA,               "Increases with stress",                 ">=",                   "<=",                     NA,                   NA,                  NA,                  NA,                    3L,
    "MMI_Macroinvertebrate",                  45,               34,                  NA,               NA,               "Decreases with stress",                  "<",                    ">",                     NA,                   NA,                  NA,                  NA,                    3L,
      "PctFinesLessThan2mm",                   3,               35,                  NA,               NA,               "Increases with stress",                  ">",                    "<",                     NA,                   NA,                  NA,                  NA,                    3L,
         "TotalPhosphorous",                  NA,               35,                  NA,               NA,               "Increases with stress",                   NA,                     NA,                     NA,                   NA,                 ">",                  NA,                    2L,
     "OE_Macroinvertebrate",                0.76,             0.69,                  NA,               NA,               "Decreases with stress",                  "<",                   ">=",                     NA,                   NA,                  NA,                  NA,                    3L,
            "TotalNitrogen",                  NA,              400,                  NA,               NA,               "Increases with stress",                   NA,                     NA,                     NA,                   NA,                 ">",                  NA,                    2L,
    "PctNativeWoodySpecies",                  90,               70,                  NA,               NA,               "Decreases with stress",                  "<",                   ">=",                     NA,                   NA,                  NA,                  NA,                    3L,
            "PctBankStable",                  70,               50,                  NA,               NA,               "Decreases with stress",                 "<=",                   ">=",                     NA,                   NA,                  NA,                  NA,                    3L,
        "PctBankCoveredOld",                  80,               60,                  NA,               NA,               "Decreases with stress",                 "<=",                   ">=",                     NA,                   NA,                  NA,                  NA,                    3L,
        "PctBankCoveredMIM",                  70,               50,                  NA,               NA,               "Decreases with stress",                 "<=",                   ">=",                     NA,                   NA,                  NA,                  NA,                    3L,
     "PctBankOverheadCover",                  NA,             14.9,                  NA,               NA,               "Decreases with stress",                   NA,                     NA,                     NA,                   NA,                 "<",                  NA,                    2L,
         "LgWoodInChanFreq",                  NA,             1.35,                  NA,               NA,               "Decreases with stress",                   NA,                     NA,                     NA,                   NA,                 "<",                  NA,                    2L,
                       "pH",                  NA,              6.5,                  NA,               9L, "Decreases and increases with stress",                   NA,                     NA,                     NA,                   NA,                "<=",                ">=",                    2L
  )


custBM <- smpl[20,]

indicatorMetadata <- read.csv("./indicator_metadata.csv", colClasses = "character") 






bmDefVisual <- function(metadata, custBM){

# Simplify value names from input custBM dataframe
indic <- custBM$Indicator
incOrDec <- custBM$IncreaserDecreaser
numCats <- custBM$ConditionCategoryNum
mod1 <- custBM$ModerateBenchmark1
mod2 <- custBM$ModerateBenchmark2
maj1 <- custBM$MajorBenchmark1
maj2 <- custBM$MajorBenchmark2

# Lower and Upper ranges for the plot
rangeLow <- indicatorMetadata %>% filter(Indicator == indic) %>% pull(RangeLower) %>% first() %>% as.numeric()
rangeUp <- indicatorMetadata %>% filter(Indicator == indic) %>% pull(RangeUpper) %>% first() %>% as.numeric()

# Account for ranges with no true lower or upper end
if (rangeUp == Inf){
  rangeUp <- maj1 * 1.2 # add 20% to create some padding
}

if (rangeLow == -Inf){
  rangeLow <- mod1 * 0.8 # add 20% to create some padding
}

### Assign xmin and xmax values for Minimal, Moderate, and Major boxes to
# be used in ggplot annotate() boxes.
# 3 categorries
if (numCats == 3){
  # NOT ph
  if (incOrDec == "Increases with stress") {
    min_xmin <- rangeLow
    min_xmax <- mod1
    
    mod_xmin <- mod1
    mod_xmax <- maj1
    
    maj_xmin <- maj1
    maj_xmax <- rangeUp
  } 
  if (incOrDec == "Decreases with stress") {
    min_xmin <- mod1
    min_xmax <- rangeUp
    
    mod_xmin <- maj1
    mod_xmax <- mod1
    
    maj_xmin <- rangeLow
    maj_xmax <- maj1
  } 
  # pH
  if (incOrDec == "Decreases and increases with stress") {
    # Acidic
    min_xmin_acid <- mod1
    min_xmax_acid <- rangeUp
    
    mod_xmin_acid <- maj1
    mod_xmax_acid <- mod1
    
    maj_xmin_acid <- rangeLow
    maj_xmax_acid <- maj1
    
    # Alkaline
    min_xmin_alk <- rangeLow
    min_xmax_alk <- mod2
    
    mod_xmin_alk <- mod2
    mod_xmax_alk <- maj2
    
    maj_xmin_alk <- maj2
    maj_xmax_alk <- rangeUp
  } 
} 

if (numCats == 2){
  # NOT ph
  if (incOrDec == "Increases with stress") {
    min_xmin <- rangeLow
    min_xmax <- maj1
    
    maj_xmin <- maj1
    maj_xmax <- rangeUp
  } 
  if (incOrDec == "Decreases with stress") {
    min_xmin <- maj1
    min_xmax <- rangeUp
    
    maj_xmin <- rangeLow
    maj_xmax <- maj1
  }
  # pH
  if (incOrDec == "Decreases and increases with stress") {
    # Acidic
    min_xmin_acid <- maj1
    min_xmax_acid <- rangeUp
    
    maj_xmin_acid <- rangeLow
    maj_xmax_acid <- maj1
    
    # Alkaline
    min_xmin_alk <- rangeLow
    min_xmax_alk <- maj2
    
    maj_xmin_alk <- maj2
    maj_xmax_alk <- rangeUp
  }
}

# Set up blank plot framework
p <- ggplot() +
  coord_cartesian(
    ylim=c(0,1),
    clip = "off",
    expand = FALSE) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(t = 10,  # Top margin
                         r = 10,  # Right margin
                         b = 10,  # Bottom margin
                         l = 10)) # Left margin) # Left margin


### Add annotations (colored areas) to plot
# NOT ph
if (indic != "pH"){
  if(numCats == 3){
    p <- p  +
      annotate("rect", xmin = mod_xmin, xmax = mod_xmax, ymin = 0, ymax = 1, fill = "#e6e600", color = "black") +
      annotate("rect", xmin = min_xmin, xmax = min_xmax, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
      annotate("rect", xmin = maj_xmin, xmax = maj_xmax, ymin = 0, ymax = 1, fill = "#895a44", color = "black") 
      
  } 
  if (numCats == 2){
    p <- p  +
      annotate("rect", xmin = min_xmin, xmax = min_xmax, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
      annotate("rect", xmin = maj_xmin, xmax = maj_xmax, ymin = 0, ymax = 1, fill = "#895a44", color = "black") 
  }
  
  p <- p + scale_x_continuous(limits = c(rangeLow,rangeUp), 
                       breaks = breaks_pretty(n = 10),  # helps with arbitrary input ranges
                       expand = c(0,0),
                       sec.axis = dup_axis(breaks = c(mod1,maj1)))
}
# pH
if (indic == "pH"){
  if(numCats == 3){
    p_acid <- p  +
      annotate("rect", xmin = mod_xmin_acid, xmax = mod_xmax_acid, ymin = 0, ymax = 1, fill = "#e6e600", color = "black") +
      annotate("rect", xmin = min_xmin_acid, xmax = min_xmax_acid, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
      annotate("rect", xmin = maj_xmin_acid, xmax = maj_xmax_acid, ymin = 0, ymax = 1, fill = "#895a44", color = "black") 
    
    p_alk <- p  +
      annotate("rect", xmin = mod_xmin_alk, xmax = mod_xmax_alk, ymin = 0, ymax = 1, fill = "#e6e600", color = "black") +
      annotate("rect", xmin = min_xmin_alk, xmax = min_xmax_alk, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
      annotate("rect", xmin = maj_xmin_alk, xmax = maj_xmax_alk, ymin = 0, ymax = 1, fill = "#895a44", color = "black") 
  } 
  if (numCats == 2){
    p_acid <- p  +
      annotate("rect", xmin = min_xmin_acid, xmax = min_xmax_acid, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
      annotate("rect", xmin = maj_xmin_acid, xmax = maj_xmax_acid, ymin = 0, ymax = 1, fill = "#895a44", color = "black") 
    
    p_alk <- p  +
      annotate("rect", xmin = min_xmin_alk, xmax = min_xmax_alk, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
      annotate("rect", xmin = maj_xmin_alk, xmax = maj_xmax_alk, ymin = 0, ymax = 1, fill = "#895a44", color = "black") 
  }
  
  # Need 2 plots for pH fork acid and alkaline
  p_acid <- p_acid + scale_x_continuous(limits = c(rangeLow,rangeUp), 
                              breaks = breaks_pretty(n = 10),  # helps with arbitrary input ranges
                              expand = c(0,0),
                              sec.axis = dup_axis(breaks = c(mod1,maj1))) +
                     labs(title = "Acidic")
  
  p_alk <- p_alk + scale_x_continuous(limits = c(rangeLow,rangeUp), 
                                        breaks = breaks_pretty(n = 10),  # helps with arbitrary input ranges
                                        expand = c(0,0),
                                        sec.axis = dup_axis(breaks = c(mod2,maj2))) +
                   labs(title = "Alkaline")
  
  # patchwork package used for stacked plots
  p <- p_acid / p_alk
}


# legend <- ggplot() +
#   coord_cartesian(
#     ylim=c(0,1),
#     clip = "off",
#     expand = FALSE) +
#   theme_void() +
#   annotate("rect", xmin = 0, xmax = 6, ymin = 0, ymax = 0.5, fill = "#00a9e6", color = "black") +
#   annotate("text", x = 3, y = 0.25, label = "Minimal") +
#   annotate("rect", xmin = 8, xmax = 14, ymin = 0, ymax = 0.5, fill = "#e6e600", color = "black") +
#   annotate("text", x = 11, y = 0.25, label = "Moderate") +
#   annotate("rect", xmin = 16, xmax = 22, ymin = 0, ymax = 0.5, fill = "#895a44", color = "black") +
#   annotate("text", x = 19, y = 0.25, label = "Major") 
if (numCats == 3){
legend <- ggplot() +
  coord_cartesian(
    ylim=c(0,1),
    xlim=c(0,120),
    clip = "off",
    expand = FALSE) +
  theme_void() +
  annotate("rect", xmin = 12, xmax = 22, ymin = 0, ymax = 0.5, fill = "#00a9e6", color = "black") +
  annotate("text", x = 23, y = 0.25, label = "Minimal", hjust = 0) +
  annotate("rect", xmin = 50, xmax = 60, ymin = 0, ymax = 0.5, fill = "#e6e600", color = "black") +
  annotate("text", x = 61, y = 0.25, label = "Moderate", hjust = 0) +
  annotate("rect", xmin = 90, xmax = 100, ymin = 0, ymax = 0.5, fill = "#895a44", color = "black") +
  annotate("text", x = 101, y = 0.25, label = "Major", hjust = 0)  
} else if (numCats == 2){
  legend <- ggplot() +
    coord_cartesian(
      ylim=c(0,1),
      xlim=c(0,120),
      clip = "off",
      expand = FALSE) +
    theme_void() +
    annotate("rect", xmin = 30, xmax = 40, ymin = 0, ymax = 0.5, fill = "#00a9e6", color = "black") +
    annotate("text", x = 41, y = 0.25, label = "Minimal", hjust = 0) +
    annotate("rect", xmin = 70, xmax = 80, ymin = 0, ymax = 0.5, fill = "#895a44", color = "black") +
    annotate("text", x = 81, y = 0.25, label = "Major", hjust = 0)  
}




#p <- p/legend

# p <- p +
#   theme(plot.margin = unit(c(0, 0, 50, 0), "points")) +
#   annotation_custom(
#     ggplotGrob(legend),
#     xmin = rangeLow,
#     xmax = rangeUp,
#     ymin = -0.2,
#     ymax = -0.1
#   )


# Return plot
p <- p / legend
p

}


bmDefVisual(indicatorMetadata, smpl[7,])
