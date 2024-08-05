# Apply bench mark table.  Allows user to set which benchmark is used for each indicator/point combination

apply_benchmarks_table <- function(defaultBenchmarkVals, benchmarkGroupDF, indicatorData) {

# Get the names of all possible benchmarks (will likley need to tweak this)
#bmVars <- unique(defaultBenchmarkVals$Indicator)
bmVars <- benchmarkGroupDF[['df']] %>% pull(Indicator) %>% unique()

# Unique benchmark group names (unique b/c it's pulling from long-form table)
bmGroups <- benchmarkGroupDF[['df']] %>% pull(bmGroup) %>% unique()
#print(bmGroups)

# Strip indicator table to basic info and set benchmark group to "Default".
# "Default" setting will be used to apply BLM's pre-defined, default conditions.
applyBechmarkDat <- indicatorData %>% st_drop_geometry() %>% select(c(PointID, EvaluationID))
applyBechmarkDat[bmVars] <- "Default"

# Actual table
rhot <- rhandsontable(applyBechmarkDat,
                      overflow = "visible",
                      rowHeaders = FALSE) %>%
  hot_table(highlightRow = TRUE, contextMenu = FALSE) %>%
  hot_cols(fixedColumnsLeft = 2, colWidths = 200) %>%
  hot_col(1:2, readOnly = TRUE) %>%
  hot_col(col = bmVars,
          type = "dropdown",
          # Including "Default" below is key. If 'bmGroups' only has 1 value, the dropdown doesn't work.
          source = c(bmGroups, "Default"),
          allowInvalid = FALSE,
          # Cells with "Default" selected are greyed out a bit.  Makes it easier to see where custom values are used.
          renderer = "function (instance, td, row, col, prop, value, cellProperties) {
                           Handsontable.renderers.TextRenderer.apply(this, arguments);
                           if (value == 'Default') {
                           td.style.color = 'lightgrey';
                           } else if (value != 'Default') {
                           td.style.background = '#C1FFC1';
                           }
                           Handsontable.renderers.DropdownRenderer.apply(this, arguments);
                         }"
  )

return(rhot)
}