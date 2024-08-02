### Display default conditions for selected points
default_conditions_table <- function(defCond){
reactable(defCond,
          showPageSizeOptions = TRUE, 
          showPageInfo = FALSE, 
          highlight = TRUE, 
          compact = TRUE,
          groupBy = "EvaluationID",
          defaultColDef = colDef(minWidth = 250)
          )
}

### Show saved benchmark groups


saved_benchmark_groups_table <- function(bmGroups) {
reactable(bmGroups,
          pagination = FALSE,
          groupBy = "bmGroup",
          selection = "multiple",
          onClick = "select",
          defaultColDef = colDef(minWidth = 220))
}

### Editable table allowing user to select which benchmark group to use for point indicators (rhandsontable)
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

### Final reach conditions summary table (reactable).  Expects condition_summary_df() output.
condition_summary_table <- function(summaryData){
reactable(summaryData,
          fullWidth = FALSE,
          pagination = FALSE, 
          showPageInfo = FALSE,
          highlight = TRUE,
          rowStyle = list(cursor = "pointer"),
          columns = list(
            Indicator = colDef(width = 220),
            Minimal = colDef(na = "–", align = "center"),
            Moderate = colDef(na = "–", align = "center"),
            Major = colDef(na = "–", align = "center"),
            Min = colDef(na = "–", align = "center"),
            Max = colDef(na = "–", align = "center"),
            Mean = colDef(na = "–", align = "center")
          ),
          columnGroups = list(
            colGroup(name = "Number of Reaches", columns = c("Minimal", "Moderate", "Major")),
            colGroup(name = "Indicator Summary Statistics", columns = c("Min", "Max", "Mean"))
          ))
}

