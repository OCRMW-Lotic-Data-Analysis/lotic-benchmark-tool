##### Helper functions -----------------------------------------------------------

### Function to make column widths that forces all data on single line.  This is somehow
# not built into the reactable package.  This wrapper also allows the addition of extra 
# reactable::colDef() inputs as a second argument.  
dynamicColWidths <- function(reachCondLong, otherColDefs = NULL){
  
  getColWidths <- function(colName){
    # Width of column name
    ncharColName <- nchar(colName)
    
    # Width of data.  Code below handles if NA count in this colName is some, all, or none.
    # max(NA, rm.na =TRUE) throws a warning without this.  
    nonNAdata <- reachCondLong %>% pull(colName) %>% nchar() %>% na.omit()
    
    if (length(nonNAdata) > 0){
      ncharVal <- nonNAdata %>% max()
    } else if (length(nonNAdata) == 0) {
      ncharVal <- 0
    }
    
    # When grouping rows, add a few characters for the EvaluationID appended row count (e.g. "XE-SS-5141_2013-08-23 (10)")
    if (colName == "EvaluationID"){
      ncharVal <- ncharVal + 3
    }
    
    maxLength <- max(ncharColName, ncharVal, na.rm = TRUE) * 12 # 12 = pixels per character (font size?)
    
    reactable::colDef(minWidth = maxLength)
  }
  
  # Make list of column widths.  set_names names the list elements properly in purrr:map().
  singleLineColWidths <- set_names(names(reachCondLong)) %>% purrr::map(getColWidths)
  
  # Merge column width settings and other colDefs.
  allColDefsMerged <- list_merge(singleLineColWidths, !!!otherColDefs) # '!!!' splices 2nd list into first
  
  # Fix attributes.  the S3 class attribute isn't allowed through with list_merge.
  # Each element is assigned the class here manually.  There must be a better way.
  
  # Function to fix attributes
  setColDefAttr <- function(x, elemName){
    y <- x[elemName]
    attr(y[[elemName]], "class") <- "colDef"
    return(y)
  }
  
  # Apply function to all elements.
  allColDefs <- map(names(allColDefsMerged), ~setColDefAttr(allColDefsMerged,.x)) %>% flatten()
}

# Download CSV button for condition_summary_table
csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}

##### Tables ---------------------------------------------------------------------

### Display default conditions for selected points
default_conditions_table <- function(defCond){
  reactable(defCond,
            pagination = FALSE, 
            highlight = TRUE, 
            compact = TRUE,
            groupBy = "EvaluationID",
            columns = dynamicColWidths(defCond,
                                       otherColDefs = list(
                                         Condition = colDef(
                                           style = function(value) {
                                             color <- reachCondPalette(value)
                                             list(background = color)
                                           }
                                         )
                                       ))
            )
}

### Show saved benchmark groups
saved_benchmark_groups_table <- function(bmGroups) {
  reactable(bmGroups,
            pagination = FALSE,
            groupBy = "BenchmarkGroup",
            selection = "multiple",
            onClick = "select",
            columns = dynamicColWidths(bmGroups)
            )
}

### Editable table allowing user to select which benchmark group to use for point indicators (rhandsontable)
apply_benchmarks_table <- function(benchmarkGroupDF, indicatorData) {
  
  # Get the names of all possible benchmarks (will likley need to tweak this)
  bmVars <- benchmarkGroupDF[['df']] %>% pull(Indicator) %>% unique()
  
  # Unique benchmark group names (unique b/c it's pulling from long-form table)
  bmGroups <- benchmarkGroupDF[['df']] %>% pull(BenchmarkGroup) %>% unique()
  #print(bmGroups)
  
  # Strip indicator table to basic info and set benchmark group to "Default".
  # "Default" setting will be used to apply BLM's pre-defined, default conditions.
  applyBechmarkDat <- indicatorData %>% st_drop_geometry() %>% select(c(PointID, EvaluationID)) %>% arrange(EvaluationID)
  applyBechmarkDat[bmVars] <- "Default"
  
  # Expand table to at least 5 rows.  This is a workaround to make the dropdowns work with 
  # overflow = "hidden" in rhandsontable.  Without this, the table expands to the whole page
  # and has extra scroll bars that mess the UI up.  
  
  extraRowNums <- NULL  # set to null so locked rows works in hot_row() works regardless of nrows.
  
  if (nrow(applyBechmarkDat) < 5) {
    extraRowNums <- (nrow(applyBechmarkDat)+1):5
    applyBechmarkDat[extraRowNums,] <- NA
  }
 
  # Actual table
  rhot <- rhandsontable(applyBechmarkDat,
                        overflow = "hidden",
                        rowHeaders = FALSE,
                        selectCallback = TRUE,
                        height = 350
                        ) %>%
    hot_table(highlightRow = TRUE, contextMenu = FALSE) %>%
    hot_row(extraRowNums, readOnly = TRUE) %>%    # lock the blank rows
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
                           } else if (isNaN(value)) {
                           td.style.background = '#C1FFC1';
                           }
                           Handsontable.renderers.DropdownRenderer.apply(this, arguments);
                         }"
    )

  return(rhot)
}

### Review applied benchmarks.  Table showing what benchmarks were appled and their condition.
review_applied_benchmarks_table <- function(reachCondLong){

  # # Make the table
  reactable(reachCondLong,
            pagination = FALSE,
            highlight = TRUE,
            compact = TRUE,
            groupBy = "EvaluationID",
            columns = dynamicColWidths(reachCondLong,
                                       otherColDefs = list(
                                         Condition = colDef(
                                           style = function(value) {
                                             color <- reachCondPalette(value)
                                             list(background = color)
                                           }
                                         )
                                       ))
            )
  
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

