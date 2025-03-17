# Lotic AIM Custom Benchmark Tool

This tool allows users to determine the condition of streams by defining custom benchmarks and applying them to Lotic AIM indicator data. The workflow is split into distinct steps: selecting indicators, defining benchmarks, applying benchmarks, and viewing results.

## 1. Select Indicators

A Leaflet map allows the user to select the Lotic AIM indicators of interest by *EvaluationID.* Each point on the map represents a field visit to a Lotic AIM point and therefore there may be multiple points per *PointID.* Point selections can be performed either by clicking individual points or drawing polygons around groups of points. Keep in mind that selection of points is based on *PointID* so if you select a point that has multiple field visits, both points will be selected.

## 2. Define Benchmarks

Each benchmark is essentially a set of thresholds that identify if a point's indicator is a *minimal, moderate, or major* departure from the defined benchmark. This tool allows for very granular analysis of benchmark, point, and indicator combinations. The user must define benchmark values for each indicator of interest and a combination of benchmarks can then be saved as a benchmark group. For example, you may want a "Fish Bearing" group with *InstantTemp* and *PctFinesLessThan2mm* benchmarks and then a "Non Fish Bearing" group with *InstantTemp* and *ChannelIncision.* Each could contain separate values for *InstantTemp*. This step also allow the user to view Default Conditions of selected points based on state and national defaults. These are largely here for reference.

## 3. Apply Benchmarks

This is where all of the defined benchmarks are applied to the points. The table consist of one row per *EvaluationID* and a column for each indicator that has a defined custom benchmark. This allows the user to apply benchmarks from different groups to the same point. Each cell contains a dropdown menu of all of the defined benchmark groups. If no group is selectd, the Default Conditions values are used. Note that not all indicators in this table have default values. If the selected cell is left as 'Default', and there is a Default Condition for that *EvaluationID*, that default condition will be used. If there is not a Default Condition, no condition will be returned for that indicator and *EvaluationID* combination.

The map below the table displays all selected points. When selecting cells in the table, the respective point will be highlighted in red on the map.

## 4. Reach Conditions

Conditions are automatically calculated in the background and will show up on this page. The user can view the conditions on the map, review the applied conditions in table form, or download the data for further analysis or visualization. The downloaded data, both CSV and Geopackage, contain all the selected point's original indicators plus the addition of conditions, either custom or default.

## 5. Summary

Some summary information is provided in the form of basic statistics and box plots. The summary table and box plots only contain indicators for which a custom benchmark was defined.
