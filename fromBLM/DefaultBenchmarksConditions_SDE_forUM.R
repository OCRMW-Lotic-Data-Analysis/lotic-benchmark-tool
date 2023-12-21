# script to create DefaultConditions table for SDE #
# run Step 1 initialization to get DF_ReachInfo then run script below
source(Step_01_Initialization_RODBC.R)

library(nngeo)
library(spdep)
library(dplyr)
library(arcgisbinding)
library(rgeos)
library(sp)
###########################################################################
# Step 1 attribute evaluationIds with benchmark groups and benchmarks for each state and indicator combination #
###########################################################################
arc.check_product()
## #Read in INDICATORS

IndicatorsSpatial <-arc.select(arc.open('I_Indicators_evw'))
IndicatorsSpatial<-arc.data2sf(IndicatorsSpatial)
indicators <- st_read(here("fromBLM", "Ian_Lotic_LCAT_Layers.gdb"), layer = "I_Indicators")

# Use if reading in from fgdb instead of SDE
#IndicatorsSpatial=sf::st_read(dsn = "BenchmarkGroups.gdb", layer = "I_Indicators")
IndicatorsSpatial <- st_read(here("fromBLM", "Ian_Lotic_LCAT_Layers.gdb"), layer = "I_Indicators")

#subset indicators to just unique identifier and spatial info
coordinates=IndicatorsSpatial[,c("EvaluationID","PointID", "BLM_AdminState")]


#### Read in BENCHMARK GROUP POLYGONS ####

defaultBenchmarks<-arc.select(arc.open('A_DefaultBenchmarkGroups_evw'))
defaultBenchmarks<-arc.data2sf(defaultBenchmarks)

# From Local DB
defaultBenchmarks <- st_read(here("fromBLM", "Ian_Lotic_LCAT_Layers.gdb"), layer = "A_DefaultBenchmarkGroups")
defaultBenchmarks <- st_make_valid(defaultBenchmarks)




# intersect evaluationID coordinates with the default benchmark polygons. Will output multiple lines for each EvalID if there are overlapping polygons.
attributedpoints1=sf::st_intersection(coordinates,defaultBenchmarks)
attributedpoints1 <- attributedpoints1[,c("EvaluationID","OBJECTID","PointID","BLM_AdminState", "Indicator","BenchmarkSource","BenchmarkMethod","BenchmarkGroup",
                                          "NonSpatialBenchmarkGroup","ModerateBenchmark1","MajorBenchmark1","ModerateBenchmark2","MajorBenchmark2",         
                                          "IncreaserDecreaser", "MajorToModerateRel1","MinimalToModerateRel1","MinimalToModerateRel2",   
                                          "MajorToModerateRel2","MajorToMinimalRel1","MajorToMinimalRel2","ConditionCategoryNum",    
                                          "RecordID","GlobalID","created_user","created_date","last_edited_user","last_edited_date","SDE_STATE_ID","geom")]  

attributedpoints1 <- attributedpoints1[,c("EvaluationID", "PointID","BLM_AdminState", "Indicator","BenchmarkSource","BenchmarkMethod","BenchmarkGroup",
                                          "NonSpatialBenchmarkGroup","ModerateBenchmark1","MajorBenchmark1","ModerateBenchmark2","MajorBenchmark2",         
                                          "IncreaserDecreaser", "MajorToModerateRel1","MinimalToModerateRel1","MinimalToModerateRel2",   
                                          "MajorToModerateRel2","MajorToMinimalRel1","MajorToMinimalRel2","ConditionCategoryNum",    
                                          "RecordID","created_user","created_date","last_edited_user","last_edited_date")] 

#### Read in BENCHMARK GROUP LINES ####
defaultBenchmarksLines <- arc.select(arc.open('A_DefaultBenchmarkGroups_Lines_evw'))
defaultBenchmarksLines<-arc.data2sf(defaultBenchmarksLines)
#from local DB
defaultBenchmarksLines <- st_read(here("fromBLM", "Ian_Lotic_LCAT_Layers.gdb"), layer = "A_DefaultBenchmarkGroups_Lines")
defaultBenchmarksLines <- st_make_valid(defaultBenchmarksLines)

coordinates_trans<-sf::st_transform(coordinates,5070)
defaultBenchmarksLines_trans<-sf::st_transform(defaultBenchmarksLines,5070)


# Subset coordinates object to only points in states that also have line benchmarks
coordinates_sub <- subset(coordinates_trans, BLM_AdminState %in% defaultBenchmarksLines_trans$BLM_AdminState)
# loop through indicators and states. Subset data one state at a time such that proper benchmarks are applied.  
# join to nearest neighbor (1 feature per indicator)
nested_list <- list()
for (i in levels(as.factor(defaultBenchmarksLines_trans$Indicator))){
  for(j in unique(coordinates_sub$BLM_AdminState)){
    data <-  defaultBenchmarksLines_trans[defaultBenchmarksLines_trans$Indicator == i & defaultBenchmarksLines_trans$BLM_AdminState == j ,]
    points <- coordinates_sub[coordinates_sub$BLM_AdminState == j,]
    nested_list[[i]][[j]]<- sf::st_join(points, data, join = nngeo::st_nn, maxdist = 20, k = 0, progress = FALSE)
    
  }
}
# un-nest the list, rbind, and remove row names
line_out <-do.call(Map, c(f = rbind, nested_list))
attributedpoints2 <- do.call(rbind, line_out)
rownames(attributedpoints2) <- NULL


# combine attributed points from polygon and line intersection

colnames(attributedpoints2)[colnames(attributedpoints2) == "ConditionCategoryNumber"] ="ConditionCategoryNum"
colnames(attributedpoints2)[colnames(attributedpoints2) == "BLM_AdminState.x"] ="BLM_AdminState"
attributedpoints2 <- attributedpoints2[,c("EvaluationID","OBJECTID","PointID","BLM_AdminState", "Indicator","BenchmarkSource","BenchmarkMethod","BenchmarkGroup",
                                          "NonSpatialBenchmarkGroup","ModerateBenchmark1","MajorBenchmark1","ModerateBenchmark2","MajorBenchmark2",         
                                          "IncreaserDecreaser", "MajorToModerateRel1","MinimalToModerateRel1","MinimalToModerateRel2",   
                                          "MajorToModerateRel2","MajorToMinimalRel1","MajorToMinimalRel2","ConditionCategoryNum",    
                                          "RecordID","GlobalID","created_user","created_date","last_edited_user","last_edited_date","SDE_STATE_ID","geom")]   
attributedpoints2<-sf::st_transform(attributedpoints2,4269)




### Read in default benchmarks MODELED values (i.e. TN, TP, SPC)###
defaultBenchmarksPoints<- arc.select(arc.open("A_DefaultBenchmarkGroups_Points"))
defaultBenchmarksPoints<-arc.data2sf(defaultBenchmarksPoints)
defaultBenchmarksPoints_df <- sf::st_drop_geometry(defaultBenchmarksPoints)
attributedpoints3 <- merge(as.data.frame(coordinates),as.data.frame(defaultBenchmarksPoints_df), by.x="EvaluationID", by.y="EvaluationID")
colnames(attributedpoints3)[colnames(attributedpoints3) == "BLM_AdminState.x"] ="BLM_AdminState"
colnames(attributedpoints3)[colnames(attributedpoints3) == "geom.x"] ="geom"
colnames(attributedpoints3)[colnames(attributedpoints3) == "PointID.x"] ="PointID"
attributedpoints3$created_user=NA
attributedpoints3$created_date=NA
attributedpoints3$last_edited_user=NA
attributedpoints3$last_edited_date=NA
attributedpoints3$SDE_STATE_ID=NA
attributedpoints3 <- attributedpoints3[,c("EvaluationID","OBJECTID","PointID","BLM_AdminState", "Indicator","BenchmarkSource","BenchmarkMethod","BenchmarkGroup",
                                          "NonSpatialBenchmarkGroup","ModerateBenchmark1","MajorBenchmark1","ModerateBenchmark2","MajorBenchmark2",         
                                          "IncreaserDecreaser", "MajorToModerateRel1","MinimalToModerateRel1","MinimalToModerateRel2",   
                                          "MajorToModerateRel2","MajorToMinimalRel1","MajorToMinimalRel2","ConditionCategoryNum",    
                                          "RecordID","GlobalID","created_user","created_date","last_edited_user","last_edited_date","SDE_STATE_ID","geom")]   


attributedpoints3 <- st_as_sf(attributedpoints3)

### Combine attributed points for Polygons, Lines and Points intersect/joins ###
attributedpoints4 <-rbind (attributedpoints1, attributedpoints2, attributedpoints3)




###########################################################################
# Step 2 pivot indicators and join benchmarks to indicators using state, EvaluationID and indicator columns 
###########################################################################
pointIDs <- DF_ReachInfo %>% tidyr::separate(EvaluationID, c('PointID', 'Date'), sep = '_')
uniquePointIDs <- unique(pointIDs['PointID'])
Indicators <- ReadTable(TableName = indicators, PointIDs=uniquePointIDs[,1])

#create new columns for modeled TN, TP and SpC
Indicators$TotalNitrogen_Modeled <- Indicators$TotalNitrogen
Indicators$TotalPhosphorous_Modeled <- Indicators$TotalPhosphorous
Indicators$SpecificConductance_Modeled <- Indicators$SpecificConductance

# For pivot to work, they must be numeric. If change column order, we have to redo this.Only pulling out numeric indicators. No site metadata.Change this to read a lookup table with list of column names.
IndicatorsNumeric=Indicators[,c(2,5,9,10,16,22,23,24:33,37,38,42:89, 109:111)]


### Create seperate column for nonspatial benchmark groups
# Alaska uses stream order, lower 48 uses bankfull width , this section can be modified in the future to include other nonspatial categories if needed
IndicatorsNumeric$NonSpatialBenchmarkGroup=ifelse(IndicatorsNumeric$BLM_AdminState=='AK'& IndicatorsNumeric$StreamOrder %in% c(1,2),"SmallStreamOrder",
                                                  ifelse(IndicatorsNumeric$BLM_AdminState=='AK'& IndicatorsNumeric$StreamOrder %in% c(3,4,5,6),"LargeStreamOrder",
                                                         ifelse(as.numeric(IndicatorsNumeric$BankfullWidthAvg)>10,"LargeBankfullWadeable","SmallBankfullWadeable")))
IndicatorsNumeric$NonSpatialBenchmarkGroup=ifelse(IndicatorsNumeric$ProtocolType=='Boatable',"Boatable",IndicatorsNumeric$NonSpatialBenchmarkGroup)


#get indicators into long format to match format of benchmark group spatial file and to make applying benchmarks easier
IndicatorsLong=reshape2::melt(IndicatorsNumeric,id.vars=c('EvaluationID','BLM_AdminState','EcoregionStreamSize','ProtocolType','StreamOrder','SampledMidLatitude','SampledMidLongitude','NonSpatialBenchmarkGroup'),variable.name="Indicator")

#subset indicators to those that have nonspatial benchmark groups and those that dont
attributedpoints5=subset(attributedpoints4, is.na(NonSpatialBenchmarkGroup)==TRUE)
attributedpoints6=subset(attributedpoints4, is.na(NonSpatialBenchmarkGroup)==FALSE)

#join indicators to benchmarks based on if nonspatial benchmark groups are needed or not
IndicatorValuesBenchmarks1=inner_join(attributedpoints5,IndicatorsLong, by=c('BLM_AdminState','Indicator','EvaluationID'))
IndicatorValuesBenchmarks1=subset(IndicatorValuesBenchmarks1,select=-c(NonSpatialBenchmarkGroup.x,NonSpatialBenchmarkGroup.y))
IndicatorValuesBenchmarks1$NonSpatialBenchmarkGroup=NA
IndicatorValuesBenchmarks2=inner_join(attributedpoints6,IndicatorsLong, by=c('BLM_AdminState','Indicator','EvaluationID','NonSpatialBenchmarkGroup'))
#merge all indicators back together in one file
IndicatorValuesBenchmarks=rbind(IndicatorValuesBenchmarks1,IndicatorValuesBenchmarks2)


###########################################################################
# Step 3 Apply Benchmarks and get conditions #
###########################################################################
#Start loop. Loop through each row. Create string to evaluate condition.

IndicatorValuesBenchmarks$Condition<-NA


for (i in 1:nrow(IndicatorValuesBenchmarks)) {
  
  #print (paste("Row ", i))
  IndicatorValuesBenchmarks_oneRow = IndicatorValuesBenchmarks[i,] 
  
  eval_stringMaj1 <- paste(IndicatorValuesBenchmarks_oneRow$value, IndicatorValuesBenchmarks_oneRow$MajorToModerateRel1, IndicatorValuesBenchmarks_oneRow$MajorBenchmark1)
  eval_stringMin1 <- paste(IndicatorValuesBenchmarks_oneRow$value, IndicatorValuesBenchmarks_oneRow$MinimalToModerateRel1, IndicatorValuesBenchmarks_oneRow$ModerateBenchmark1)
  eval_stringMaj2 <- paste(IndicatorValuesBenchmarks_oneRow$value, IndicatorValuesBenchmarks_oneRow$MajorToModerateRel2, IndicatorValuesBenchmarks_oneRow$MajorBenchmark2)
  eval_stringMin2 <- paste(IndicatorValuesBenchmarks_oneRow$value, IndicatorValuesBenchmarks_oneRow$MinimalToModerateRel2, IndicatorValuesBenchmarks_oneRow$ModerateBenchmark2)
  eval_stringMajMin1 <- paste(IndicatorValuesBenchmarks_oneRow$value, IndicatorValuesBenchmarks_oneRow$MajorToMinimalRel1, IndicatorValuesBenchmarks_oneRow$MajorBenchmark1)
  eval_stringMajMin2 <- paste(IndicatorValuesBenchmarks_oneRow$value, IndicatorValuesBenchmarks_oneRow$MajorToMinimalRel2, IndicatorValuesBenchmarks_oneRow$MajorBenchmark2)
  
  
  #eval(parse(text = eval_stringMaj1))
  #eval(parse(text = eval_stringMin1))
  #eval(parse(text = eval_stringMaj2))
  #eval(parse(text = eval_stringMin2))
  IndicatorValuesBenchmarks[i,"Condition"]=
    ifelse(IndicatorValuesBenchmarks_oneRow$ConditionCategoryNum==3,  
           ifelse(IndicatorValuesBenchmarks_oneRow$IncreaserDecreaser=="Increases with stress"|IndicatorValuesBenchmarks_oneRow$IncreaserDecreaser=="Decreases with stress",
                  ifelse(eval(parse(text = eval_stringMaj1)),"Major",
                         ifelse(eval(parse(text = eval_stringMin1)),"Minimal", "Moderate")
                  ),          
                  
                  ifelse(IndicatorValuesBenchmarks_oneRow$IncreaserDecreaser=="Decreases and increases with stress",
                         ifelse(eval(parse(text = eval_stringMaj1)) |eval(parse(text = eval_stringMaj2)) ,"Major",
                                ifelse(eval(parse(text = eval_stringMin1)) & eval(parse(text = eval_stringMin2)),"Minimal","Moderate"))
                         ,NA)),
           # if statements for 2 categories         
           ifelse(IndicatorValuesBenchmarks_oneRow$IncreaserDecreaser=="Increases with stress"|IndicatorValuesBenchmarks_oneRow$IncreaserDecreaser=="Decreases with stress",
                  ifelse(eval(parse(text = eval_stringMajMin1)),"Major","Minimal"),
                  
                  
                  ifelse(IndicatorValuesBenchmarks_oneRow$IncreaserDecreaser=="Decreases and increases with stress",
                         ifelse(eval(parse(text = eval_stringMajMin1)) |eval(parse(text = eval_stringMajMin2)) ,"Major","Minimal")
                         ,NA))
    )
  
}



############################################################################
# Step 4 format and write out as a feature class
###########################################################################
IndicatorValuesBenchmarksFormatted=IndicatorValuesBenchmarks[,c("BLM_AdminState","EvaluationID","PointID", "SampledMidLatitude","SampledMidLongitude","Indicator","value","BenchmarkGroup","BenchmarkSource", "BenchmarkMethod","ModerateBenchmark1","MajorBenchmark1","ModerateBenchmark2","MajorBenchmark2","Condition")]

library(arcgisbinding)
# Get spatial information from exisitng spatial file already in correct coordinatesystem/projection
dat<-arc.open('I_Indicators')
wkt<-arc.shapeinfo(dat)$WKT
#write to geodatabase
arc.write(path='',data=IndicatorValuesBenchmarksFormatted, shape_info=list(type='Point', WKT=wkt), overwrite=TRUE)



