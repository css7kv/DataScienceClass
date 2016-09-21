HorseColic <- read.table("~/Desktop/UVA/Fall 2016/DS/Lecture 4/HorseColicOriginalData.txt")
HC <- HorseColic
HC[HC == "?"] <- NA
colnames(HC) <- c('surgery?','Age', 'Hospital.Number', 'Rectal.Temp', 'Pulse', 'Resp.Rate', 'Temp.of.Extremities', 'Peripheral.Pulse','Mucous.Membranes', 'Cap.Refill.Time', 'Pain', 'Peristalsis', 'Abdominal.Distension','Nasogastric.Tube', 'Nasogastric.Reflux', 'Nasogastric.Reflux.Ph', 'Rectal.Exam.Feces', 'Abdomen','Packed.Cell.Volume', 'Total.Protein', 'Abdominocentesis.Appearance', 'Abdominocentesis.total.protein', 'Outcome','Surgical.Lesion?','Type.of.Lesion','Type.of.Lesion2','Type.of.Lesion3','cp_data')
summary(HC)
library(DMwR)

#every row of HC that is missing 20% or more of its data
#after running the command, default of manyNAs(HC) is 0.2 (so for line 13 you don't have to specify)
manyNAs(HC,0.2)

#removing rows missing 20% or more of its data
HC <- HC[-manyNAs(HC),]

#total number of rows of HC
nrow(HC)
summary(HC)

#nullify an atribute
HC$Nasogastric.Reflux.Ph <- NULL

summary(HC)
library(ggplot2)
#pairs(HC)
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(HC,"Nasogastric.Reflux.Ph",which(as.numeric(HC$Nasogastric.Reflux.Ph) > 1000),NA)



#manyNAs(HC,0.2)
#HC <- HC[-manyNAs(HC),]
#nrow(HC)
#colnames(HC)
#colnames(HC[27])
