HorseColic <- read.table("~/Desktop/UVA/Fall 2016/DS/HorseColicOriginalData.txt")
HC <- HorseColic
View(HC)

#for data frame in which cells are equal to "?", replace them with NA
HC[HC == "?"] <- NA

#change column names
colnames(HC) <- c('surgery?','Age', 'Hospital.Number', 'Rectal.Temp', 'Pulse', 'Resp.Rate', 'Temp.of.Extremities', 'Peripheral.Pulse','Mucous.Membranes', 'Cap.Refill.Time', 'Pain', 'Peristalsis', 'Abdominal.Distension','Nasogastric.Tube', 'Nasogastric.Reflux', 'Nasogastric.Reflux.Ph', 'Rectal.Exam.Feces', 'Abdomen','Packed.Cell.Volume', 'Total.Protein', 'Abdominocentesis.Appearance', 'Abdominocentesis.total.protein', 'Outcome','Surgical.Lesion?','Type.of.Lesion','Type.of.Lesion2','Type.of.Lesion3','cp_data')
View(HC)
summary(HC)

#get rid of Nasogastric.Reflux.Ph since majority of horses are NA
HC$Nasogastric.Reflux.Ph <- NULL

hist(HC$Type.of.Lesion3)

#writing a function to replace outliers with NA's
outlierReplace = function(dataframe, cols, rows, newValue = NA){
  if(any(rows)){
    sets(dataframe, rows, cols, newValue)
  }
}

#call the function
outlierReplace(HC, "Type.of.Lesion3", which(HC$Type.of.Lesion3 > 1000), NA)
