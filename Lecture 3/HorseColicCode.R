HorseColic <- read.table("~/Desktop/UVA/Fall 2016/DS/HorseColicOriginalData.txt")
HC <- HorseColic
View(HC)

#for data frame in which cells are equal to "?", replace them with NA
HC[HC == "?"] <- NA

#change column names
colnames(HC) <- c('surgery?', 'Age', 'Hospital.Number', 'Rectal.Temp', 'Pulse', 'Resp.Rate', 'Temp.of.Extremeties')
