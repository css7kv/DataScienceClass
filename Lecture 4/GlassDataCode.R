library(DMwR)

glassdata <- read.csv("~/Desktop/UVA/Fall 2016/DS/Lecture 4/glassdata.txt", header=FALSE, stringsAsFactors=FALSE)
View(glassdata)
colnames(glassdata) <- c('instance.number','RI','Na','Mg','Al','Si','K','Ca','Ba','Fe','Type')
View(glassdata)
manyNAs(glassdata,0.2)
glassdata <- glassdata[-manyNAs(glassdata),]
nrow(glassdata)
View(glassdata)
sum(is.na(glassdata))

## Let's explore correlation

#looks at all the rows, and all columns except ID # and outcome
symnum(cor(glassdata[,2:10], use = "complete.obs"))

## Now that we know there's a correlation, can we use it?
#show me a line in which the Ca value is na?
which(is.na(glassdata$Ca))

#making a linear model
lm(Ca~RI,data=glassdata)

#assign the missing value based on the eqn found from the linear model
glassdata[13,"Ca"]<- -71.1+382*glassdata[13,"RI"]
sum(is.na(glassdata))
which(is.na(glassdata$RI))

## Next, let's try kNN imputation
glassdata <- knnImputation(glassdata, k=10)
sum(is.na(glassdata))

## Finally, let's try mean imputation:

glassdata$Na[is.na(glassdata$Na)] <- mean(glassdata$Na, na.rm=TRUE)

## Now replace all missing data by the means for the given attribute.

for (j in 1:length(glassdata[1,]))
{
  
  if (is.numeric(glassdata[,j]))
  {
    for(k in 1:length(glassdata[,1]))
    {
      if(is.na(glassdata[k,j]))
      {
        glassdata[k,j] <- mean(glassdata[,j],na.rm=T)
      }
    }
  }
}


write.csv(glassdata, file = "glassdata.csv")
