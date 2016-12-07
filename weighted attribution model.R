install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("forecast")
install.packages("stringr")
install.packages("plyr") 
install.packages("utils")
library(data.table)
library(stringr)
library(plyr)

names(DATASET)
names(DATASET) <- c("path", "placement", "conversion", "conversion value")
newdata <- as.data.frame(DATASET)
newdata$path <- as.character(newdata$path)
newdata$placement <- as.character(newdata$placement)
newdata$path <- strsplit(newdata$path, " > ")
newdata$placement <- strsplit(newdata$placement, " > ")

newdata2 <- newdata
#View(newdata2)

#get smallest form of paths
for (i in 1:length(newdata$placement)){
  for (j in 1:length(newdata$placement[[i]])){
    if (newdata$placement[[i]][j] == "(unavailable)")
      newdata$placement[[i]][j] <- newdata$path[[i]][j]
  }
}

View(newdata)

for (i in 1:length(newdata$placement)){
  if ((length(newdata$placement[[i]])) == 2)
    if ((newdata$placement[[i]][length(newdata$placement[[i]])]) == (newdata$placement[[i]][length(newdata$placement[[i]])-1]))
      newdata$placement[[i]] = newdata$placement[[i]][-(length(newdata$placement[[i]]))] 
}

for (i in 1:length(newdata$placement)){
  if ((length(newdata$placement[[i]])) > 1){
      
      #creating columns for last DONE
      newdata$last[[i]] <- (newdata$placement[[i]][(length(newdata$placement[[i]]))])
      

      #deleting last DONE
      newdata$placement[[i]] = newdata$placement[[i]][-(length(newdata$placement[[i]]))]
      

      #unique DONE
      newdata$placement[[i]]=unique(newdata$placement[[i]][1:(length(newdata$placement[[i]]))])
      
 
      #APPEND
      newdata$placement[[i]] = append(newdata$placement[[i]],newdata$last[[i]])
      
      #print(newdata$placement[[369]][length(newdata$placement[[369]])])
      #print(newdata$placement[[369]][length(newdata$placement[[369]])-1])
        
      #get rid of repeated last and second last stage  
      if ((newdata$placement[[i]][length(newdata$placement[[i]])]) == (newdata$placement[[i]][length(newdata$placement[[i]])-1]))
        newdata$placement[[i]] = newdata$placement[[i]][-(length(newdata$placement[[i]]))]
    }
}

View(newdata)



#create new table`
usable <- newdata[-1]
usable <- usable[-4]
View(usable)



# FUNCTION to clean Conversion Value data
cleanValue <- function(str){
  # Remove SGD
  str <- substr(str, 4, nchar(str))
  # Remove ','
  str <-gsub(",", "", str)
  
  return(as.double(str))
}

# Convert column into numeric data
usable$`conversion value` <- lapply(as.character(usable$`conversion value`), function(x) cleanValue(x)) 

lastclick <- as.data.frame(usable)


#assuming that impressions have half the value of clicks, count path weight and unit(0.5) value
usable$counti = sapply(usable$placement, function(x) sum(str_count(x, "(impression)")))
for (i in 1:length(newdata$placement)){
  usable$count[[i]] <- (length(newdata$placement[[i]]))}
usable$weight = as.numeric(usable$counti)*0.5 + (as.numeric(usable$count)-as.numeric(usable$counti))
usable$unit = round((as.numeric(usable$`conversion value`)/as.numeric(usable$weight)), digits=2)


View(usable)


#splitting list into rows
myFunc <- function(x) unlist(strsplit(unlist(x), ","))
z = data.frame(`Path` = myFunc(usable$placement), Value = rep(usable$unit, sapply(usable$placement, function(x) length(myFunc(x)))))

View(z)

z2 <- z
View(z2)


#double the value if it is a click
for (i in 1:length(z2$Path)) {
  if (grepl('(impression)', z2$Path[i]))
    z2$Value[i] = (as.numeric(z2$Value[i]))/2
  }
View(z2)
z3 = aggregate(z2$Value, by=list(Placement=z2$Path), FUN=sum)
names(z3) <- c("Placement", "Attributed Conversion Value")

install.packages("xlsx")
library(xlsx)
#write.xlsx(z2, "C:/Users/.../Downloads/z.xlsx")
write.xlsx(z3, "C:/Users/..../Downloads/z3.xlsx")



#create last click attribution to compare
View(lastclick)

for (i in 1:length(lastclick$placement)){
  if ((length(lastclick$placement[[i]])) > 1){
lastclick$placement[[i]] <- (lastclick$placement[[i]][(length(lastclick$placement[[i]]))])
  }
}

lastclick$`conversion value` = as.numeric(lastclick$`conversion value`)
lastclick$placement = as.character(lastclick$placement)
lastclick = aggregate(lastclick$`conversion value`, by=list(Placement=lastclick$placement), FUN=sum)
View(lastclick)
write.xlsx(lastclick, "C:/Users/..../Downloads/lc.xlsx")
