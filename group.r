## dependencies
library(dplyr)


## keywords laden
keywords <- read.csv("bfgl.csv", sep=",") %>% select(keyword)


## frequentietabel
woorden <- paste(keywords$keyword, collapse=" ")
uniekeWoorden <- strsplit(woorden, " ")[[1]]
woordFreq <- as.data.frame(table(uniekeWoorden)) %>%
  arrange(desc(Freq))
rm(uniekeWoorden, woorden)