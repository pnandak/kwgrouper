
## woorden getagd als concurrent opslaan in een vector

temp <- wordtagsdimstypes %>%
  filter(Dimensie=="concurrent") %>%
  select(keyword)

brandwords <- as.character(temp[,1])

rm(temp)

## woorden die getagd zijn als concurrent (opgeslagen in 'brandwords') en die
## niet de dimensie concurrent hebben marken als te deleten

test <- wordtagsdimstypes %>%
  filter(keyword %in% brandwords, Dimensie!="concurrent") %>%
  mutate(delete="yes")

## mergen met origineel dataframe

test2 <- merge(wordtagsdimstypes,test, all.x=TRUE)

## obsolete rows deleten

processedData <- test2 %>% filter(is.na(delete)) %>% select(keyword,volume,tag,Dimensie,Type)

rm(test1,test2)

## voorbereiden voor tableay-creator Nick


tableau <- processedData %>%
  select(keyword,volume,tag)
