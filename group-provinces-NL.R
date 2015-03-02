## dependencies

## library(dplyr)
## library(WriteXLS)
## library(reshape2)


## keywords laden

## keywords <- read.csv("bfgl.csv", sep=",") %>% select(keyword)

## processing code

keywordsProcessed <- keywords %>%
  
  mutate(
    
    ## 1. Zuid-Holland
    zuidholland=grepl("(?=.*zuid)(?=.*holland)",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 2. Noord-Holland
    noordholland=grepl("(?=.*noord)(?=.*holland)",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 3. Noord-Brabant
    noordbrabant=grepl("brabant",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 4. Overijssel
    overijssel=grepl("(?=.*over)(?=.*(ijssel|ijsel))",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 5. Utrecht
    utrecht=grepl("utrecht",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 6. Drenthe
    drenthe=grepl("drente|drenthe",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 7. Friesland
    friesland=grepl("(?=.*(frys|fries))(?=.*(lân|land))",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 8. Gelderland
    gelderland=grepl("(?=.*gelder)(?=.*land)",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 9. Limburg
    limburg=grepl("limburg",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 10. Zeeland
    zeeland=grepl("(?=.*zee)(?=.*land)",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 11. Groningen
    groningen=grepl("groningen",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 12. Flevoland
    flevoland=grepl("(?=.*flevo)(?=.*land)",keyword,ignore.case=TRUE,perl=TRUE)
    
  )
