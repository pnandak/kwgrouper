## dependencies laden

library(xlsx)
library(plyr)
library(dplyr)

## excel ding laden

df <- read.csv("tableau.csv") %>% select(keyword,volume,tag)

## Keywordindex aanmaken

keywords <- as.character(unique(df$keyword))

## Keyword en Dimensies selecteren

df_dplyr <- select(df, keyword, tag)

## tellen hoeveel kolommen we nodig hebben in het final df

  ## unieke combo's in een tijdelijkke tabel zetten

  colcount <- sort(table(df_dplyr$keyword),decreasing=TRUE)[1]

## dataframe aanmaken met voldoende kolommen

df_final <- as.data.frame(matrix(ncol = colcount))

for (i in seq(keywords)) {
  
  ## Unieke dimensies overhouden
  
  ttest <- distinct(filter(df_dplyr, keyword == keywords[i]))
  
  ## Keyword als kolomnaam wegzetten
  
  colnames(ttest) <- c("Keyword",as.character(ttest[1,1]))
  
  ## Eerste kolom wegknikkeren
  
  trow <- select(ttest, 2)
  
  ## Transposen, van factor characers maken en rownamen extracten en storen als final row

  frow <- as.data.frame(t(trow))
  frow[] <- lapply(frow, as.character)
    
  ## In dataframe plempen
  df_final <- rbind.fill(df_final, cbind(rownames(frow),frow))
}

## DF formatten

df_final <- select(df_final, 6, 1, 2, 3, 4, 5)
colnames(df_final) <- c("Keyword", "Tag 1", "Tag 2", "Tag 3", "Tag 4")

## excel wegschrijven

write.xlsx(df_final, "KeywordChecklist3.xlsx", sheetName="Woorden en tags", row.names=F, showNA=F)
write.xlsx(tags, "tags.xlsx", sheetName="Tags en dimensies", row.names=F, showNA=F)