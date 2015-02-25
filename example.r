## example(keywordsProcessed)

example <- function(regex="(?=.*load)((?!truck|less|full).)*$",column=ncol(keywordsProcessed),n=20) {
  
  range <- grep(regex,keywordsProcessed$keyword,perl=TRUE)
  
  sample_n(keywordsProcessed[range,c(1,column)],n)
  
}