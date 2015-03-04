## example(keywordsProcessed)

tagchecker <- function(regex="(?=.*load)((?!truck|less|full).)*$",fnct="words",n=0) {
  
  range <- grep(regex,keywordsLong$tag,perl=TRUE)
  
  n <- ifelse(n==0,length(range),n)
  
  if(fnct=="words") { 
    
    sample_n(keywordsLong[range,c(1,2)],n)
  
  } else if(fnct=="length") {
    
    length(range)
    
  } else { print("invalid input") }
   
}