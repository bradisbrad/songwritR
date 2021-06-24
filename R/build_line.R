build_line <- function(lyrics_dict, starter = NULL, length = 6){
  if(is.null(starter)){
    starter <- names(lyrics_dict)[sample(1:length(lyrics_dict), 1)]
  }
  line <- starter
  for(i in 1:length){
    opts <- lyrics_dict[[line[i]]]
    if(is.null(opts)){
      res_line <- str_c(line, collapse = ' ')
      return(res_line)
    }
    line <- c(line, opts[sample(1:length(opts), 1)])
  }
  res_line <- str_c(line, collapse = ' ')
  return(res_line)
}
