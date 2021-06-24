create_lyrics_dict <- function(lyrics_table, song_element = NULL){
  if(!is.null(song_element)){
    lyrics_table <- lyrics_table %>% 
      filter(str_detect(element,song_element))
  }
  lines <- lyrics_table %>% 
    pull(lyric) %>% 
    unique()
  
  lines_split <- lines %>% 
    str_remove_all('[^A-z ]') %>% 
    tolower() %>% 
    str_split(' ')
  
  lyrics_dict <- list()
  for(i in 1:length(lines_split)){
    if(length(lines_split[[i]]) > 1){ 
      for(j in 1:(length(lines_split[[i]])-1)){
        if(!lines_split[[i]][j] %in% names(lyrics_dict)){
          lyrics_dict[lines_split[[i]][j]] <- lines_split[[i]][j+1]
        } else {
          lyrics_dict[[lines_split[[i]][j]]] <- unique(c(lyrics_dict[[lines_split[[i]][j]]], lines_split[[i]][j+1]))
        }
      }
    }
  }
  lyrics_dict
}