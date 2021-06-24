get_albums <- function(album_table, artist){
  if(!'album_name' %in% names(album_table)){
    stop('album column must be named `album_name`')
  }
  map_dfr(album_table$album_name, 
          ~ genius_album(artist = artist, 
                         album = ., 
                         info = 'all')) %>% 
    left_join(album_table)
}