library(rvest)
library(dplyr)
ted_master <-
  read.csv('master_list_ted.csv',
           stringsAsFactors = F,
           colClasses = 'character')
views_master <- c()
time_master <- c()
for (i in 1:nrow(ted_master)) {
  ptime <- proc.time()
  url <- as.character(ted_master$talk_url[i])
  xpath <- '//*[@id="sharing-count"]/span'
  views <-
    as.numeric(str_replace_all(
      str_trim(
        url %>% read_html() %>% html_nodes(xpath = xpath) %>% html_text()
      ),
      ',',
      ''
    ))
  views_master <- rbind(views_master, views)
  cat(paste0('talk ', i))
  cat('\n')
  time_taken <- proc.time() - ptime
  time_master <- c(time_master, time_taken)
  cat(paste0('time taken: ', time_taken['user.self']))
  cat('\n')
  if (i == nrow(ted_master))
  {
    write.csv(views_master, 'views_ted.csv', row.names = F)
  }
}