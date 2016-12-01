# TED scrape --------------------------------------------------------------


library(rvest)
library(dplyr)
library(stringr)
# dir_path <- setwd('~/Downloads/ted_talks/')

url <- "https://www.ted.com/talks?page="
class_path <- ".media__message"

total_pages <- ".results__pagination"
data <-
  url %>% read_html() %>% html_nodes(".results__pagination") %>% html_text()
last_page <-
  as.numeric(str_sub(
    str_sub(data,-8,-1),
    1,
    str_locate("str_sub(data,-8,-1)", "|")[[1]] + 1
  ))

master_list <- c()
for (i in 1:last_page) {
  cat(paste0("page number: ",i))
  cat('\n')
  # Sys.sleep(10)
  data <-
    paste0(url, i) %>% read_html() %>% html_nodes(class_path) %>% html_text()
  for (j in 1:length(data)) {
    xpath <-
      paste0(
        "//*[@id=",
        '"browse-results"]/div[1]/div[',
        j,
        "]/div/div/div/div[2]/h4[2]/a"
      )
    post_url <-
      paste0(url, i) %>% read_html() %>% html_nodes(xpath = xpath) %>% html_attrs()
    temp_url <- paste0('https://ted.com', unlist(post_url)[2])
    speaker_xpath <-
      '//*[@id="talk-pusher"]/div[1]/div[2]/div[1]/div[2]/div/div/div[3]/a'
    
    
    talk_summary_xpath <-
      '//*[@id="talk-pusher"]/div[1]/div[2]/div[1]/p/text()'
    talk_summary <-
      str_trim(
        temp_url %>% read_html() %>% html_nodes(xpath = talk_summary_xpath) %>% html_text()
      )
    
    # Sys.sleep(10)
    
    
    speaker_details <-
      temp_url %>% read_html() %>% html_nodes(xpath = speaker_xpath) %>% html_attrs
    speaker_details <-
      paste0('https://ted.com', unlist(speaker_details)[2])
    
    if (length(speaker_details) > 0) {
      speaker_summary_xpath <-
        '//*[@id="shoji"]/div[2]/div/div[2]/div[2]/div/div/div[2]/div[1]/div/div[1]/text()'
      speaker_links_xpath <-
        '//*[@id="shoji"]/div[2]/div/div[2]/div[1]/div/div/div[2]/div/div[1]/div/div[4]/div/a'
      
      speaker_data <-
        speaker_details %>% read_html() %>% html_nodes(xpath = speaker_summary_xpath) %>% html_text()
      speaker_data <-
        ifelse(length(speaker_data) > 0, speaker_data, 'not available')
      
      # Sys.sleep(10)
      
      speaker_title_xpath <-
        '//*[@id="shoji"]/div[2]/div/div[2]/div[1]/div/div/div[2]/div/div[1]/div/div[2]/div/div'
      speaker_title <-
        str_trim(
          speaker_details %>% read_html() %>% html_nodes(xpath = speaker_title_xpath) %>% html_text()
        )
      speaker_title <-
        ifelse(length(speaker_title) > 0, speaker_title, 'not available')
      
      speaker_links <-
        speaker_details %>% read_html() %>% html_nodes(xpath = speaker_links_xpath) %>% html_attrs()
      speaker_twitter <-
        unlist(speaker_links)[grepl('twitter.com', unlist(speaker_links))]
      speaker_twitter <-
        ifelse(length(speaker_twitter) > 0,
               speaker_twitter,
               'not available')
      
      # Sys.sleep(10)
      
    }
    
    temp <- unlist(str_split(data[j], '\n'))
    speaker <- temp[2]
    topic <- temp[5]
    posted <- temp[12]
    tags <- temp[18]
    temp <-
      data.frame(
        "speaker" = speaker ,
        "speaker_title" = speaker_title,
        "topic" = topic,
        "talk_summary" = talk_summary,
        "speaker_bio" = speaker_data,
        "talk_url" = temp_url,
        "speaker_twitter" = speaker_twitter,
        "speaker_ted_url" = speaker_details,
        "posted" = posted,
        "tags" = tags
      )
    master_list <- rbind(master_list, temp)
    cat(paste0(j, 'talks have been completed'))
    cat('\n')
    if (j %% 5 == 0 & j < last_page) {
      write.csv(master_list,
                paste0('master_list_temp_', j, '.csv'),
                row.names = F)
      cat(paste0(j, 'talks have been saved'))
      cat('\n')
    }
    if (i == last_page & j == length(data)) {
      write.csv(master_list, paste0('master_list_ted.csv'), row.names = F)
    }
    
  }
  
  
  
}
