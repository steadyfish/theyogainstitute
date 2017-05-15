# download data
  
  # other references:
  # Mixcloud API doc: https://www.mixcloud.com/developers/
library(jsonlite)
library(plyr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(lubridate)
library(RecordLinkage)
download_data = function(baseurl = "https://api.mixcloud.com/theyogainstitute/feed/", batch_size = 20){
  end_indicator = FALSE
  loop_ind = 1
  # batch_size = 20
  l_in = list()
  
  while(!end_indicator)
  {
    
    offset = paste("&offset=", (loop_ind-1)*batch_size, sep = "")
    url = paste0("https://api.mixcloud.com/theyogainstitute/feed/?limit=", batch_size, offset)  
    message("Retrieving page ", loop_ind, " - ", url)
    j_in = fromJSON(url, flatten = TRUE)
    l_in[[loop_ind]] = rbind.pages(j_in$data$cloudcasts)
    
    loop_ind = loop_ind + 1
    end_indicator = !("next" %in% names(j_in$paging))
  }
  
  d_in = rbind.pages(l_in)
  return(d_in)  
}

d_in = download_data()

vars = c("play_count", "key", "created_time", "audio_length", "slug", "favorite_count", "listener_count", "name", "url")
d_in1 = d_in %>%
  select_(.dots = vars)


# approach 1: separate fields by comma
d_in2 = d_in1 %>%
  mutate(name1 = tolower(name)) %>%
  mutate(name2 = str_replace(name1, "parisamvad ", "parisamvad,")) %>%
  separate(name2, into = c("spec1", "date", "topic", "given_by", "misc"), sep = ",")

d_in3 = d_in2 %>%
  mutate(date1 = dmy(date))

# approach 2: remove unwanted words
# d_in3 = d_in1 %>%
  # mutate(name_cln = str_replace_all(string = tolower(name), 
                                    # c("parisamvad" = "", "jayadeva" = "", "yogendra" = "", 
                                      # "dr." = "", "smt." = "", "hansaji" = "")) 
         # )

# approach 3: extract elements from "slug" with proper regex
d_in1[, c("spec1", "date", "topic")] = str_match(string = d_in1$slug, 
                                                 pattern = "([\\S]*?parisamvad)-??([\\S]*?(2015|2016|2017))-??(([\\S])*?(smt|dr|$))")[, c(2, 3, 5)]
View(table(d_in1$topic, useNA = "always"))
View(d_in1 %>% filter(is.na(topic)))



d_in2 = d_in1 %>% 
  mutate(topic1 = str_replace(topic, "^-", ""),
         topic2 = str_replace(topic1, "(-smt|-dr)$", ""))
View(table(d_in2$topic2, useNA = "always"))



# format:
str_view_all(string = a, pattern = "([\\S]*?parisamvad)")
str_match(string = a, pattern = "([\\S]*?parisamvad)([\\S]*?(2016|2017|16|17))")
([text]*[parisamvad])-([alphanumeric text]*[-16- | -17- | -2016- | -2017-])(alphanumeric)
