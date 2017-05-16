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
library(networkD3)
library(data.tree)
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
    # message("Retrieving page ", loop_ind, " - ", url)
    j_in = fromJSON(url, flatten = TRUE)
    l_in[[loop_ind]] = rbind.pages(j_in$data$cloudcasts)
    
    loop_ind = loop_ind + 1
    end_indicator = !("next" %in% names(j_in$paging))
  }
  
  d_in = rbind.pages(l_in)
  return(d_in)  
}

clean_data = function(d_in, keep_vars = c("play_count", "key", "created_time", "audio_length", "slug", "favorite_count", "listener_count", "name", "url")){
  d_in1 = d_in %>%
    select_(.dots = keep_vars)

  # d_in2 = d_in1 %>%
  #   mutate(name1 = tolower(name)) %>%
  #   mutate(name2 = str_replace(name1, "parisamvad ", "parisamvad,")) %>%
  #   separate(name2, into = c("spec1", "date", "topic", "given_by", "misc"), sep = ",")
  # 
  # d_in3 = d_in2 %>%
  #   mutate(date1 = dmy(date))

  d_in1[, c("spec1", "date", "topic")] = str_match(string = d_in1$slug, 
                                                   pattern = "([\\S]*?parisamvad)-??([\\S]*?(2015|2016|2017))-??(([\\S])*?(smt|dr|$))")[, c(2, 3, 5)]

  d_in2 = d_in1 %>% 
    mutate(topic1 = str_replace(topic, "^-", ""),
           topic2 = str_replace(topic1, "(-smt|-dr)$", "")) %>%
    select(-c(topic, topic1)) %>%
    rename(topic = topic2) %>%
    select(topic, url, created_time, play_count, audio_length)

  return(d_in2)  
}

link_data = function(d_in){
  d_link = read.csv("../data/dictionary.csv", header = TRUE, stringsAsFactors = FALSE)
  
  d_in1 = d_link %>%
    full_join(d_in, by = c("topic" = "topic"))
  
  d_in2 = d_in1 %>%
    # filter(!is.na(topic)) %>%
    mutate(t = str_detect(topic, "bg"),
           category = if_else(
             str_detect(topic, fixed("bg")), "bhagwad gita",
             if_else(str_detect(topic, fixed("pys")), "patanjali yogasutra",
                     if_else(is.na(category), "other", category))))
  
  return(d_in2)
}

plot_data = function(d_in){
  d_in1 = FromDataFrameNetwork(d_in)
  d_in2 = ToListExplicit(d_in1, unname = TRUE)
  
  radialNetwork(List = d_in2, height = 1000, width = 1000, fontSize = 15, fontFamily = 'helvetica', opacity = 0.8, nodeColour = "cyan")
  # diagonalNetwork(d_in2)
}

# approx join for linking data
# d_in5 = compare.dedup(d_in4, strcmp = c(1), strcmpfun = jarowinkler)
# hist(d_in5$pairs$topic)
# # trying cutoff as 0.75 based on the histogram
# # computing weights (don't understand the reason)
# d_in6 = emWeights(d_in5, cutoff = 0.75)
# d_in7 = getPairs(d_in6, max.weight = 1, min.weight = 0.1)
