library(rvest)
library(Rcpp)
library(xml2)
#install.packages("lubridate")
library(lubridate)
library(tibble)
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)

tar<-"https://news.joins.com/politics/bluehouse/list/1?filter=OnlyJoongang"

max_page<-function(tar_url){
  read_html(tar_url) %>%
    html_nodes("a.link_page") %>%
    html_text() %>%
    as.numeric() %>%
    max() %>%
    return()
}

max<-max_page(tar)
max
tar_url<-"https://news.joins.com/politics/bluehouse/list/1?filter=OnlyJoongang"


root<-"https://news.joins.com"

articles<-c()

for(i in 1:max){
  tar_url<-paste0("https://news.joins.com/politics/bluehouse/list/",i,"?filter=OnlyJoongang")
  print(tar_url)
  read_html(tar_url) %>%
    html_nodes("h2.headline.mg") %>% 
    html_nodes("a") %>% 
    html_attr("href") -> link_list

  for(j in 1:length(link_list)) {
    print(paste0(i, j))
    tar <- paste0(root, link_list[j])
    news <- read_html(tar)
    news %>%
      html_nodes("h1#article_title") %>%
      html_text() -> title
    
    news %>%
      html_nodes("div.byline") %>%
      as.character() %>%
      strsplit("em", fixed = T) %>% 
      .[[1]] %>%
      grep("입력", ., value=T) %>%
      gsub(">|입력|</", "", .) %>%
      trimws() %>%
      ymd_hm(tz="Asia/Seoul") -> datetime
    
    news %>%
      html_nodes("span.profile strong a") %>%
      html_text() %>%
      .[1] -> reporter
    
    news %>%
      html_nodes("div#article_body") %>%
      as.character() %>%
      strsplit("<br>|</div>") %>%
      .[[1]] %>%
      trimws() -> body_tem
    body_tem<-body_tem[-grep("(</|<!)",body_tem)]
    body_tem<-body_tem[nchar(body_tem)>1]
    body<-body_tem[-grep("[0-9a-zA-Z.]+@[0-9a-zA-Z]+.[0-9a-zA-Z]", body_tem)]
      
    tem <- tibble(title, datetime, reporter, body)
    
    articles %>%
      bind_rows(tem) -> articles
    
  }
}

readr::write_excel_csv(articles, "test_run.csv")
