library(tidyverse)
library(rvest)
library(polite)
library(purrr)
library(stringr)
library(sf)
#run session
session=bow("https://www.ncleg.gov/Members/MemberList/S")
res=scrape(session,list("s"=""))
elements = html_elements(res, ".col-8.col-md-7.pr-0")
#pull senatorial info
senators=map_dfr(elements,function(element) {
  senator_name = html_element(element,"p:nth-child(1) > a") %>% html_text2()
  senator_district = html_element(element,".member-col:nth-child(48) p:nth-child(3) a , .text-nowrap+ p a , p:nth-child(2) a") %>% html_text2()
  senator_party = html_element(element,"p:nth-child(1)") %>% html_text2()
  link = html_element(element,"p:nth-child(1) > a") %>% html_attr("href")
  return(list(
    "name"=senator_name,
    "district"=as.numeric(str_extract(senator_district, "[0-9]+$")),
    "party"=senator_party,
    "link"=link
  ))
})
#clean up formatting
senators <- mutate(senators,party=str_extract(party,"(?<=\\()[:alpha:](?=\\))"))
#get senators' links
get_terms = function(link) {
  temp_session <<- nod(session, link)
  resp = scrape(temp_session)
  term_length = html_elements(resp, ".col-12.col-sm-7.col-md-8.col-lg-9.col-xl-4.order-3.order-sm-4.align-self-center.align-self-xl-start.mt-3.mt-sm-0 > div > div:nth-child(2)") %>% html_text2()
  return(term_length)
}
#add in the links
senators = rowwise(senators, everything()) %>%
  mutate(term_length=list(get_terms(link)))
#clean up formatting
senators$term_length <- as.numeric(str_extract(as.character(senators$term_length), "^[0-9]+"))
senators$link<-NULL
#fix senatorial issues
senators[4,4] <- as.numeric("0")
senators[48,2] <- as.numeric("17")
#find the average experience
senators %>% group_by(party) %>% summarize(avg_length=mean(term_length,na.rm=TRUE))
#plot
districting = read_sf("SL 2021-173 Senate - Shapefile/SL 2021-173 Senate.shp")
ggplot() +
  geom_sf(data=merge(districting,(senators %>% rename(DISTRICT=district))),aes(fill=party)) + scale_fill_manual(values=c("blue","red"))
#ggsave("republican-districts.png")
ggplot() +
  geom_sf(data=merge(districting,(senators %>% rename(DISTRICT=district))),aes(fill=term_length))
#ggsave("term-lengths.png")