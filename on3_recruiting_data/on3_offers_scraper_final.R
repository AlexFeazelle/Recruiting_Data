library(rvest)
library(tidyverse)


#on3 links scraper

on3_links_url <- "https://www.on3.com/college/"
on3_links_page <- read_html(on3_links_url)


links_on3 <- tibble(
  college = on3_links_page |> 
    html_elements("span.TeamItem_organization__q7oys") |> 
    html_text(trim = TRUE),
  link = on3_links_page |> 
    html_elements("a.TeamItem_channelLink__FeP_Z") |> 
    html_attr("href")
)
#view(links_on3)
#vroom::vroom_write(links_on3,"links_on3_teams.csv",delim = ",")

on3_links <- links_on3 |> 
  pull(link)
links1 <- links_on3 |> 
slice(1:46) |> 
  pull(link)
links2 <-links_on3 |> 
  slice(47:92) |> 
  pull(link)
links3 <- links_on3 |> 
  slice(93:138) |> 
  pull(link)



links3







#on_3 offers scraper
on3_offer_scraper <- function(year,link) {
url_on3 <- glue::glue("https://www.on3.com{link}football/{year}/offers/")

on3_page <- read_html(url_on3)
offer <- on3_page |> 
  html_elements("div.InterestListFilters_filterWrapper__tBWOD") |> 
  html_element("div") |> 
  html_text(trim = TRUE)
year <- year
  

df <- on3_page |> 
  html_elements("ul.OfferList_offerListContainer__eWhYH") |> 
  html_elements("li") |> 
  map_df(function(x) {
    data.frame(
    name = x |> 
      html_element("div.OfferListItem_name__SzOsx") |> 
      html_text(trim = TRUE),
    school = x |> 
      html_element("a.MuiTypography-root.MuiTypography-inherit.MuiLink-root.MuiLink-underlineHover.OfferListItem_highSchool__Dr2cr") |> 
      html_text(trim = TRUE),
    city_state = x |> 
      html_element("span.OfferListItem_homeTownName__TMk_g") |> 
      html_text(trim = TRUE),
    pos = x |> 
      html_element("span.OfferListItem_position__APGn7") |> 
      html_text(trim = TRUE),
    height = x |> 
      html_element("span.OfferListItem_height__tFr5B") |> 
      html_text(trim = TRUE),
    weight = x |> 
      html_element("p.MuiTypography-root.MuiTypography-body1.OfferListItem_playerVitals__Tq7V2.css-1pxtrc6-MuiTypography-root") |> 
      html_element("span:nth-child(3)") |> 
      html_text(trim = TRUE),
    stars = x |> 
      html_element("span.MuiRating-root.MuiRating-sizeSmall.Mui-readOnly.StarRating_star__GR_Ff.MuiRating-readOnly.css-1lauo1g-MuiRating-root") |> 
      html_attr("aria-label"),
    on3_composite_rating = x |> 
      html_element("span.StarRating_overallRating__wz9dE.StarRating_bolded__ll0Yc.StarRating_border__bPQv1") |> 
      html_text(trim = TRUE),
    nat_rank = x |> 
      html_element("dt.StarRating_ranksNumberWrapper__ZiRGA") |> 
      html_text(trim = TRUE),
    pos_rank = x |> 
      html_element("dl.StarRating_ranksWrapper__hMUow") |> 
   #   html_element("dt:nth-child(2)") |> 
      html_text(trim = TRUE),
   on3_link = x |> 
     html_element("div.OfferListItem_name__SzOsx") |>
     html_element("a") |> 
     html_attr("href"),
   year = year,
  offer = offer
    )}
    ) # |> 
 #data.table::rbindlist(fill = TRUE)
#  mutate(name = str_remove(name,"Claim Profile"),
 ##        pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
   #      state_rank = pos_rank,
    #     pos_rank = str_extract(pos_rank,"[0-9]+"),
     #    state_rank = str_remove(state_rank,"POS[0-9]+"),
#         state_rank = str_extract(pos_rank,"[0-9]+"),
 #        city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
  #       state = str_remove(city,'.*,\\s*'),
   #      city = str_remove(city, ",.*"),
    #     height_ft = str_extract(height,"[0-9]"),
     #    height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
      #   height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
#         year = year,
 #        offer = offer,
  #       year = as.numeric(year),
   #      weight = as.numeric(weight),
    #     stars = str_remove(stars,"Stars"),
     #    stars = as.numeric(stars),
      #   nat_rank = as.numeric(nat_rank),
       #  on3_composite_rating = as.numeric(on3_composite_rating)
      #   ) |> 
#  select(name,year,height,weight,pos,school,city,state,on3_link,
  #       on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer) 
return(df)
}
#26
on3_26_offers <- map(links1,on3_offer_scraper,year = 2026, .progress = TRUE) |> 
  data.table::rbindlist()
on3_26_offers_2 <- map(links2,on3_offer_scraper,year = 2026, .progress = TRUE) |> 
  data.table::rbindlist()
on3_26_offers_3 <- map(links3,on3_offer_scraper,year = 2026, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_26 <- rbind(on3_26_offers,on3_26_offers_2,on3_26_offers_3) |> 
  
   mutate(name = str_remove(name,"Claim Profile"),
          pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
        state_rank = pos_rank,
       pos_rank = str_extract(pos_rank,"[0-9]+"),
      state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
          city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
        city = str_remove(city, ",.*"),
       height_ft = str_extract(height,"[0-9]"),
      height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
     height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
           year = year,
          offer = offer,
         year = as.numeric(year),
        weight = as.numeric(weight),
       stars = str_remove(stars,"Stars"),
      stars = as.numeric(stars),
     nat_rank = as.numeric(nat_rank),
    on3_composite_rating = as.numeric(on3_composite_rating)
     ) |> 
    select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_26)

#25
on3_25_offers <- map(links1,on3_offer_scraper,year = 2025, .progress = TRUE) |> 
  data.table::rbindlist()
on3_25_offers_2 <- map(links2,on3_offer_scraper,year = 2025, .progress = TRUE) |> 
  data.table::rbindlist()
on3_25_offers_3 <- map(links3,on3_offer_scraper,year = 2025, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_25 <- rbind(on3_25_offers,on3_25_offers_2,on3_25_offers_3) |> 
  mutate(name = str_remove(name,"Claim Profile"),
         pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
         state_rank = pos_rank,
         pos_rank = str_extract(pos_rank,"[0-9]+"),
         state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
         city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
         city = str_remove(city, ",.*"),
         height_ft = str_extract(height,"[0-9]"),
         height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
         height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
         year = year,
         offer = offer,
         year = as.numeric(year),
         weight = as.numeric(weight),
         stars = str_remove(stars,"Stars"),
         stars = as.numeric(stars),
         nat_rank = as.numeric(nat_rank),
         on3_composite_rating = as.numeric(on3_composite_rating)
  ) |> 
  select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_25)

#24
on3_24_offers <- map(links1,on3_offer_scraper,year = 2024, .progress = TRUE) |> 
  data.table::rbindlist()
on3_24_offers_2 <- map(links2,on3_offer_scraper,year = 2024, .progress = TRUE) |> 
  data.table::rbindlist()
on3_24_offers_3 <- map(links3,on3_offer_scraper,year = 2024, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_24 <- rbind(on3_24_offers,on3_24_offers_2,on3_24_offers_3) |> 
  
  mutate(name = str_remove(name,"Claim Profile"),
         pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
         state_rank = pos_rank,
         pos_rank = str_extract(pos_rank,"[0-9]+"),
         state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
         city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
         city = str_remove(city, ",.*"),
         height_ft = str_extract(height,"[0-9]"),
         height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
         height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
         year = year,
         offer = offer,
         year = as.numeric(year),
         weight = as.numeric(weight),
         stars = str_remove(stars,"Stars"),
         stars = as.numeric(stars),
         nat_rank = as.numeric(nat_rank),
         on3_composite_rating = as.numeric(on3_composite_rating)
  ) |> 
  select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_24)

#23
on3_23_offers <- map(links1,on3_offer_scraper,year = 2023, .progress = TRUE) |> 
  data.table::rbindlist()
on3_23_offers_2 <- map(links2,on3_offer_scraper,year = 2023, .progress = TRUE) |> 
  data.table::rbindlist()
on3_23_offers_3 <- map(links3,on3_offer_scraper,year = 2023, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_23 <- rbind(on3_23_offers,on3_23_offers_2,on3_23_offers_3) |> 
  
  mutate(name = str_remove(name,"Claim Profile"),
         pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
         state_rank = pos_rank,
         pos_rank = str_extract(pos_rank,"[0-9]+"),
         state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
         city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
         city = str_remove(city, ",.*"),
         height_ft = str_extract(height,"[0-9]"),
         height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
         height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
         year = year,
         offer = offer,
         year = as.numeric(year),
         weight = as.numeric(weight),
         stars = str_remove(stars,"Stars"),
         stars = as.numeric(stars),
         nat_rank = as.numeric(nat_rank),
         on3_composite_rating = as.numeric(on3_composite_rating)
  ) |> 
  select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_23)


#22
on3_22_offers <- map(links1,on3_offer_scraper,year = 2022, .progress = TRUE) |> 
  data.table::rbindlist()
on3_22_offers_2 <- map(links2,on3_offer_scraper,year = 2022, .progress = TRUE) |> 
  data.table::rbindlist()
on3_22_offers_3 <- map(links3,on3_offer_scraper,year = 2022, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_22 <- rbind(on3_22_offers,on3_22_offers_2,on3_22_offers_3) |> 
  
  mutate(name = str_remove(name,"Claim Profile"),
         pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
         state_rank = pos_rank,
         pos_rank = str_extract(pos_rank,"[0-9]+"),
         state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
         city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
         city = str_remove(city, ",.*"),
         height_ft = str_extract(height,"[0-9]"),
         height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
         height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
         year = year,
         offer = offer,
         year = as.numeric(year),
         weight = as.numeric(weight),
         stars = str_remove(stars,"Stars"),
         stars = as.numeric(stars),
         nat_rank = as.numeric(nat_rank),
         on3_composite_rating = as.numeric(on3_composite_rating)
  ) |> 
  select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_22)

#21
on3_21_offers <- map(links1,on3_offer_scraper,year = 2021, .progress = TRUE) |> 
  data.table::rbindlist()
on3_21_offers_2 <- map(links2,on3_offer_scraper,year = 2021, .progress = TRUE) |> 
  data.table::rbindlist()
on3_21_offers_3 <- map(links3,on3_offer_scraper,year = 2021, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_21 <- rbind(on3_21_offers,on3_21_offers_2,on3_21_offers_3) |> 
  
  mutate(name = str_remove(name,"Claim Profile"),
         pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
         state_rank = pos_rank,
         pos_rank = str_extract(pos_rank,"[0-9]+"),
         state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
         city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
         city = str_remove(city, ",.*"),
         height_ft = str_extract(height,"[0-9]"),
         height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
         height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
         year = year,
         offer = offer,
         year = as.numeric(year),
         weight = as.numeric(weight),
         stars = str_remove(stars,"Stars"),
         stars = as.numeric(stars),
         nat_rank = as.numeric(nat_rank),
         on3_composite_rating = as.numeric(on3_composite_rating)
  ) |> 
  select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_21)

#20
on3_20_offers <- map(links1,on3_offer_scraper,year = 2020, .progress = TRUE) |> 
  data.table::rbindlist()
on3_20_offers_2 <- map(links2,on3_offer_scraper,year = 2020, .progress = TRUE) |> 
  data.table::rbindlist()
on3_20_offers_3 <- map(links3,on3_offer_scraper,year = 2020, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_20 <- rbind(on3_20_offers,on3_20_offers_2,on3_20_offers_3) |> 
  
  mutate(name = str_remove(name,"Claim Profile"),
         pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
         state_rank = pos_rank,
         pos_rank = str_extract(pos_rank,"[0-9]+"),
         state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
         city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
         city = str_remove(city, ",.*"),
         height_ft = str_extract(height,"[0-9]"),
         height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
         height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
         year = year,
         offer = offer,
         year = as.numeric(year),
         weight = as.numeric(weight),
         stars = str_remove(stars,"Stars"),
         stars = as.numeric(stars),
         nat_rank = as.numeric(nat_rank),
         on3_composite_rating = as.numeric(on3_composite_rating)
  ) |> 
  select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_20)

#19
on3_19_offers <- map(links1,on3_offer_scraper,year = 2019, .progress = TRUE) |> 
  data.table::rbindlist()
on3_19_offers_2 <- map(links2,on3_offer_scraper,year = 2019, .progress = TRUE) |> 
  data.table::rbindlist()
on3_19_offers_3 <- map(links3,on3_offer_scraper,year = 2019, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_19 <- rbind(on3_19_offers,on3_19_offers_2,on3_19_offers_3) |> 
  
  mutate(name = str_remove(name,"Claim Profile"),
         pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
         state_rank = pos_rank,
         pos_rank = str_extract(pos_rank,"[0-9]+"),
         state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
         city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
         city = str_remove(city, ",.*"),
         height_ft = str_extract(height,"[0-9]"),
         height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
         height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
         year = year,
         offer = offer,
         year = as.numeric(year),
         weight = as.numeric(weight),
         stars = str_remove(stars,"Stars"),
         stars = as.numeric(stars),
         nat_rank = as.numeric(nat_rank),
         on3_composite_rating = as.numeric(on3_composite_rating)
  ) |> 
  select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_19)

#18
on3_18_offers <- map(links1,on3_offer_scraper,year = 2018, .progress = TRUE) |> 
  data.table::rbindlist()
on3_18_offers_2 <- map(links2,on3_offer_scraper,year = 2018, .progress = TRUE) |> 
  data.table::rbindlist()
on3_18_offers_3 <- map(links3,on3_offer_scraper,year = 2018, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_18 <- rbind(on3_18_offers,on3_18_offers_2,on3_18_offers_3) |> 
  
  mutate(name = str_remove(name,"Claim Profile"),
         pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
         state_rank = pos_rank,
         pos_rank = str_extract(pos_rank,"[0-9]+"),
         state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
         city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
         city = str_remove(city, ",.*"),
         height_ft = str_extract(height,"[0-9]"),
         height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
         height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
         year = year,
         offer = offer,
         year = as.numeric(year),
         weight = as.numeric(weight),
         stars = str_remove(stars,"Stars"),
         stars = as.numeric(stars),
         nat_rank = as.numeric(nat_rank),
         on3_composite_rating = as.numeric(on3_composite_rating)
  ) |> 
  select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_18)

#17
on3_17_offers <- map(links1,on3_offer_scraper,year = 2017, .progress = TRUE) |> 
  data.table::rbindlist()
on3_17_offers_2 <- map(links2,on3_offer_scraper,year = 2017, .progress = TRUE) |> 
  data.table::rbindlist()
on3_17_offers_3 <- map(links3,on3_offer_scraper,year = 2017, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_17 <- rbind(on3_17_offers,on3_17_offers_2,on3_17_offers_3) |> 
  
  mutate(name = str_remove(name,"Claim Profile"),
         pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
         state_rank = pos_rank,
         pos_rank = str_extract(pos_rank,"[0-9]+"),
         state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
         city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
         city = str_remove(city, ",.*"),
         height_ft = str_extract(height,"[0-9]"),
         height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
         height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
         year = year,
         offer = offer,
         year = as.numeric(year),
         weight = as.numeric(weight),
         stars = str_remove(stars,"Stars"),
         stars = as.numeric(stars),
         nat_rank = as.numeric(nat_rank),
         on3_composite_rating = as.numeric(on3_composite_rating)
  ) |> 
  select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_17)

#16
on3_16_offers <- map(links1,on3_offer_scraper,year = 2016, .progress = TRUE) |> 
  data.table::rbindlist()
on3_16_offers_2 <- map(links2,on3_offer_scraper,year = 2016, .progress = TRUE) |> 
  data.table::rbindlist()
on3_16_offers_3 <- map(links3,on3_offer_scraper,year = 2016, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_16 <- rbind(on3_16_offers,on3_16_offers_2,on3_16_offers_3) |> 
  
  mutate(name = str_remove(name,"Claim Profile"),
         pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
         state_rank = pos_rank,
         pos_rank = str_extract(pos_rank,"[0-9]+"),
         state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
         city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
         city = str_remove(city, ",.*"),
         height_ft = str_extract(height,"[0-9]"),
         height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
         height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
         year = year,
         offer = offer,
         year = as.numeric(year),
         weight = as.numeric(weight),
         stars = str_remove(stars,"Stars"),
         stars = as.numeric(stars),
         nat_rank = as.numeric(nat_rank),
         on3_composite_rating = as.numeric(on3_composite_rating)
  ) |> 
  select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_16)

#15
on3_15_offers <- map(links1,on3_offer_scraper,year = 2015, .progress = TRUE) |> 
  data.table::rbindlist()
on3_15_offers_2 <- map(links2,on3_offer_scraper,year = 2015, .progress = TRUE) |> 
  data.table::rbindlist()
on3_15_offers_3 <- map(links3,on3_offer_scraper,year = 2015, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_15 <- rbind(on3_15_offers,on3_15_offers_2,on3_15_offers_3) |> 
  
  mutate(name = str_remove(name,"Claim Profile"),
         pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
         state_rank = pos_rank,
         pos_rank = str_extract(pos_rank,"[0-9]+"),
         state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
         city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
         city = str_remove(city, ",.*"),
         height_ft = str_extract(height,"[0-9]"),
         height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
         height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
         year = year,
         offer = offer,
         year = as.numeric(year),
         weight = as.numeric(weight),
         stars = str_remove(stars,"Stars"),
         stars = as.numeric(stars),
         nat_rank = as.numeric(nat_rank),
         on3_composite_rating = as.numeric(on3_composite_rating)
  ) |> 
  select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_15)

#14
on3_14_offers <- map(links1,on3_offer_scraper,year = 2014, .progress = TRUE) |> 
  data.table::rbindlist()
on3_14_offers_2 <- map(links2,on3_offer_scraper,year = 2014, .progress = TRUE) |> 
  data.table::rbindlist()
on3_14_offers_3 <- map(links3,on3_offer_scraper,year = 2014, .progress = TRUE) |> 
  data.table::rbindlist()
on3_offers_final_14 <- rbind(on3_14_offers,on3_14_offers_2,on3_14_offers_3) |> 
  
  mutate(name = str_remove(name,"Claim Profile"),
         pos_rank = str_remove(pos_rank,"NATL[0-9]+"),
         state_rank = pos_rank,
         pos_rank = str_extract(pos_rank,"[0-9]+"),
         state_rank = str_remove(state_rank,"POS[0-9]+"),
         state_rank = str_extract(pos_rank,"[0-9]+"),
         city = str_extract(city_state,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
         state = str_remove(city,'.*,\\s*'),
         city = str_remove(city, ",.*"),
         height_ft = str_extract(height,"[0-9]"),
         height_in = sapply(str_extract_all(height,"[0-9]+"),function(x) x[2]),
         height = (12 * as.numeric(height_ft) + as.numeric(height_in)),
         year = year,
         offer = offer,
         year = as.numeric(year),
         weight = as.numeric(weight),
         stars = str_remove(stars,"Stars"),
         stars = as.numeric(stars),
         nat_rank = as.numeric(nat_rank),
         on3_composite_rating = as.numeric(on3_composite_rating)
  ) |> 
  select(name,year,height,weight,pos,school,city,state,on3_link,
         on3_composite_rating,stars,nat_rank,pos_rank,state_rank,offer)
view(on3_offers_final_14)


#csv save offers
vroom::vroom_write(on3_offers_final_26,"on3_offers_2026.csv",delim = ",")
vroom::vroom_write(on3_offers_final_25,"on3_offers_2025.csv",delim = ",")
vroom::vroom_write(on3_offers_final_24,"on3_offers_2024.csv",delim = ",")
vroom::vroom_write(on3_offers_final_23,"on3_offers_2023.csv",delim = ",")
vroom::vroom_write(on3_offers_final_22,"on3_offers_2022.csv",delim = ",")
vroom::vroom_write(on3_offers_final_21,"on3_offers_2021.csv",delim = ",")
vroom::vroom_write(on3_offers_final_20,"on3_offers_2020.csv",delim = ",")
vroom::vroom_write(on3_offers_final_19,"on3_offers_2019.csv",delim = ",")
vroom::vroom_write(on3_offers_final_18,"on3_offers_2018.csv",delim = ",")
vroom::vroom_write(on3_offers_final_17,"on3_offers_2017.csv",delim = ",")
vroom::vroom_write(on3_offers_final_16,"on3_offers_2016.csv",delim = ",")
vroom::vroom_write(on3_offers_final_15,"on3_offers_2015.csv",delim = ",")
vroom::vroom_write(on3_offers_final_14,"on3_offers_2014.csv",delim = ",")
