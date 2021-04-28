# Purpose
## To scrape gumtree for adverts of road bikes

library(tidyverse)
library(rvest)
library(lubridate)
library(glue)

# Start by getting how many pages there are

# url for where the pages of ads are listed for road bikes
url <- "https://www.gumtree.co.za/s-road-bikes/v1c9801p1"

tag <- "/v1c9801p"

# get the number of pages
get_last_page <- function(html){

    n_ads <- html %>%
        html_nodes(".displayResults") %>%
        html_text() %>%
        str_remove("Results 1 to 20 of") %>%
        parse_number()

    # total pages
    pages_data <- round(n_ads/20) + 1

    pages_data

}

first_page <- read_html(url)

latest_page_number <- get_last_page(first_page)
# recodes latest page number to 50 if larger
latest_page_number <- ifelse(latest_page_number <= 50, latest_page_number, 50)


# list of pages
list_of_pages <- str_c(url, "page-", 1:latest_page_number, tag, 1:latest_page_number) %>%
    # the section above creates the url with the page numbers in both places
    as_tibble() %>%
    transmute(link = value) %>%
    # collects the page number and stores it in the tibble
    mutate(page = as.integer(str_remove_all(link, paste0(".*", substring(tag,2)))))


# function that gets the urls for each ad from the different pages
get_ad_links <- function(link){
    html <- read_html(link)

    message(link)

    html %>%
        html_nodes(".related-ad-title") %>%
        html_attr("href")
}

# creates a list of links from each page
list_of_links <- list_of_pages %>%
    # the possibly here means it will store the error and continue should it hit a problem
    mutate(text = map(link, possibly(get_ad_links, otherwise = "failed", quiet = T)))


# unnests the list of links for a tibble that isn't compact
list_of_links <-list_of_links %>%
    unnest() %>%
    mutate(ad_url = text,
           ad_number = row_number()) %>%
    select(-text)

# adds in the rest of the url to the link
list_of_links_clean <- list_of_links %>%
    mutate(ad_url = str_c("https://www.gumtree.co.za", ad_url))


# save list of links
st <- format(Sys.time(), "%Y-%m-%d-%I-%M-%p")
write.csv(list_of_links_clean, file = paste("data/links/gumtree_links_road",st, ".csv", sep = ""))




get_ad_text <- function(ad_url){
    # store the html from the page
    html <- read_html(ad_url)

    message(glue("Getting ad from {ad_url}"))

    # title
    title <- html %>%
        html_node("h1") %>%
        html_text()

    # price
    price <- html %>%
        html_node(".ad-price") %>%
        html_text() %>%
        parse_number()

    # text
    text <- html %>%
        html_nodes("#revip-description .description-content") %>%
        html_text()

    # photos
    n_photos <- html %>%
        html_node(".count") %>%
        html_text() %>%
        parse_number()

    # views
    n_views <- html %>%
        html_node(".view-count span") %>%
        html_text() %>%
        parse_number()

    # date of ad
    ad_date <- as.character(now()- html %>%
                                html_node(".vip-stats .creation-date") %>%
                                html_text() %>%
                                str_to_lower() %>%
                                str_remove(" ago") %>%
                                str_replace_all("a |an ", "1 ") %>%
                                duration())

    # ad_date safe is included in case there is a problem with the syntax above and can be calculated from the scrape time function
    # ad_date_safe <- html %>%
    # html_node(".vip-stats .creation-date") %>%
    # html_text()

    # seller name
    seller_name <-  html %>%
        html_node(".seller-name") %>%
        html_text()

    # seller age
    seller_age <- html %>%
        html_node(".seller-year") %>%
        html_text() %>%
        parse_number()

    # all time ads
    n_all_time_ads <- html %>%
        html_node(".seller-active-ads+ .seller-active-ads span") %>%
        html_text() %>%
        parse_number()

    # active ads
    n_active_ads <- html %>%
        html_node(".seller-active-ads:nth-child(1) .ads-number-info span") %>%
        html_text() %>%
        parse_number()

    # location
    location <- str_c((html %>%  html_node(".attribute:nth-child(1)") %>% html_text() %>% str_remove("Location:")),
                      html %>% html_node(".breadcrumbs span:nth-child(2) span") %>% html_text(), sep = ", ")



    # scrape date and time
    scrape_time <- Sys.time() %>% as.character() # can add it str_replace_all('[[:punct:] ]+', "_")

    # varlist <- c(title, price, text, n_photos, n_views, ad_date, location, seller_age, n_all_time_ads, n_active_ads, scrape_time)

    # nest(tibble(title, price, text, n_photos, n_views, ad_date, location, seller_age, n_all_time_ads, n_active_ads, scrape_time),
    #      nested = c(title, price, text, n_photos, n_views, ad_date, location, seller_age, n_all_time_ads, n_active_ads, scrape_time))

    tibble(title, price, text, n_photos, n_views, ad_date, seller_name, location, seller_age, n_all_time_ads, n_active_ads, scrape_time)

}

ads_nested <- list_of_links_clean %>%
    mutate(text = map(ad_url, possibly(get_ad_text, "failed")))

ads <- ads_nested %>%
    unnest()



provinces <- c("Western Cape", "KwaZulu-Natal", "Gauteng", "Eastern Cape", "Free State", "North West", "Mpumalanga", "Limpopo", "Northern Cape")

ads$province <- str_extract_all(ads$location, paste(provinces, collapse = "|")) %>%
    sapply(., paste, collapse = ", ")

# Save output

st <- format(Sys.time(), "%Y-%m-%d-%I-%M-%p")
write_rds(ads, paste0("data/ads/gumtree_ads_road_", st, ".rds"))
