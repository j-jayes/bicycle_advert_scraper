# Purpose
## To scrape bikehub for adverts

library(tidyverse)
library(rvest)
library(lubridate)
library(glue)

# Start by getting how many pages there are

url <- "https://bikehub.co.za/classifieds/g/bikes"

get_last_page <- function(html){

    first_page %>%
        html_nodes(".text-md-left") %>%
        html_text() %>%
        str_remove("Page 1 of") %>%
        parse_number()
}

first_page <- read_html(url)

latest_page_number <- get_last_page(first_page)

# Create list of pages to get links from

list_of_pages <- str_c(url, "?page=", 1:latest_page_number) %>%
    # the section above creates the url with the page numbers in both places
    as_tibble() %>%
    transmute(link = value) %>%
    mutate(page = parse_number(str_remove_all(link, "https://bikehub.co.za/classifieds/g/bikes")))

# Collecting the ad links from the list of pages

## function that gets the urls for each ad from the different pages
get_ad_links <- function(link){
    html <- read_html(link)

    message(glue("getting ads from page {link}"))

    html %>%
        html_nodes("marketplace-item-single-list") %>%
        html_attr(":marketplace-item-url")
}

# creates a list of links from each page
list_of_links <- list_of_pages %>%
    # the possibly here means it will store the error and continue should it hit a problem
    mutate(text = map(link, possibly(get_ad_links, otherwise = "failed", quiet = T)))


# unnests the list of links for a tibble that isn't compact
list_of_links_clean <- list_of_links %>%
    unnest() %>%
    mutate(ad_url = str_remove_all(text, "\\'"),
           ad_number = row_number()) %>%
    select(-text)

# adds in the rest of the url to the link
# list_of_links_clean <- list_of_links %>%
#   mutate(ad_url = str_c("https://www.gumtree.co.za", ad_url))


# save list of links
st <- format(Sys.time(), "%Y-%m-%d-%I-%M-%p")
write.csv(list_of_links_clean, file = paste("data/links/bikehub_links_",st, ".csv", sep = ""))




get_ad_text <- function(ad_url){
    # store the html from the page
    html <- read_html(ad_url)

    message(glue("Getting ad from {ad_url}"))

    # title
    title <- html %>%
        html_node("title") %>%
        html_text() %>%
        str_remove(" \\| Bike Hub")

    # price
    price <- html %>%
        html_node(".text-secondary") %>%
        html_text() %>%
        parse_number()

    # text
    text <- html %>%
        html_nodes("#ad-content-left .pb-2 p") %>%
        html_text() %>%
        str_squish()

    info_box <- html %>%
        html_nodes(".full-spec-list") %>%
        html_text() %>%
        str_squish()

    # photos
    # this counts the number of image links and divides it by two because each image contains "Image" twice.
    n_photos <- as.integer(html %>%
                               html_node("global-image-slider") %>%
                               html_attrs() %>%
                               as.character() %>%
                               str_count("Image") / 2)

    # date of ad
    ad_date <- as.character(
        now()- html %>%
            html_node(".text-right") %>%
            html_text() %>%
            str_to_lower() %>%
            str_squish() %>%
            str_remove(" ago") %>%
            str_replace_all("a|an", "1") %>%
            duration())

    # seller name
    seller_name <-  html %>%
        html_node("#ad-content-right") %>%
        html_text() %>%
        str_remove_all("\\n") %>%
        str_squish() %>%
        str_remove("\\$\\(docu.*") %>%
        str_remove("Contact.*")

    # seller age
    seller_details <- read_html(
        html %>%
            html_node("user-marketplace-details") %>%
            html_attr(":get-user-details-url") %>%
            as.character() %>%
            str_remove_all("\'")) %>%
        html_node("body") %>%
        html_text()

    # location
    location <- html %>%
        html_node("#ad-content-right") %>%
        html_text() %>%
        str_squish() %>%
        str_remove("\\$\\(docu.*") %>%
        str_remove(".*Location ")

    # scrape date and time
    scrape_time <- Sys.time() %>% as.character() # can add it str_replace_all('[[:punct:] ]+', "_")

    # varlist <- c(title, price, text, n_photos, n_views, ad_date, location, seller_age, n_all_time_ads, n_active_ads, scrape_time)

    # nest(tibble(title, price, text, n_photos, n_views, ad_date, location, seller_age, n_all_time_ads, n_active_ads, scrape_time),
    #      nested = c(title, price, text, n_photos, n_views, ad_date, location, seller_age, n_all_time_ads, n_active_ads, scrape_time))

    tibble(title, price, text, info_box, n_photos, ad_date, seller_name, location, seller_details, scrape_time)

}

# mapping through each url
ads_nested <- list_of_links_clean %>%
    mutate(text = map(ad_url, possibly(get_ad_text, "failed")))

ads <- ads_nested %>%
    unnest()


# joining text and info box
ads <- ads %>%
    mutate(text = str_c(text, info_box, sep = "Info box: ")) %>%
    select(-info_box)

# seller age
ads <- ads %>%
    mutate(seller_age = str_remove_all(seller_details, ".*member_since"),
           seller_age = str_remove_all(seller_age, "total_ads.*"),
           seller_age = str_remove_all(seller_age, "[[:punct:]]"),
           # this makes the seller date the first of the month that is provided
           seller_age = as.character(dmy(glue("01 {seller_age}"))))

# n_all_time_ads
ads <- ads %>%
    mutate(n_all_time_ads = str_remove_all(seller_details, ".*total_ads"),
           n_all_time_ads = str_remove_all(n_all_time_ads, "active_ads.*"),
           n_all_time_ads = parse_number(str_remove_all(n_all_time_ads, "[[:punct:]]")))

# n_active_ads
ads <- ads %>%
    mutate(n_active_ads = str_remove_all(seller_details, ".*active_ads"),
           n_active_ads = str_remove_all(n_active_ads, "user_profile_url.*"),
           n_active_ads = parse_number(str_remove_all(n_active_ads, "[[:punct:]]")))

ads <- ads %>%
    mutate(n_views = "NA for bikehub") %>%
    relocate(n_views, .before = ad_date) %>%
    relocate(scrape_time, .after = n_active_ads) %>%
    select(-seller_details)


provinces <- c("Western Cape", "KwaZulu-Natal", "Gauteng", "Eastern Cape", "Free State", "North West", "Mpumalanga", "Limpopo", "Northern Cape")


ads$province <- str_extract_all(ads$location, paste(provinces, collapse = "|")) %>%
    sapply(., paste, collapse = ", ")


ads <- ads %>%
    mutate(type = str_remove_all(ad_url, "https://bikehub.co.za/classifieds/item/"),
           type = str_remove_all(type, "\\/[0-9].*"),
           type = str_replace_all(type, "-", " "))

# Save output

st <- format(Sys.time(), "%Y-%m-%d-%I-%M-%p")
write_rds(ads, paste0("data/ads/bike_hub_", st, ".rds"))








