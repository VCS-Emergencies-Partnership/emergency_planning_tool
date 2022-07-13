# Script to use postcodeio to find lat/long of postcodes for Charity Commissioner data
library(readr)
library(dplyr)
library(PostcodesioR)
library(httr)

unique_postcodes_df <- read_rds("data/postcodes.rds") 

# Method 1 - bulk_postcode_lookup()
unique_postcode_vector <- unique_postcodes_df |>
  pull()

# Postcodeio function takes vectors of length 100 so split into
# list of vectors of length 100
unique_postcodes_list <- split(unique_postcode_vector, ceiling(seq_along(unique_postcode_vector)/99))

# Test
bulk_postcode_lookup(unique_postcodes_list[1])
# Face same issue as https://github.com/ropensci/PostcodesioR/issues/16 i.e. Bad Request (HTTP 400)

# Method 2 - Use postcode_lookup()
# TO DO: improve this code - couldn't get mutate() or map() to work with postcodelookup()
unique_postcodes_df <- unique_postcodes_df |>
  mutate(lat = NA,
         long = NA)

for (i in 1:nrow(unique_postcodes_df)) {
  result <- postcode_lookup(unique_postcodes_df[[i, "charity_contact_postcode_join"]])
  unique_postcodes_df[i, "lat"] <- result$latitude
  unique_postcodes_df[i, "long"] <- result$longitude
}

# Save 
unique_postcodes_df |>
  write_rds("data/postcodes_lookup.rds")
