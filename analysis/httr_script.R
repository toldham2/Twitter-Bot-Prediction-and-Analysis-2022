require(httr)
require(jsonlite)
require(dplyr)

bearer_token <- "AAAAAAAAAAAAAAAAAAAAAGyPYwEAAAAA8yXgiooeII6vicurbcvbRhdlW4I%3DBX7RgM5BZ3DwQ9qUrwkhdQLkIm3io79NiGVFVD8o7Ksl81zcq7"
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

params <- list(`user.fields` = c('created_at', 'public_metrics'))

id <- '23022687'
url_handle <-
  sprintf('https://api.twitter.com/2/users/%s/followers?user.fields=created_at,public_metrics', id)

response <-
  httr::GET(url = url_handle,
            httr::add_headers(.headers = headers))
obj <- httr::content(response, as = "text")

json_data <- fromJSON(obj, flatten = TRUE) %>% as.data.frame
View(json_data)

tryCatch({
  next_token <- json_data$meta.next_token %>%
           tail(1)
}, error=function(e) {
  next_token <- ""
})


while(!next_token == "") {
  
}
