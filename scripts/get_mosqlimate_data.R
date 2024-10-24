library(httr)
library(jsonlite)

infodengue_api <- "https://api.mosqlimate.org/api/datastore/infodengue/"
arbovirose <- c("dengue", "chik")
start_date <- "2024-01-01"
end_date <- ymd(Sys.Date())

df_final <- NULL
for(i in 1:length(arbovirose)){
  
  filters <- paste0("disease=", arbovirose[i], "&start=", start_date, "&end=", end_date)
  page <- 1
  paginas <- page
  
  while(page <= paginas){
    
    print(paste0(arbovirose[i], ": ", page, " de ", paginas, " (",round(page/paginas*100, 2) ,"%)"))
    pagination <- paste0("?page=", page, "&per_page=100&")
    
    url <- paste0(infodengue_api, pagination, filters)
    resp <- GET(url)
    content <- content(resp, "text")
    json_content <- fromJSON(content)
    df_temp <- json_content$items %>% 
      mutate(
        CID = ifelse(arbovirose[i] == "dengue", "A90", "A92.0")
      )
    df_final <- rbind(df_final, df_temp)
    if(page == 1){
      paginas <- json_content$pagination$total_pages
    }
    page = page + 1
  }
}
