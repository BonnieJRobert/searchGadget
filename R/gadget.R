#to do...
#         add second search for description based on title
#         dynamically update slider (max)
#         empty search (search = "blah") causes crash
require(shiny)
require(miniUI)
require(DT)
require(rvest)
require(dplyr)
require(janitor)

url_home <- 'https://catalogue.data.gov.bc.ca'
url_sub <- '/dataset?sort=metadata_modified+desc&q='

#function scrapes a url and returns names of datasets on each page
get_ds_titles <- function(u){
  
  #pull headings(titles) from url, u
  x <- read_html(u) %>%
    html_nodes('.dataset-heading a') %>%
    html_text()
  
  return(x)
}

#function takes a dataset title and creates a url
create_url_from_title <- function(ttl) {
  
  #handle NULL
  if (is.null(ttl)) 
    ttl <- "" 
  
  url <- janitor::make_clean_names(ttl)
  url <- gsub("_", "-", url)
  url <- paste("", url, sep = "")
  
  #handle case when title ends in a )
  if (grepl(")$", ttl)) {
    paste(url, "-", sep = "")
  }

  url <- paste("https://catalogue.data.gov.bc.ca/dataset/", url, sep = "")
  
  return(url)
}

#function performs error handling for search string
clean_search_string <- function(val) {

  #handle NULL
  if (is.null(val)) 
    val <- "" 
  
  #replace whitespace with '+'
  val <- gsub("\\s+", "+", val)
  
  return(val)
}

search_data_bc <- function() {
  
  ui <- miniPage(
    
    #include title banner and close buttons
    gadgetTitleBar("Enter search criteria", left = NULL),
    
    miniContentPanel(
        textInput("search_box_1", "Search Criteria", ""),  #input testing required. currently takes a single word
        actionButton("search_1", "Go"),
        sliderInput("slider_1", "Number of Pages", min = 1, max = 10, value = c(1,5)),
        headerPanel(""),
        headerPanel(""),
        textInput("search_box_2", "Lookup Dataset Title", ""),
        actionButton("search_2", "Go")
      )
    
  )
  
  server <- function(input, output, session) {
    
    #print available datasets in databc (search criteria)
    observeEvent(input$search_1, {
    
      search_string = clean_search_string(input$search_box_1)
      url <- paste(url_home, url_sub, search_string , sep = "")
      webpage <- read_html(url)
      
      #get page urls from first page
      pages <- html_nodes(webpage,'.pagination-centered a') %>% html_text()
      
      x = seq(min(as.numeric(pages), na.rm=TRUE),
              max(as.numeric(pages), na.rm=TRUE), 
              by = 1)
      
      page_urls <- paste(url, "&page=", x, sep = "")
      
      #get dataset titles/names
      
      #error handling if the search returns no values
      min_p <- input$slider_1[1]
      max_p <- input$slider_1[2] 
      
      l <- lapply(page_urls[1:max_p], get_ds_titles)
      
      print(unlist(l))
      
    })
    
    #print description of given dataset in databc (dataset title)
    observeEvent(input$search_2, {
     
      search_string = clean_search_string(input$search_box_2)
      url <- create_url_from_title(search_string)

      desc <- read_html(url) %>% 
        html_nodes(xpath = '//meta[@property="og:description"]') %>% 
        html_attr('content')
      
      print(desc)
      
    })
    
    #close app when done pressed
    observeEvent(input$done, {
      #stop the app
      stopApp()
    })
  }
  
  runGadget(ui, server)
}
