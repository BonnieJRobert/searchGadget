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
url_dat <- '/dataset?q='
url_end <- '&sort=score+desc%2C+record_publish_date+desc'

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

  #handle case when title ends in a )
  if (grepl(")$", ttl)) {
    url <- paste(url, "-", sep = "")
  }

  url <- paste(url_home, url_dat, url, url_end, sep = "")

  return(url)
}

#function takes a dataset title and creates a url
create_dip_url_from_title <- function(ttl) {

  #handle NULL
  if (is.null(ttl))
    ttl <- ""

  url <- gsub("\\+", "-", ttl)
  url <- tolower(url)

  #handle case when title ends in a )
  if (grepl(")$", ttl)) {
    url <- paste(url, "-", sep = "")
  }

  url <- paste(url_home, "/dataset/", url, sep = "")

  return(url)
}

#function performs error handling for search string
clean_search_string <- function(val) {

  #handle NULL
  if (is.null(val))
    val <- ""

  #replace whitespace with '+'
  val <- gsub("\\s+", "+", val)

  #could change case here

  return(val)
}

search_data_bc <- function() {

  ui <- miniPage(

    #include title banner and close buttons
    gadgetTitleBar("Enter search criteria", left = NULL),

    miniTabstripPanel(
      miniTabPanel("DataBC Data", icon = icon("table"),
        miniContentPanel(
          textInput("search_box_1", "Search Criteria", ""),  #input testing required. currently takes a single word
          sliderInput("slider_1", "Number of Pages", min = 1, max = 10, value = c(1,2)),
          actionButton("search_1", "Go"),
          headerPanel(""),
          headerPanel(""),
          textInput("search_box_2", "Lookup Dataset Title", ""),
          actionButton("search_2", "Go")
        )
      ),
      miniTabPanel("DIP Data", icon = icon("table"),
        miniContentPanel(
          actionButton("search_dip_1", "Show DIP Datasets"),
          headerPanel(""),
          headerPanel(""),
          textInput("search_box_dip", "Lookup DIP Metadata", ""),
          actionButton("search_dip_2", "Go")
        )
      )
    )
  )

  server <- function(input, output, session) {

    #print available datasets in databc (search criteria)
    observeEvent(input$search_1, {

      search_string = clean_search_string(input$search_box_1)

      url <- paste(url_home, url_dat, search_string, url_end, sep = "")
      webpage <- read_html(url)

      #get page urls from first page
      pages <- html_nodes(webpage,'.pagination-centered a') %>% html_text()
      pages <- suppressWarnings(as.numeric(pages))
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


    #print description of given dataset in databc (dataset title)
    observeEvent(input$search_dip_1, {

      url <- 'https://catalogue.data.gov.bc.ca/group/data-innovation-program'

      webpage <- read_html(url)

      #get page urls from first page
      pages <- html_nodes(webpage,'.pagination-centered a') %>% html_text()
      pages <- suppressWarnings(as.numeric(pages))
      x = seq(min(as.numeric(pages), na.rm=TRUE),
              max(as.numeric(pages), na.rm=TRUE),
              by = 1)

      page_urls <- paste(url, "?page=", x, sep = "")
      l <- lapply(page_urls, get_ds_titles)

      print(unlist(l))

    })


    #print description of given dataset in databc (dataset title)
    observeEvent(input$search_dip_2, {

      search_string = clean_search_string(input$search_box_dip)
      url <- create_dip_url_from_title(search_string)

      desc <- read_html(url) %>%
        html_nodes(xpath = '//meta[@property="og:description"]') %>%
        html_attr('content')

      print(desc)

      ds <- read_html(url) %>%
        html_elements(".resource-url-analytics") %>%
        html_text(trim = TRUE)

      ds <- ds[which(ds != 'Download')]
      ds <- gsub("csv", "\\.csv", ds)

      print(ds)
    })

    #close app when done pressed
    observeEvent(input$done, {
      #stop the app
      stopApp()
    })
  }

  runGadget(ui, server)
}
