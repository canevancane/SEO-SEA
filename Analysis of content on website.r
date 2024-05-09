# analysis of content on website

library(rvest)
library(httr)
library(stringi)
library(magick)
library(xml2)
library(httr)


# import data
websites <- read.csv("/Users/vincentbecker/VSCode/SEO Project/results.csv")

# delete column X from dataset
websites <- websites[, -1]

View(websites)
################## FUNCTION TO GET PIXEL AND CHARACTERS #######################
dt <- data.frame()


numChar <- c()
RE <- c()

numCandRE <- function(url) {
  error_count <- 0  # Initialize error count for this URL
  
  tryCatch({
    # Record start time
    start_time <- Sys.time()
    
    # Load body of page
    page <- read_html(url)
    pagebody <- page %>% html_element("body")
    
    # Calculate loading time
    end_time <- Sys.time()
    loading_time <- end_time - start_time
    
    # Define all html text elements
    html_text_elements <- c(
      "p", 
      "h1", "h2", "h3", "h4", "h5", "h6",
      "span",
      "div",
      "a",
      "strong",
      "em",
      "blockquote",
      "pre",
      "code",
      "abbr",
      "cite",
      "q",
      "sub",
      "sup",
      "del",
      "ins",
      "b",
      "i"
    )
    
    # Filter for only html elements
    
    text_strings <- character()  # Initialize text_strings as an empty character vector
    
    for (element in html_text_elements) {
      var <- page %>% html_elements(element)
      var <- var %>% html_text()
      text_strings <- c(text_strings, var)  # Assign directly to text_strings
    }
    
    # Delete code snippets
    
    # text_strings <- grep("{.*}", text_strings, value = TRUE, invert = TRUE)
    text_strings <- gsub("(.*)<.*>(.*)", "\\1\\2", text_strings)
    text_strings <- grep("function", text_strings, value = TRUE, invert = TRUE)
    
    text_strings <- unlist(text_strings)
    # Count number of characters for whole website
    nc <- sum(sapply(text_strings, nchar))
    
    # Save number of characters to list
    numChar <- nc
    print(nc)
    
    
    # Real estate of image
    # Filter image rows of code
    images <- page %>% html_elements("img")
    
    # Filter for image links
    images_url <- images %>% html_attr("src")
    
    # Getting raw url of page
    url_raw <- gsub("(https*://\\w+.\\w+.\\w+)/.*", "\\1", url)
    
    # Getting space of image
    image_url_raw <- gsub("https*://\\w+.\\w+.\\w+(/.*)$", "\\1", images_url)
    
    #print(image_url_raw)
    
    # Function for getting size of all images
    
    RE <- numeric()  # Initialize RE as an empty numeric vector
    
    for (image in image_url_raw) {
      
      # Check if the URL length exceeds a certain threshold (e.g., 500 characters)
      if (nchar(image) > 500) {
        cat("Skipping image link with excessive length:", image, "\n")
        next  # Skip processing this image link
      }
      
      slashformat <- grep("//", image, value = TRUE)
      nonslashformat <- grep("//", image, value = TRUE, invert = TRUE)
      
      if (length(slashformat) > 0) {
        slashformat <- paste0("https:", slashformat)
        imagelink <- slashformat
      } else if (length(nonslashformat) > 0) {
        imagelink <- paste(url_raw, nonslashformat, sep = "")
      } 
      
      #print(imagelink)
      
      tryCatch({
        img <- image_read(imagelink)
        img_jpeg <- image_convert(img, "jpeg")
        info <- image_info(img_jpeg)
        width <- as.integer(info$width)
        height <- as.integer(info$height)
        re <- width * height
        
        RE <- c(RE, re)
        
        rm(img_jpeg)
      }, error = function(e) {
        cat("An error occurred:", conditionMessage(e), "\n")
        RE <- c(RE, 0)  # Append 0 to RE
        error_count <<- error_count + 1  # Increment error count
      })
    }
    sumRE <- sum(RE)
    row <- c(url, numChar, sumRE, error_count, loading_time)  # Include loading time in the row
    dt <<- rbind(dt, row)
  }, error = function(e) {
    cat("An error occurred while fetching the page:", conditionMessage(e), "\n")
    # Add 0s to both arrays and indicate 1 error for this URL
    row <- c(url, 0, 0, 1, NA)  # NA for loading time since page couldn't be fetched
    dt <<- rbind(dt, row)
  })
}

x <- 1

for (url in websites$results[4001:8000]) {
  print(x)
  x <- x + 1
  numCandRE(url)
}

# save dataframe as csv
write.csv(dt, file = "scraping_analysis_4001_to_8000.csv")

View(dt)

colnames(dt) <- c("URL", "nChar", "sumRE", "nError", "loadTime")


websites$rank

dt$Rank <- websites$rank
dt$Keyword <- websites$keyword

View(dt)

View(dt)

