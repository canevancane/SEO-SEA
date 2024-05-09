# Get Top 100 Startpage Reulsts

library(rvest)
library(httr)
library(stringi)
library(magick)
library(xml2)
library(httr)

httr::set_config(httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:124.0) Gecko/20100101 Firefox/124.0"))


################## FUNCTION TO SET USER AGENT #######################

user_agents <- c(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.1234.123 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.1234.123 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.85 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.150 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.101 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.104 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.159 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.121 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/93.0.4577.63 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/84.0.4147.135 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.61 Safari/537.36"
)


# Function to get a random User-Agent string
get_random_user_agent <- function() {
  sample(user_agents, 1)
}



################## FUNCTION TO GET TOP 50 GOOGLE (IS SAVED AS RESULTS) ######################

top10 <- function(word, page){
  url <- paste0("https://www.startpage.com/sp/search?t=device&lui=english&language=english&query=", word, "&cat=web&page=", page)
  
  get_random_user_agent()
  
  page <- read_html(url)
  
  link_raw <- page %>% html_elements("a.result-title.result-link")
  
  link <- link_raw %>% html_attr("href")
  
  return(link)
}

results <- c()
keyword <- c()
rank <- c()

dt <- data.frame(keyword, results, rank)

top100 <- function(word){
    results <<- c()
    keyword <<- c()
    rank <<- c()

        system("osascript -e 'quit app \"NordVPN\"'")
        print("VPN Disconnected")
        system('open -a "NordVPN"')
        system('open -a "NordVPN"')
        system('open -a "NordVPN"')
        print("VPN Connected")
        Sys.sleep(30)
        print("Sleep over")
        for(i in 1:5) {
            print(i)
            link <- top10(word, i)
            print(link)
            results <<- c(results, link)
            
            for(j in 1:10) {
                keyword <<- c(keyword, word)

                current_rank <- (i-1)*10 + j
                rank <<- c(rank, current_rank)

            }
        }
        system("osascript -e 'quit app \"NordVPN\"'")
        print("VPN Disconnected")
        system('open -a "NordVPN"')
        system('open -a "NordVPN"')
        system('open -a "NordVPN"')
        print("VPN Connected")
        Sys.sleep(30)
        print("Sleep over")
        for(i in 6:10) {
            print(i)
            link <- top10(word, i)
            print(link)
            results <<- c(results, link)

            for(j in 1:10) {
                keyword <<- c(keyword, word)

                current_rank <- (i-1)*10 + j
                rank <<- c(rank, current_rank)
            }
        }
    # add to data frame and keep old inputs
    dt <<- rbind(dt, data.frame(keyword, results, rank))
}

################## LOAD LIST OF SERVICE INDUSTRIES #######################

# read csv
service_industries <- read.csv("/Users/vincentbecker/VSCode/SEO Project/services.csv")

################## LOOP THROUGH SERVICE INDUSTRIES #######################
for(i in 1:80) {
  repeat {
    error_occurred <- FALSE
    tryCatch({
      print(i)
      word <- service_industries$x[i]
      top100(word)
    }, error = function(e) {
      print(paste("Error occurred:", e))
      error_occurred <- TRUE
    })
    if (!error_occurred) break
  }
}

# save dt to csv
write.csv(dt, "/Users/vincentbecker/VSCode/SEO Project/results.csv")

