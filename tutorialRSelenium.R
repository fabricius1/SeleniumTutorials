# Import libraries
library("RSelenium")
library("tidyverse")
library("getPass")

# This line makes R show messages in English.
# Use "pt" if you want them to be displayed in Portuguese
Sys.setenv("LANG" = "en")

# Customized function to check class() and typeof() at once:
check_object <- function(object) {
    cat("\nclass: ",
        class(object),
        "\ntypeof: ",
        typeof(object),
        "\n")
}

# Create rsClientServer object with Google Chrome browser.
# Change the chromever argument for the Chrome version used in your machine
client_server <- RSelenium::rsDriver(browser=c("chrome"), 
                                     chromever="91.0.4472.101", 
                                     port=4545L, 
                                     verbose=F)
check_object(client_server)

# Save the remoteDriver object in the computational variable called "driver"
driver <- client_server[["client"]]
check_object(driver)

# Navigate to the page to be scraped
url <- "https://quotes.toscrape.com/"
driver$navigate(url)

# Get the current url
driver$getCurrentUrl()

# Maximize window
driver$maxWindowSize()

# This line is very important: comment it out and run it if your Selenium 
# remoteDriver crashes and you can't open another one in the same port


# system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

driver$findElement

# Get the login link element by using xpath
login_xpath <- "/html/body/div/div[1]/div[2]/p/a"
login_link <- driver$findElement(using = "xpath", value = login_xpath)
check_object(login_link)

# This method returns the element tag name
login_link$getElementTagName()

# This method returns the element inner text
login_link$getElementText()

# This method returns an element attribute (in this case, "href")
login_link$getElementAttribute("href")

# Click on the element
login_link$clickElement()

# Go back to the last page and wait 2.5 seconds
driver$goBack()
Sys.sleep(2.5)
cat("ok")

# Go forward in the browser history and wait 2 seconds
driver$goForward()
Sys.sleep(2)
cat("ok")

# This code uses a css selector to get the link to the main page
css_selector <- "div.header-box.row > div.col-md-8 > h1 > a"
main_page_link <- driver$findElement("css selector", css_selector)
check_object(main_page_link)

# Get the element info
main_page_link$getElementTagName()
main_page_link$getElementText()
main_page_link$getElementAttribute("style")

# Click on the link
main_page_link$clickElement()

# This function makes the browser return to the main page
return_to_main_page <- function() {
    css_selector <- "div.header-box.row > div.col-md-8 > h1 > a"
    driver$findElement("css selector", css_selector)$clickElement()
}

# The method findElements() returns a list of Web Elements.
# This code finds all links in page 1 by using the tag name
return_to_main_page()

all_links_page_1 <- driver$findElements("tag name", "a")

cat(paste("number of links in main page: ",
          length(all_links_page_1),
          "\n\n")
   )

cat("object", quote(all_links_page_1), "\n")
check_object(all_links_page_1)

cat("\n\nseventh object in", quote(all_links_page_1), "list:\n")
check_object(all_links_page_1[[7]])

# The function below, when used in a webElement with the "a" tag name,
# returns the link inner text and its url.
# Both info are also printed on the console if print_output = TRUE
show_links_info <- function(link_element, print_output = TRUE) {
    text <- as.character(link_element$getElementText())
    url <- as.character(link_element$getElementAttribute("href"))
    if (print_output) {
        cat(paste0(text, ": "),
            url,
            "\n")
        # line below makes the info be displayed during iteration
        flush.console()
    }
        
    c(text, url)
}

# Use lapply to apply the show_links_info function to the list with
# all the links in the first page
saved_list <- lapply(all_links_page_1, show_links_info)

# The code below saves all links from page 1 in a dataframe

links_dataframe <- data.frame()

for (i in 1:length(all_links_page_1)) {
    links_info <- show_links_info(all_links_page_1[[i]], print = FALSE)
    temp_data_frame <- data.frame(index = nrow(links_dataframe) + 1,
                                  text = stringr::str_trim(links_info[1]),
                                  url = links_info[2])
    links_dataframe <- rbind(links_dataframe, temp_data_frame)
}

head(links_dataframe, 3)

# This code clicks on the "next" link and moves along the pages.
# When the last page is reached and there is no next link, break the loop.
return_to_main_page()
i <- 1

repeat {
    cat("\npage ", i)
    flush.console()
    i <- i + 1
    
    next_link_as_list <- driver$findElements(using="css selector", "li.next > a")

    if (length(next_link_as_list) == 0) {
        break
    }
    
    next_link_as_list[[1]]$clickElement()
    Sys.sleep(2)
    
    
}

cat("\nwe reached the last page")
return_to_main_page()
cat("\nback to page 1\n")

# From the code above, we discovered that there are 10 pages in the website.
# The function below goes to a chosen page and it will raise an error
# if the argument passed to it is not an integer number between 1 and 10
go_to_page <- function(page_number=1) {
    if (is.character(page_number)) {
        cat("Error: you passed a string as argument.")
    }
    
    if (!is.numeric(page_number) ||
        as.integer(page_number) < 1 ||
        as.integer(page_number) > 10 ) {
        stop("Provide an integer number between 1 and 10 as argument")
    }
    
    if (page_number %% 1 != 0) {
        page_number <- trunc(page_number)
        cat(quote(page_number), "truncated to", page_number)
    }
    
    if (page_number != 1) {
        base_url_page <- "https://quotes.toscrape.com/page/"
        driver$navigate(paste(base_url_page, as.character(page_number), sep=""))    
    } else {
        return_to_main_page()
    }
}


# Go to page 3, wait 2.5 seconds and then go to page 7

go_to_page(3)
driver$getCurrentUrl()

Sys.sleep(2.5)

go_to_page(7)
driver$getCurrentUrl()

# This code truncates 3.141592653589793 to 3 and then go to page 3

go_to_page(pi)
Sys.sleep(2.5)
driver$getCurrentUrl()

# The following function calls will return errors

go_to_page("page number 3")
go_to_page("3")
go_to_page("3.1415")
go_to_page(25)
go_to_page(97.75)

# The code below will extract the page number from page 10
go_to_page(10)

my_url <- as.character(driver$getCurrentUrl())
cat(my_url, "\n")

split <- base::strsplit(my_url, "/")
cat(unlist(split)[length(unlist(split))])

return_to_main_page()

# This code reproduces all the first ten quotes as a long string.
# It shows how you can use the getElementText() method to access the
# inner text from all children elements at once.
string <- ""
quotes_divs_list <- driver$findElements("css selector", "div.quote")
string <- unlist(lapply(quotes_divs_list, function(element) {
    string <- paste(string, 
                    as.character(element$getElementText()),
                    "\n\n",
                    sep="")
    return(string)
}))

cat(string)

# Customized functions to be used in the next sapply() and lapply() calls in this code:
get_element_text <- function(element) {
    as.character(element$getElementText())
}

get_link_href <- function(element) {
    as.character(element$getElementAttribute("href"))
}

click_element <- function(element) {
    element$clickElement()
}

# With this code one can get all the info for the quotes in the current page
return_to_main_page()
quotes_text <- sapply(driver$findElements("css selector", "span.text"),
                      get_element_text)
authors_names <- sapply(driver$findElements("css selector", "small.author"),
                       get_element_text)
authors_links <- sapply(driver$findElements("partial link text", "(about)"),
                        get_link_href)
tags_text <- sapply(driver$findElements("css selector", "div.tags"),
                    get_element_text)


# Now, we only need to loop over all pages and save the quotes info into a dataframe
return_to_main_page()
all_quotes <- data.frame()
i <- 0

while (TRUE) {
    quotes_texts <- sapply(driver$findElements("css selector", "span.text"),
                           get_element_text)
    authors_texts <- sapply(driver$findElements("css selector", "small.author"),
                            get_element_text)
    authors_links <- sapply(driver$findElements("partial link text", "(about)"),
                            get_link_href)
    tags_texts <- sapply(driver$findElements("css selector", "div.tags"),
                         get_element_text)
    page_quotes <- data.frame("sequence" = (i * 10 + 1):(i * 10 +10),
                              "quote" = quotes_texts,
                              "author" = authors_texts,
                              "author_biography_link" = authors_links,
                              "tags" = tags_texts)
    
    all_quotes <- rbind(all_quotes, page_quotes)
    i <- i + 1
    cat("\npage", i)
    flush.console()
    
    try(next_link_as_list <- driver$findElements(using="css selector", "li.next > a"),
        silent = TRUE)
    
    if (length(next_link_as_list) == 0) {
        break
    }
    
    next_link_as_list[[1]]$clickElement()
    Sys.sleep(2)
    
}

# save the all_quotes dataframe as a CSV file
write.csv(all_quotes,
          file = "quotes.csv",
          quote = FALSE,
          row.names = FALSE,
          fileEncoding = "UTF-8")

cat(paste0("all_quote data frame dimensions: ", nrow(all_quotes), ", ", ncol(all_quotes)))

head(all_quotes, 3)

# Each author has a biography page. We can get that info too.
# This will get all the unique authors biography links
unique_links_biographies <- unique(all_quotes$author_biography_link)
cat("total of biographical pages:", length(unique_links_biographies))

# Now we can loop over these biography links and save their info in a dataframe
authors_info <- data.frame()

for (link in unique_links_biographies) {
    driver$navigate(link)
    Sys.sleep(3)
    index <- nrow(authors_info) + 1
    name <- driver$findElement("css selector", "h3.author-title")$getElementText()
    born_date <- driver$findElement("css selector", "span.author-born-date")$getElementText()
    born_location <- driver$findElement("css selector", "span.author-born-location")$getElementText()
    description <- driver$findElement("css selector", "div.author-description")$getElementText()
    
    cat("\n", index, "of", length(unique_links_biographies))
    flush.console()
    
    new_row <- data.frame("index" = index,
                          "name" = as.character(name),
                          "born_date" = as.character(born_date),
                          "born_location" = as.character(born_location),
                          "description" = as.character(description))
    
    authors_info <- rbind(authors_info, new_row) 
}

head(authors_info)

# Now we go back to the login page and play a little with the text input boxes.

# Go to the login page
login_xpath <- "/html/body/div/div[1]/div[2]/p/a"
driver$findElement(using = "xpath", value = login_xpath)$clickElement()

# Save the text input boxes elements in variables, using their id (two forms):
username_input <- driver$findElement("id", "username") 
password_input <- driver$findElement("css selector", "#password")

# Username and password values
# (one can also use base::readline() or getPass::getPass() 
# to ask for user input)
username <- "Fabrício"
password <- "1234 is not a secure password!"

# When dealing with input boxes, it is good practice to clear their value first
username_input$clearElement()

# Send the username information (it needs to be passed as a list)
username_input$sendKeysToElement(list(username))

# Do the same with password
password_input$clearElement()
password_input$sendKeysToElement(list(password))

# Once you don't need the username and password variables anymore, delete them
rm(username, password)

# Find the submit button
submit_button <- driver$findElement("css selector", "input.btn.btn-primary")

# Use the submitElement() method to submit the form information
submit_button$submitElement()

# let's go to another page and learn more about how to manipulate forms
url <- "http://httpbin.org/forms/post"
driver$navigate(url)
Sys.sleep(3)

############# CHALLENGE ################
# Ask a pizza using the form.
# Use RSelenium to do that.
# Provide fictional data and order the following kind of pizza:
# * large size
# * bacon, extra cheese and mushroom toppings
# * 19h45 delivery time
# * give some delivery instructions
# 
# Then hit the submit button.
# You will see a JSON page with your orders info.
# 
# You can tell the browser to go back to the order page
# and play with it again.
#
# Can you make a code to automate ordering 
# 20 different pizzas?
# Try it yourself. You most certainly can!
########################################

# This code choose the large option in the radio buttons
radio_buttons <- driver$findElements("css selector", 'input[type="radio"]')
radio_buttons[[3]]$clickElement()

# Choose the first, second and fourth toppings to your pizza
checkboxes <- driver$findElements("css selector", 'input[name="topping"]')
lapply(checkboxes[-3], click_element)

# Set delivery time to 19h45
time <- driver$findElement("css selector", 'input[type="time"]')
time$sendKeysToElement(list("19:45"))

poetry <- "
Two households, both alike in dignity\n
(In fair Verona, where we lay our scene),\n
From ancient grudge break to new mutiny,\n
Where civil blood makes civil hands unclean.\n
From forth the fatal loins of these two foes\n
A pair of star-crossed lovers take their life;\n
Whose misadventured piteous overthrows\n
Doth with their death bury their parents’ strife.\n
The fearful passage of their death-marked love\n
And the continuance of their parents’ rage,\n
Which, but their children’s end, naught could remove,\n
Is now the two hours’ traffic of our stage;\n
The which, if you with patient ears attend,\n
What here shall miss, our toil shall strive to mend.\n"

# Send aditional information to the textarea input
textarea <- driver$findElement("tag name", "textarea")
textarea$sendKeysToElement(list(poetry))

# Submit form and go back
driver$findElement("css selector", "p > button")$submitElement()
Sys.sleep(5)
driver$goBack()

# Refresh the form page:
driver$refresh()

# When you are done working with selenium, 
# Quit the browser and end the session
driver$quit()
driver$closeServer()
rm(driver, client_server)

# This releases the port.
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)


