library(dplyr)
library(purrr)
library(ggplot2)


bird_counts <- list(c(3, 1), c(3, 8, 1, 2), c(8, 3, 9, 9, 5, 5), 
                    c(8, 9, 7, 9, 5, 4, 1, 5))
bird_counts

# Create bird_sum list, loop over and sum elements of bird_counts
# with a for loop
bird_sum <- list()

for (i in seq_along(bird_counts)) {
  bird_sum[[i]] <- sum(bird_counts[[i]])
}

# do the same thing with purrr: sum each element of bird_counts, 
# and put in bird_sum
bird_sum <- map(bird_counts, sum)

# Subsetting lists
lo <- list()

lo[["data"]] <- data.frame(bird = c("robin", "sparrow", "jay"),                           
                           weight = c(76, 14, 100),                           
                           wing_length = c(100, 35, 130))

lo[["model"]] <- lm(weight ~ wing_length,            
                    data = lo[["data"]])

lo[["plot"]] <- ggplot(data = lo[["model"]],             
                      aes(x = weight, y = wing_length)) +                  
                      geom_point()

# library "repurrrsive" contains wesanderson dataset, 
# containing color palettes from wes anderson movies
install.packages("repurrrsive")
library(repurrrsive)
# Load data
data(wesanderson)

# Get the third element of the first wesanderson vector
wesanderson[[1]][3]
# Get fourth element of the 'GrandBudapest' wesanderson vector
wesanderson$GrandBudapest[4]

data(sw_films)

# Subset the first element of the sw_films data
sw_films[[1]]

# Subset the first element of the sw_films data, get title 
sw_films[[1]]$title

# Determine the length of each element
map(wesanderson, length)
# Do the same thing in more explicit format map(list, ~function(.x))
map(wesanderson, ~length(.x))

# Use map_*() functions to tell purrr what type of output you want
# Create a numcolors column and fill with length of each wesanderson element
data.frame(numcolors = map_dbl(wesanderson, ~length(.x)))

# Load sw_films
data(sw_films)
# Set names
sw_films_named <- sw_films %>%
  set_names(map_chr(sw_films, "title"))
# view names
names(sw_films_named)

# Use pipes inside map() to iterate over multiple functions
# Create a list of values from 1 through 10
numlist <- list(1:10)

# Iterate over the numlist 
map(numlist, ~.x %>% sqrt() %>% sin())

# Simulate data - values to use as mean for simulated dataset
list_of_means <- list(5, 2, 300, 15)
list_of_means

list_of_df <- map(list_of_means,
                  ~data.frame(a = rnorm(mean = .x,
                                        n = 200,
                                        sd = (5/2))))
head(list_of_df[[1]])

# Simulate data for 2 populations, a & b, from the sites north, east, 
# and west. Populations a & b will be randomly drawn 
# from a normal distribution, with different means and sd's.

# List of sites north, east, and west
sites <- list("north", "east", "west")

# Create a list of dataframes, each with sites, a, and b columns 
list_of_df <-  map(sites,  
                   ~data.frame(sites = .x,
                               a = rnorm(mean = 5, n = 200, sd = (5/2)),
                               b = rnorm(mean = 200, n = 200, sd = 15)))

# Map over list_of_df with a linear model, to compare b as the predictor
# and a as the response. Then pipe the lm output into map and generate
# the summary of each model (each site).
list_of_df %>%
  map(~ lm(a ~ b, data = .)) %>%
  map(., summary)

# Use safely() to debug a list and see where errors are occurring
a <- list(-10, 1, 10, 0) %>% 
  map(safely(log, otherwise = NA_real_)) %>%
  # Transpose the result
  transpose()

# Print the list
a

# Print the result element in the list
a[["result"]]

# Print the error element in the list
a[["error"]]

# Load sw_people data
data(sw_people)

# Map over sw_people and pull out the height element
height_cm <- map(sw_people, "height") %>%
  map(function(x){
    ifelse(x == "unknown",NA, # Change unknowns to NA
           as.numeric(x))     # Change data type of known heights to numeric
  })

# Map over sw_people and pull out the height element
height_ft <- map(sw_people , "height") %>% 
  map(safely(function(x){  # map over safely() to convert heights from cm to ft
    x * 0.0328084
  }, quiet = FALSE)) %>% # set quiet to FALSE so errors are printed
  transpose()

# Print your list, the result element, and the error element
height_ft
height_ft[["result"]]
height_ft[["error"]]

# Use possibly() to resolve errors
a <- list(-10, 1, 10, 0) %>%
  map(possibly(function(x){
    log(x)
  }, otherwise = NA_real_))

# For the sake of this exercise, height_cm was converted to numeric
# but the value "unknown" was retained.
height_cm <- map(sw_people, "height") %>%
  map(function(x){
    ifelse(x == "unknown", "unknown", 
           as.numeric(x))
  })
# Convert cm to feet, using possibly to convert missing values to NA
height_cm %>%
  map_dbl(possibly(function(x){
    x * 0.0328084
  }, otherwise = NA_real_))

# Use walk() for printing cleaner list outputs
short_list <- list(-10, 1, 10)
# Print results the regular way
short_list
# Compare print output of walk()
walk(short_list, print)

# Use walk to display multiple plots sequentially
data(gap_split)
# Map over the first 10 elements of gap_split
plots <- map2(gap_split[1:10], 
              names(gap_split[1:10]), 
              ~ ggplot(.x, aes(year, lifeExp)) + 
                geom_line() +
                labs(title = .y))
# Print all plots: object name, then function name
walk(plots, print)

# Checking and setting names
# Load the data
data(gh_users)

# Check if data has names
names(gh_users)

# Map over and extract name element of list
map(gh_users, ~.x[["name"]])

# Name gh_users with the names of the users
gh_users_named <- gh_users %>% 
  set_names(map_chr(gh_users, "name"))

# Check gh_repos structure
str(gh_repos)

# Name gh_repos with the names of the repo owner
gh_repos_named <- gh_repos %>% 
  map_chr(~ .[[1]]$owner$login) %>% 
  set_names(gh_repos, .)
# Check that setting names worked
names(gh_repos_named)

# Asking questions from lists
# Question:  Who joined github first?
# How:  Name gh_users with the "name" element and sort by "created_at"
map_chr(gh_users, ~.[["created_at"]]) %>%
  set_names(map_chr(gh_users, "name")) %>%
  sort()
# Question:  Are all repositories user-owned, rather than org-owned?
# How:  Output a vector that returns TRUE for "type" = "USER"
map_lgl(gh_users, ~.[["type"]] == "User") 
# Question:  Which user has the most public repositories?
# How:  Output a named numeric vector of the # of public repos
map_int(gh_users, ~.[["public_repos"]]) %>%
  set_names(map_chr(gh_users, "name")) %>%
  sort()

# Questions about gh_repos
# Question:  Which repo is the largest?
# How:  Map over gh_repos > map_dbl over the "size" element > 
# map again over size with ~max
# Map over gh_repos to generate numeric output
map(gh_repos, 
    ~map_dbl(.x, 
             ~.x[["size"]])) %>%
  # Grab the largest element
  map(~max(.x))
  
# Plotting with purrr
# Create a data frame containing public repos and followers
gh_users_df <- gh_users %>%
  map_df(~ tibble(
    public_repos = .x[["public_repos"]],
    followers = .x[["followers"]]
  ))
# Create a scatterplot of public repos vs followers
ggplot(data = gh_users_df,
       aes(x = public_repos, y = followers)) +
  geom_point()
# Create a histogram of followers by piping in df
gh_users_df %>%
  ggplot(aes(x = followers)) +
  geom_histogram()
# Create a dataframe with four columns
map_df(gh_users, `[`, 
       c("login", "name", "followers", "public_repos")) %>%
  # Plot followers by public_repos
  ggplot(., 
         aes(x = followers, y = public_repos)) + 
  # Create scatter plots
  geom_point()

# Question:  What is the distribution of heights of characters
# in each of the Star Wars films?
data(sw_people)
# Turn data into correct dataframe format
film_by_character <- tibble(filmtitle = map_chr(sw_films, "title")) %>%
  mutate(filmtitle, characters = map(sw_films, "characters")) %>%
  unnest()

# Pull out elements from sw_people
sw_characters <- map_df(sw_people, `[`, c("height", "mass", "name", "url"))

# Join the two new objects
character_data <- inner_join(film_by_character, sw_characters, 
                             by = c("characters" = "url")) %>%
  # Make sure the columns are numbers
  mutate(height = as.numeric(height), mass = as.numeric(mass))

# Plot the heights, faceted by film title
ggplot(character_data, aes(x = height)) +
  geom_histogram(stat = "count") +
  facet_wrap(~ filmtitle)

# 3 vectors containing hourly number of visits to the contact page of a website
# where visitors saw a different new site design for each vector.
visit_a <- c(117, 147, 131, 73, 81, 134, 121)
visit_b <- c(180, 193, 116, 166, 131, 153, 146)

# Convert hourly visits to visits per day
to_day <- function(x) {
  x * 24
}

# Create a list containing both vectors: all_visits
all_visits <- list(visit_a, visit_b)

# Convert to daily number of visits: all_visits_day
all_visits_day <- map(all_visits, to_day)

# Compare mean of visits: Map the mean() function and output a numeric vector
map_dbl(all_visits_day, mean)

# visit_c contains visits for users who saw the old design in the same week.
visit_c <- c(57, 110, 68, 72, 87, 141, 67)

# Create all_tests list and modify with to_day() function
all_tests <- list(visit_a, visit_b, visit_c)
all_tests_day <- map(all_tests, to_day)

# Plot all_tests_day with map
map(all_tests_day, barplot)

# Plot all_tests_day
walk(all_tests_day, barplot)

# Get the sum, of the all_tests_day list, element by element, and check its class
sum_all <- pmap_dbl(all_tests_day, sum)
class(sum_all)

# Using as_mapper to write reusable code
# Turn visit_a into daily number using an anonymous function
map(visit_a, function(x) {
  x * 24
})

# Turn visit_a into daily number of visits by using a mapper
map(visit_a, as_mapper(~ .x * 24))

# Create a mapper object called to_day
to_day <- as_mapper(~ .x * 24)

# Use it on the three vectors
map(visit_a, to_day)
map(visit_b, to_day)
map(visit_c, to_day)

# Cleaning up data with keep()
all_visits <- list(visit_a, visit_b, visit_c)
# Create a mapper that test if .x is more than 100 
is_more_than_hundred <- as_mapper(~ .x > 100)

# Use this mapper with keep() on the all_visits object 
map(all_visits, ~ keep(.x, is_more_than_hundred))

# Use the  day vector to set names to all_list
day <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
full_visits_named <- map(all_visits, ~ set_names(.x, day))

# Use this mapper with keep() 
map(full_visits_named, ~ keep(.x, is_more_than_hundred))

# Split up with keep() and discard()
# Set the name of each subvector
all_visits_named <- map(all_visits, ~ set_names(.x, day))

# Create a mapper that will test if .x is over 100 
threshold <- as_mapper(~ .x > 100)

# Run this mapper on the all_visits_named object: group_over
group_over <- map(all_visits_named, ~ keep(.x, threshold))

# Run this mapper on the all_visits_named object: group_under
group_under <-  map(all_visits_named, ~ discard(.x, threshold))

# Create a threshold variable, set it to 160
threshold <- 160

# Create a mapper that tests if .x is over the defined threshold
over_threshold <- as_mapper(~ .x > threshold)

# Are all elements in every all_visits vectors over the defined threshold? 
map(all_visits, ~ every(.x, over_threshold))

# Are some elements in every all_visits vectors over the defined threshold? 
map(all_visits, ~ some(.x, over_threshold))

# Safe iterations
urls <- list("https://thinkr.fr", "https://colinfay.me", "http://not_working.org", 
         "https://en.wikipedia.org", "http://cran.r-project.org/", "https://not_working_either.org")
# Create a safe version of read_lines()
safe_read <- safely(read_lines)

# Map it on the urls vector
res <- map(urls, safe_read)

# Set the name of the results to `urls`
named_res <- set_names(res, urls)

# Extract only the "error" part of each sublist
map(named_res, "error")

# Code a function that discard() the NULL from safe_read()
safe_read_discard <- function(url){
  safe_read(urls) %>%
    discard(is.null)
}

# Map this function on the url list
res <- map(urls, safe_read_discard)

# Create a possibly() version of read_lines()
possible_read <- possibly(read_lines, otherwise = 404)

# Map this function on urls, pipe it into set_names()
res <- map(urls, possible_read) %>% set_names(urls)

# Paste each element of the list 
res_pasted <- map(res, paste, collapse = " ")

# Keep only the elements which are equal to 404
keep(res_pasted, ~ .x == 404)

# Now write a function to do it all in one step
url_tester <- function(url_list){
  url_list %>%
    # Map a version of read_lines() that otherwise returns 404
    map( possibly(read_lines, otherwise = 404) ) %>%
    # Set the names of the result
    set_names( urls ) %>% 
    # paste() and collapse each element
    map(paste, collapse = " ") %>%
    # Remove the 404 
    discard(~ .x == 404) %>%
    names() # Will return the names of the good ones
}

# Try this function on the urls object
url_tester(urls)

# Modify the function to choose between returning results or errors
url_tester <- function(url_list, type = c("result", "error")) {
  type <- match.arg(type)
  url_list %>%
    # Apply safe_read to each URL
    map(safe_read) %>%
    # Set the names to the URLs
    set_names(url_list) %>%
    # Transpose into a list of $result and $error
    transpose()  %>%
    # Pluck the type element
    pluck(type) 
}

# Try this function on the urls object
url_tester(urls, type = "error") 

# Change the function to use GET from httr
library(httr)
url_tester <- function(url_list){
  url_list %>%
    # Map a version of GET() that would otherwise return NULL 
    map( possibly(GET, otherwise = NULL) ) %>%
    # Set the names of the result
    set_names( urls ) %>%
    # Remove the NULL
    compact() %>%
    # Extract all the "status_code" elements
    map("status_code")
}

# Try this function on the urls object
url_tester(urls)

# Now using only working urls
urls <- list("https://thinkr.fr", "https://colinfay.me", "https://en.wikipedia.org", "http://cran.r-project.org/")

# Compose a status extractor 
status_extract <- compose(status_code, GET)

# Try with "https://thinkr.fr" & "https://en.wikipedia.org"
status_extract("https://thinkr.fr")
status_extract("https://en.wikipedia.org")

# Map it on the urls vector, return a vector of numbers
map_dbl(urls, status_extract)

# Negate the %in% function 
`%not_in%` <- negate(`%in%`)

# Compose a status extractor 
extract_status <- compose(status_code, GET)

# Complete the function definition
strict_code <- function(url) {
  # Extract the status of the URL
  code <- extract_status(url)
  # If code is not in the acceptable range ...
  if (code %not_in% 200:203) {
    # then return NA
    return(NA)
  }
  code
}

# Map the strict_code function on the urls vector
res <- map(urls, strict_code)

# Set the names of the results using the urls vector
res_named <- set_names(res, urls)

# Negate the is.na function
is_not_na <- negate(is.na)

# Run is_not_na on the results
is_not_na(res_named)

# A content extractor
library(rvest)

# Prefill html_nodes() with the css param set to h2
get_h2 <- partial(html_nodes, css = "h2")

# Combine the html_text, get_h2 and read_html functions
get_content <- compose(html_text, get_h2, read_html)

# Map get_content to the urls list
res <- map(urls, get_content) %>%
  set_names(urls)

# Print the results to the console
res

# Extract all links on a specific page
# Create a partial version of html_nodes(), with the css param set to "a"
get_a <- partial(html_nodes, css = "a")

# Create href(), a partial version of html_attr()
href <- partial(html_attr, name = "href")

# Combine href(), get_a(), and read_html()
get_links <- compose(href, get_a, read_html)

# Map get_links() to the urls list
res <- map(urls, get_links) %>%
  set_names(urls)

# See the result
res

