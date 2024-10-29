# Make sure to install Chrome or Chromium browser before running this script!

# Dependency ---- 

# We also need a few R packages to interact with the browser, scrape, and clean
# the data. Please install them if you haven't already by uncommenting and
# running: 

# install.packages(c("chromote", "rvest", "lubridate", "dplyr"))

# Load the packages
library(rvest)
library(lubridate)
library(dplyr)
library(chromote)

# Launch Browser ----

# Create a new browser session
chrome_session <- ChromoteSession$new()

# Launch chrome to view actions taken in the browser
chrome_session$view()

# Get the browser's version
chrome_session$Browser$getVersion()

# Head over to R-project.org ----

# Open a new tab and navigate to a URL
chrome_session$Page$navigate("https://www.r-project.org/")

# Wait for the page to load
Sys.sleep(1)

# Highlight an element on the page
chrome_session$Runtime$evaluate(
  expression = "
  // Find the element
  element = document.querySelector('.sidebar');
  
  // Highlight it
  element.style.backgroundColor = 'yellow';
  element.style.border = '2px solid red';
  "
)

# Wait for the action to complete
Sys.sleep(0.5)

# Take a screenshot of the highlighted element
chrome_session$screenshot("r-project-sidebar.png", selector = ".sidebar")

# View the screenshot
browseURL("r-project-sidebar.png")

## Retrieve sidebar element ----

# Retrieve the sidebar content
node <- chrome_session$DOM$querySelector(
  nodeId = chrome_session$DOM$getDocument()$root$nodeId,
  selector = ".sidebar"
)

# Get the outerHTML of the node
html_content <- chrome_session$DOM$getOuterHTML(
  nodeId = node$nodeId
)


## Parse the sidebar content with `rvest` ----

# Pull the node's HTML response
html_content$outerHTML |>      # Extract the HTML content
  rvest::minimal_html() |>     # Convert to XML document
  rvest::html_elements("a") |> # Obtain all anchor tags
  rvest::html_text()           # Extract the text from the anchor tags


# Create a new tab session for windy.com ----
  
# Start a new browser tab session
chrome_session_windy <- chrome_session$new_session()

# Open a new tab in the current browser
chrome_session_windy$view()

## Navigate to windy.com ----

# Navigate to windy.com
chrome_session_windy$Page$navigate("https://www.windy.com")

# Wait for the page to load
Sys.sleep(0.5) 

## Search for a location ----

# First focus the input field
chrome_session_windy$Runtime$evaluate('
  document.querySelector("#q").focus();
')

# Brief pause to ensure focus is complete
Sys.sleep(0.5) 

# Enter search term and trigger search
search_query <- 'Stanford University Museum of Art'
chrome_session_windy$Runtime$evaluate(
  expression = sprintf('{
    // Get the search input
    const searchInput = document.getElementById("q");
    searchInput.value = "%s";
    
    // Focus the input
    searchInput.focus();
    
    // Trigger input event
    const inputEvent = new Event("input", { bubbles: true });
    searchInput.dispatchEvent(inputEvent);
    
    // Trigger change event
    const changeEvent = new Event("change", { bubbles: true });
    searchInput.dispatchEvent(changeEvent);
    
    // Force the search to update - this triggers the site\'s search logic
    const keyupEvent = new KeyboardEvent("keyup", {
      key: "a",
      code: "KeyA",
      keyCode: 65,
      bubbles: true
    });
    searchInput.dispatchEvent(keyupEvent);
  }', search_query)
)

# Wait for and, then, click the first search result
Sys.sleep(0.5) 
chrome_session_windy$Runtime$evaluate('
    document.querySelector(".results-data a").click();
  ')

## Extract weather data ----
  
# Wait for and, then, extract the weather data table
Sys.sleep(0.5) 
html <- chrome_session_windy$Runtime$evaluate('
    document.querySelector("table#detail-data-table").outerHTML
  ')$result$value

## Parse the table using `rvest` ----
raw_weather_table <- html |>
  read_html() |> 
  html_node('table') |> # Select the table to extract it without getting a node set
  html_table() |> # Convert the table to a data frame
  as.data.frame()

raw_weather_table

## Implement a weather cleaning function ----

clean_weather_table <- function(df) {
  
  # Retrieve the current date
  current_month <- as.numeric(format(Sys.Date(), "%m"))
  current_year <-  as.numeric(format(Sys.Date(), "%Y"))
  current_day <- as.numeric(format(Sys.Date(), "%d"))
  
  # Create times vector from row 2
  times <- df[2, ] |> 
    as.character() |> 
    na_if("") |> 
    na.omit() |>
    as.vector()
  
  # Create values from rows 4, 6, 7, and 8
  temps <- df[4, ] |> str_extract("[0-9]+") |> na.omit() |> as.numeric()
  wind_speed <- df[6, ] |> as.numeric() |> na.omit()
  wind_gusts <- df[7, ] |> as.numeric() |> na.omit()
  beaufort <- df[8, ] |> as.numeric() |> na.omit()
  
  # Extract and parse dates more carefully from first row
  dates <- df[1, ] |> 
    as.character() |> 
    na_if("") |>
    na.omit() |>
    unique()  # Get unique date entries
  
  # Create a mapping of column positions to dates
  date_mapping <- df[1, ] |>
    as.character() %>%       
    {tibble(
      col_num = seq_along(.),
      date = .
    )} |>
    filter(!is.na(date), date != "") |>
    # Fill in the dates for columns that share the same date
    fill(date)
  
  # Handle both AM/PM times and 24-hour format
  clean_times <- times |>
    str_replace("(\\d+)(AM|PM)", "\\1 \\2") |>  # Add space before AM/PM
    str_replace("(\\d+)$", "\\1M") |>           # Add M to bare numbers for evening
    str_replace("(\\d{1})([AP]M)", "0\\1 \\2")   # Ensure two digits before AM/PM
  
  
  # Combine into a clean dataframe
  clean_df <- tibble(
    col_position = seq_along(temps),
    temperature_f = temps,
    wind_speed_kts = wind_speed,
    wind_gusts_kts = wind_gusts,
    beaufort = beaufort,
    time = clean_times
  ) |>
    # Join with date mapping
    left_join(date_mapping, by = c("col_position" = "col_num")) |>
    # Extract day info
    mutate(
      day_name = str_extract(date, "\\w+day"),
      day_num = str_extract(date, "\\d+"),
      raw_datetime = paste(day_name, day_num, time, current_year)
    ) |>
    # Convert strings to proper datetime objects
    mutate(
      datetime = parse_date_time(
        raw_datetime, 
        orders = c(
          "a d I p Y",    # e.g., "Tuesday 29 7 PM 2024"
          "a d HM Y"      # e.g., "Tuesday 29 1900 2024"
        ),
        quiet = TRUE
      )
    ) |>
    # Handle month transitions
    mutate(
      current_day = current_day,
      month_adj = if_else(
        as.numeric(day_num) < current_day,
        current_month %% 12 + 1,
        current_month
      ),
      datetime = if_else(
        !is.na(datetime),
        datetime + months(month_adj - month(datetime)),
        datetime
      )
    ) |>
    # Add converted measurements
    mutate(
      wind_speed_mph = wind_speed_kts * 1.15078,   # Convert knots to mph
      wind_gusts_mph = wind_gusts_kts * 1.15078,   # Convert knots to mph
      temperature_c = (temperature_f - 32) * 5/9   # Convert °F to °C
    ) |>
    # Clean up intermediate columns
    select(-col_position, -current_day, -month_adj, -date, -raw_datetime)
  
  return(clean_df)
}

## Clean the raw weather table ----
cleaned_weather <- clean_weather_table(raw_weather_table)
cleaned_weather

## Summarize the weather data ----
cleaned_weather |>
  summarise(
    avg_temp_f = mean(temperature_f),
    max_wind = max(wind_speed_mph),
    max_gusts = max(wind_gusts_mph),
    avg_beaufort = mean(beaufort)
)

# Clean up the browser sessions ----

chrome_session_windy$close()
chrome_session$close()
