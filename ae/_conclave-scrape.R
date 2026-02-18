# ==============================================================================
# packages
# ==============================================================================

library(rvest)
library(tidyverse)

# ==============================================================================
# scrape
# ==============================================================================

url <- "https://catholic-hierarchy.org/bishop/spope0.html"
page <- read_html(url)

my_tables <- html_nodes(page, "table")

popes_table <- html_table(my_tables[[1]], fill = TRUE)
names(popes_table) <- make.unique(names(popes_table))

# ==============================================================================
# clean
# ==============================================================================

popes_clean <- popes_table |>
  rename(
    papal_number = `#`,
    papal_name = `PapalName`,
    birth_date = `BirthDate`,
    conclave_start = `ConclaveStart`,
    date_elected = `Elected`,
    age_elected = `Elected.1`,
    date_installed = `Installed`,
    age_installed = `Installed.1`,
    date_end = `End of Reign`, 
    age_end = `End of Reign.1`,
    years_elected = `Length`,
    years_installed = `Length.1`,
  ) |>
  filter(papal_name != "PapalName") |>
  mutate(
    papal_number = as.integer(papal_number),
    age_elected = as.numeric(age_elected),
    age_installed = as.numeric(age_installed),
    age_end = as.numeric(age_end),
    years_elected = as.numeric(years_elected),
    years_installed = as.numeric(years_installed),
    conclave_start = as.Date(conclave_start, format = "%d %b %Y"),
    date_elected = as.Date(date_elected, format = "%d %b %Y"),
    birth_date = as.Date(birth_date, format = "%d %b %Y"),
    date_end = ifelse(papal_name == "Leo XIV", NA, date_end),
    resigned = str_detect(date_end, "#"),
    date_end = str_remove_all(date_end, "#"),
    date_end = as.Date(date_end, format = "%d %b %Y"),
    not_bishop = str_detect(date_installed, "\\*"),
    date_installed = str_remove_all(date_installed, "\\*"),
    date_installed = as.Date(date_installed, format = "%d %b %Y"),
    conclave_length_days = as.integer(date_elected - conclave_start)
  )

# ==============================================================================
# plot
# ==============================================================================

ggplot(popes_clean, aes(x = conclave_length_days)) + 
  geom_histogram() + 
  labs(
    x = "Days",
    title = "Length of papal conclaves",
    caption = "Source: scraped from catholic-hierarchy.org"
    )
