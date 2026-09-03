library(readabs)

# 1. List every catalogue readabs knows about
catalogues <- show_available_catalogues()
# Returns a data frame of catalogue names and numbers (e.g. "6401.0" for CPI)
# Useful for browsing when you don't know the exact number

# 2. Search catalogues by keyword
search_catalogues("GNE")
# Searches catalogue titles for a keyword and returns matches
# Good when you know roughly what topic you want but not the cat_no

# 3. Once you have a catalogue number, list the tables/files inside it
show_available_files(cat_no = "6401.0")
# Returns the individual spreadsheet files within that catalogue
# (e.g. Table 1, Table 2...) so you know which one to download

national_accounts <- read_abs(cat_no = "5206.0", tables = 1)
# Downloads Table 1 of the National Accounts catalogue and tidies it
# into a long data frame with columns like date, series, value, series_type

# Now filter for the GNE series by matching the description text
gne <- national_accounts[
  grepl(
    "Gross national expenditure",
    national_accounts$series,
    ignore.case = TRUE
  ),
]


library(dplyr)
gne <- national_accounts %>%
  filter(series_id == "A2304113C")

gne <- national_accounts[national_accounts$series_id == "A2304113C", ]

nc <- national_accounts[national_accounts$series_id == "A2304037L", ]


nrow(national_accounts) # is the data frame even populated?
"A2304037L" %in% national_accounts$series_id # TRUE/FALSE — is it in there at all?

nc2 <- read_abs(series_id = "A2304037L")
