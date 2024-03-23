# ====================================================================================
# boe_avg_base_rates_calc.R
# ====================================================================================


# ------------------------------------------------------------------------------------
# USER INPUTS:
# ------------------------------------------------------------------------------------

# set path to universal input variables for this year
ui_path <- "C:/Users/danin/OneDrive/Documents/Shanda Holdings Projects/2024_Project_Year/1_QA/universal_inputs_2023_2024_(April_2024).csv"

# path to save outputs (where earnings_growth.csv is saved)
output_path <- "C:/Users/danin/OneDrive/Documents/Shanda Holdings Projects/2024_Project_Year/2_Raw Data/Macroeconomic/"

# ------------------------------------------------------------------------------------

# The URL is built using the instructions here:
# https://www.bankofengland.co.uk/boeapps/database/help.asp?Back=Y&Highlight=CSV#CSV

# read in universal inputs
ui <- fread(ui_path)

boe_historic_base_rate_maker <- function(ui, output_path_boe){
  
# extract distribution year from the universal inputs
  distribution_year <- ui %>% filter(input_name == "distribution_year" ) %>% select(input_value) %>% as.character() %>% as.integer()
  
  series_code <- "IUDBEDR"
  start_date <- "01/Apr/2001"
  end_date <- paste0("31/Mar/", distribution_year)
  url = paste0("https://www.bankofengland.co.uk/boeapps/iadb/fromshowcolumns.asp?csv.x=yes&Datefrom=",
               start_date,
               "&Dateto=",
               end_date,
               "&SeriesCodes=",
               series_code,
               "&CSVF=TN&UsingCodes=Y&VPD=Y&VFD=N")
  
  daily_base_rate_boe <- fread(url)
  
  # Clean the data and summarise it
  
  # The data has a DATE column with the date the "dd-mm-yyyy" format and a IUDBEDR column with the
  # value
  # Here we determine the financial year, and we group it to calculate the means by financial year.
  # The outcome is written to the desired output path.
  
  daily_base_rate_boe %>% 
    mutate(year = lubridate::year(as.Date(DATE, "%d %b %Y")),
           month = lubridate::month(as.Date(DATE, "%d %b %Y")),
           fin_year = ifelse(month >= 4,
                             paste(year, year +1, sep = "_"),
                             paste(year - 1, year, sep = "_"))) %>%
    group_by(fin_year) %>%
    summarise(avg = mean(IUDBEDR)) %>%
    fwrite(., paste0(output_path, "boe_avg_base_rates.csv"))
}