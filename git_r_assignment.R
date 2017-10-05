# Author: Joan Wang
# Purpose: Exploratory analysis of the DoT FARS data


# install pertinent packages
# install.packages(c('readr', 'haven', 'dplyr', 'tidyr', 'stringr', 'ggplot2'))
library('readr')
library('haven')
library('dplyr')
library('tidyr')
library('stringr')
library('ggplot2')


# load FARS data
acc2014 <- read_sas('accident.sas7bdat')
acc2015 <- read_csv('accident.csv')

ls()
class(acc2014)
class(acc2015)


# combine two years of FARS data
acc2014 <- mutate(acc2014, TWAY_ID2=na_if(acc2014$TWAY_ID2, ""))
table(is.na(acc2014$TWAY_ID2))        

dim(acc2014)
dim(acc2015)
all_cols <- c(colnames(acc2014), colnames(acc2015))

print_missing <- function(df){
  bools <- all_cols %in% colnames(df)
  falses <- which(!bools) 
  for (col in falses) {
    print(all_cols[col])
  }
}

print_missing(acc2014) # missing columns: "RUR_URB", "FUNC_SYS", "RD_OWNER"
print_missing(acc2015) # missing columns: "ROAD_FNC"
# NOTE: is there a more straightforward way to do this?

acc <- bind_rows(acc2014, acc2015)
count(acc, RUR_URB)
# There are over 30k NA values for the RUR_URB column of the combined tibble
# because this column did not exist in acc2014. When the two tibbles were combined
# using bind_rows(), the 2014 values for that column were filled with NA.


# merging on another data source
fips <- read_csv('fips.csv')
glimpse(fips)

acc$COUNTY = str_pad(as.character(acc$COUNTY), 3, 'left', 0)
acc$STATE = str_pad(as.character(acc$STATE), 2, 'left', 0)
acc <- rename(acc, StateFIPSCode = STATE, CountyFIPSCode = COUNTY)

joined <- left_join(acc, fips, by = c('StateFIPSCode', 'CountyFIPSCode'))


# exploratory data analysis
agg <- group_by(joined, YEAR, StateName) %>%
  summarise(TOTAL = sum(FATALS, na.rm=TRUE))

agg_wide <- spread(agg, YEAR, TOTAL) %>%
  rename(TOTAL_2014 = '2014', TOTAL_2015 = '2015')

agg_wide <- mutate(agg_wide, pct_diff = (TOTAL_2015 - TOTAL_2014)/TOTAL_2014)

agg_wide <- arrange(agg_wide, desc(pct_diff))

agg_wide <- filter(agg_wide, pct_diff > 0.15, StateName != "<NA>")

# rewrite using chain operators
agg_wide <- group_by(joined, YEAR, StateName) %>%
  summarise(TOTAL = sum(FATALS, na.rm=TRUE)) %>%
  spread(YEAR, TOTAL) %>%
  rename(TOTAL_2014 = '2014', TOTAL_2015 = '2015') %>%
  mutate(pct_diff = (TOTAL_2015 - TOTAL_2014)/TOTAL_2014) %>%
  arrange(desc(pct_diff)) %>%
  filter(pct_diff > 0.15, StateName != "<NA>")


