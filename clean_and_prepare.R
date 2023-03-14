# install.packages("tidyverse")
# library(tidyverse)

install.packages("readxl")
library(readxl)

# Read grain.xlsx file and two sheets into page.one and page.two

setwd("C:\\Users\\Shree\ Ravi\ Adhikari\\OneDrive\\Documents\\ANA515EXP")

page.one <- read_excel("grain.xlsx",sheet = "Sheet1")
page.two <- read_excel("grain.xlsx",sheet = "Sheet2")

dim(page.one)
dim(page.two)

# Using the R's command rbind, merge two datasets.

dataframe <- rbind(page.one, page.two)
dim(dataframe)

library(tidyverse)

# There are values like NA, --, --- 
# treat them as N/A 

df <- dataframe %>%
  mutate(across(where(is.character), ~na_if(., "---")))

df <- df %>%
  mutate(across(where(is.character), ~na_if(., "--")))

df <- df %>%
  mutate(across(where(is.character), ~na_if(., "NA")))

# missing_data_count shall compute and store the number of missing values in each column 
missing_data_count <- setNames(
  data.frame(matrix(ncol = 2, nrow = 0)), 
  c("Column", "NullValues")
)

for (column in colnames(df)) {
  missing = sum(is.na(df[column]))
  missing_data_count[nrow(missing_data_count) + 1, ] = c(column, missing)
  print(sprintf("%d missing values for %s", missing, column))
}

# barplot of missing values

ggplot(
  missing_data_count, 
  aes(x=Column, y = NullValues)) + 
  geom_bar(stat="identity")


# There are invalid characters like ", "\n", "\r" characters in column Landgrabber

invalid_chars = "\"|\r|\n"

df$Landgrabber <- gsub(pattern = invalid_chars, replacement = "", x = df$Landgrabber)

# create a copy of `status` column
df$status <- df$`Status of deal`

# remove `Summary` and `Projected Investment`
# Because `summary` is a huge chunk of text. It might be useful for NLP project, but not here.
# Projected Investment because "301 missing values for Projected investment"
df <- df %>%
  select(-`Status of deal`, -Summary, -`Projected investment`)

# rename all column names to lowercase
names(df) <- tolower(names(df))

# Fix status column

# For Done, In process and their variant create a single value
done_codes = c("Done", "Don", "Done\r\n", "Done (50-yr lease)", "Done - 15/08/2011")

df$status <- sapply(
  as.vector(df$status), 
  function(x) if(x %in% done_codes) "Done" else x 
)

processing_codes = c("Inprocess", "In process")

df$status <- sapply(
  as.vector(df$status), 
  function(x) if(x %in% processing_codes) "In Process" else x 
)

# mou signed
df$status <- sapply(
  as.vector(df$status), 
  function(x) if(x == "MoU signed (2009)") "MoU Signed" else x 
)

# suspended
df$status <- sapply(
  as.vector(df$status), 
  function(x) if(x == "Suspended (October 2011)") "Suspended" else x 
)

# barplot of statuses

barplot(
  table(
    df$status
  )
)

# Finally export the dataframe to csv
write.csv(df, "cleaned_data.csv", row.names=FALSE)

