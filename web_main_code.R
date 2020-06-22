# Libraries needed

library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(knitr)
library(kableExtra)
library(scales)
library(tibble)
library(DT)

# Read the source file
setwd("/Users/usersgianazzi/Documents/data_science/data_files")
ds <- read.csv2("rifiuti_porto_genova_2008_2017_eda_0_labels.csv", header=TRUE, sep = ";", check.names = FALSE)

# Multiple columns taken and collapsed into key-value pairs
ds <- gather(ds, year, tonnes, -CER)

# Year converted to numeric
ds <- ds %>%
        mutate(year=as.numeric(year))

# Data reshaped, from long to wide format
ds_wide <- ds %>%
        spread(CER, tonnes)

# How many recors are missing?
vuoti <- sum(is.na(ds_wide))

# Dimension of the tidy dataset
dimension <- dim(ds)
RowNum <- nrow(ds)
ColNum <- ncol(ds)
sum(is.na(ds))

# A column with rows totals is added
ds_wide_presentation <- mutate(ds_wide, total = rowSums(ds_wide[,-1], na.rm = TRUE))

# Extract year and rows totals for charts and for presentation table
year_and_rows_total_for_chart <- select(ds_wide_presentation, year, total)

year_and_rows_total_for_table <- year_and_rows_total_for_chart %>%
        mutate(year=as.integer(year)) %>% 
        spread(year, total) 

grafico1 <- ggplot(year_and_rows_total_for_chart, aes(year, total)) + 
        geom_point() + 
        geom_smooth() + 
        labs(title="Waste collection in the Port of Genoa (t per year, 2008-2018)", x="Year of collection", y="Tonnes per year") +
        scale_x_continuous(breaks= pretty_breaks())

total_2008 <- filter(ds, year == 2008)
total_2009 <- filter(ds, year == 2009)
total_2010 <- filter(ds, year == 2010)
total_2011 <- filter(ds, year == 2011)
total_2012 <- filter(ds, year == 2012)
total_2013 <- filter(ds, year == 2013)
total_2014 <- filter(ds, year == 2014)
total_2015 <- filter(ds, year == 2015)
total_2016 <- filter(ds, year == 2016)
total_2017 <- filter(ds, year == 2017)
total_2018 <- filter(ds, year == 2018)
