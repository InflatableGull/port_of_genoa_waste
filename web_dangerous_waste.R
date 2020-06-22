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

# A column with rows totals is added
ds_wide_presentation <- mutate(ds_wide, total = rowSums(ds_wide[,-1], na.rm = TRUE))



# All CER codes for dangerous waste in a summary wide table
pericolosi_all <-  select(ds_wide_presentation, year, CER_050103,CER_060106,CER_070104,CER_070208,CER_070601,CER_070704,CER_080111,CER_080117,
                          CER_080121,CER_080409,CER_090101,CER_090105,CER_120114,CER_120116,CER_120301,CER_130205,CER_130206,CER_130502,
                          CER_130507,CER_130802,CER_140603,CER_150110,CER_150202,CER_160104,CER_160107,CER_160121,CER_160211,CER_160213,
                          CER_160215,CER_160303,CER_160305,CER_160403,CER_160504,CER_160506,CER_160508,CER_160601,CER_160602,CER_160708,
                          CER_160709,CER_161001,CER_161003,CER_170106,CER_170204,CER_170301,CER_170410,CER_170503,CER_170603,CER_170605,
                          CER_170903,CER_180103,CER_190105,CER_190205,CER_190207,CER_190810,CER_190813,CER_191307,CER_200121)

# A column with rows totals is added
dw_wide_presentation <- mutate(pericolosi_all, total = rowSums(pericolosi_all[,-1], na.rm = TRUE))

# Extract year and rows totals for dangerous waste for charts and for presentation table
year_and_rows_danger_for_chart_dw <- select(dw_wide_presentation, year, total)

year_and_rows_total_for_table_dw <- year_and_rows_danger_for_chart_dw %>%
        mutate(year=as.integer(year)) %>% 
        spread(year, total) 


library(scales)


grafico2 <- ggplot(year_and_rows_danger_for_chart_dw, aes(year, total)) + 
        geom_point() + 
        geom_smooth() + 
        labs(title="Dangerous waste collection in the Port of Genoa (t per year, 2008-2018)", x="Year of collection", y="Tonnes per year") +
        scale_x_continuous(breaks= pretty_breaks())

pericolosi_all <-  select(ds_wide_presentation, CER_050103,CER_060106,CER_070104,CER_070208,CER_070601,CER_070704,CER_080111,CER_080117,CER_080121,CER_080409,CER_090101,CER_090105,CER_120114,CER_120116,CER_120301,CER_130205,CER_130206,CER_130502,CER_130507,CER_130802,CER_140603,CER_150110,CER_150202,CER_160104,CER_160107,CER_160121,CER_160211,CER_160213,CER_160215,CER_160303,CER_160305,CER_160403,CER_160504,CER_160506,CER_160508,CER_160601,CER_160602,CER_160708,CER_160709,CER_161001,CER_161003,CER_170106,CER_170204,CER_170301,CER_170410,CER_170503,CER_170603,CER_170605,CER_170903,CER_180103,CER_190105,CER_190205,CER_190207,CER_190810,CER_190813,CER_191307,CER_200121)

pericolosi_all_sum <- sapply(pericolosi_all, sum, na.rm = TRUE)
pericolosi_totali_per_anno <- rowSums(pericolosi_all, na.rm = T)

##################################################################################################################################################################################

x1 <- select(ds_wide_presentation,CER_050103,CER_060106,CER_070104,CER_070208,CER_070601,CER_070704,CER_080111,CER_080117,CER_080121,CER_080409)

x2 <- colSums(x1, na.rm = TRUE)
x3 <- rbind(x1, x2)
rownames(x3) <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "Total")

##################################################################################################################################################################################

y1 <- select(ds_wide_presentation,CER_090101,CER_090105,CER_120114,CER_120116,CER_120301,CER_130205,CER_130206,CER_130502,CER_130507,CER_130802)

y2 <- colSums(y1, na.rm = TRUE)
y3 <- rbind(y1, y2)
rownames(y3) <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "Total")

##################################################################################################################################################################################

z1 <- select(ds_wide_presentation,CER_140603,CER_150110,CER_150202,CER_160104,CER_160107,CER_160121,CER_160211,CER_160213,CER_160215,CER_160303)

z2 <- colSums(z1, na.rm = TRUE)
z3 <- rbind(z1, z2)
rownames(z3) <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "Total")

##################################################################################################################################################################################

j1 <- select(ds_wide_presentation,CER_160403,CER_160504,CER_160506,CER_160508,CER_160601,CER_160602,CER_160708,CER_160709,CER_161001,CER_161003)
j2 <- colSums(j1, na.rm = TRUE)
j3 <- rbind(j1, j2)
rownames(j3) <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "Total")

##################################################################################################################################################################################

w1 <- select(ds_wide_presentation,CER_170106,CER_170204,CER_170301,CER_170410,CER_170503,CER_170603,CER_170605,CER_170903,CER_180103,CER_190105)
w2 <- colSums(w1, na.rm = TRUE)
w3 <- rbind(w1, w2)
rownames(w3) <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "Total")

##################################################################################################################################################################################

k1 <- select(ds_wide_presentation,CER_190205,CER_190207,CER_190810,CER_190813,CER_191307,CER_200121)
k2 <- colSums(k1, na.rm = TRUE)
k3 <- rbind(k1, k2)
rownames(k3) <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "Total")