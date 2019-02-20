# Data Pre-Processing

# Create "tfr_panel_data.csv"
# DV: DataValue -> TFR values
# IV: RecallLag
tfr.ini = read.csv(file = "tfr_data.csv", sep = ",")
tfr.ini$year = format(as.Date(format(date_decimal(tfr.ini$TimeMid), "%Y-%m-%d")), "%Y")

tfr.ini =
  tfr.ini %>%
  group_by(Country.or.area, year) %>%
  select(Country.or.area, year, DataValue, RecallLag) %>%
  summarise(DataValue = mean(DataValue), RecallLag = mean(RecallLag)) 

timeline = data_frame(year = rep(1950:2016, each = length(unique(tfr.ini$Country.or.area))))
timeline$Country.or.area = rep(unique(tfr.ini$Country.or.area), 67)

tfr = merge(x = timeline, y = tfr.ini, by = c("Country.or.area", "year"), all.x = TRUE)

# Add two categorical IVs: region & development
reg.dev = read.csv(file = "un_subregion.csv", sep = ",")
dat = merge(x = tfr, y = reg.dev, by = "Country.or.area")

write.csv(dat, file = "tfr_panel_data.csv")

rm(list = ls())

# Create "tfr_india.csv"
# Subset the data to include "India" ONLY
# Subset the data to include only data ranging from 1960 to 2016
dat = read.csv(file = "tfr_panel_data.csv", sep = ",") %>% 
  filter(year >= 1960, Country.or.area == "India")

# Add GDP(in billions) as a continuous variable (round to 2 decimals)
gdp = read.csv(file.choose(), header = T, skip = 4, sep = ",") 
gdp = gdp %>%
  filter(Country.Name == "India")
gdp = as.numeric(t(gdp))
dat$gdp = round(gdp[!is.na(gdp)][1:57] / 1000000000, 2)

# Recode RecallLag to categorical
dat$RecallLag = abs(dat$RecallLag)
# hist(data$RecallLag)
dat$recall = cut(dat$RecallLag,
                 breaks = c(-Inf, 1, 4, Inf),
                 labels = c("Immediate", "Fast", "Slow"))

dat$DataValue = round(dat$DataValue, 1)

keep = c("DataValue", "year", "gdp", "recall")
dat = dat[ ,keep]
names(dat) = c("TFR", "Year", "GDP", "Recall")

rm(gdp)
rm(keep)

write.csv(dat, file = "tfr_india.csv")
