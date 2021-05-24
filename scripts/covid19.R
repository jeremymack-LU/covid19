library(dplyr)

df.cases <- read.csv("covid19_daily_report.csv", sep=",", header=TRUE)
df.names <- read.csv("county_names.csv", sep=",", header=TRUE)

df.dates <- df.cases %>% distinct(Date) %>% as.data.frame()

df.all <- merge(df.dates, df.names, all=TRUE)
names(df.all)[2] <- "County"

df <- merge(df.all, df.cases, by=c("County","Date"), all.x=TRUE)
df[is.na(df)] <- 0

df <- df %>% mutate(Date=as.Date(Date, format = "%m/%d/%y"))
df <- df %>% arrange(County, Date)

write.csv(df, file="covid19_pa_counties.csv", row.names=FALSE)
