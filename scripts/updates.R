# Load packages -----------------------------------------------------------
library("rvest")
library("tidyverse")
library("zoo")
library("pdftools")
library("maps")
library("readr")

devtools::install_github("yonghah/esri2sf")
library("esri2sf")

# Covid-19 data -----------------------------------------------------------
## PA Department of Health - feature layer ----
url <- 'https://services1.arcgis.com/Nifc7wlHaBPig3Q3/arcgis/rest/services/COVID_PA_Counties/FeatureServer/0'

sf <- esri2sf(url)

sf <- sf %>% arrange(County); sf[42,1] <- "McKean"

df <- sf %>%
  as.data.frame() %>%
  select(County,Cases,Deaths) %>%
  mutate(PA_counties_NAME=as.factor(County),
         Covid_cases=as.numeric(gsub(",","",Cases)),
         Covid_deaths=as.numeric(gsub(",","",Deaths))) %>%
  mutate(Covid_deaths=ifelse(is.na(Covid_deaths),0,Covid_deaths)) %>%
  arrange(County)

df <- df[,c(4:6)]
df <- df %>% filter(PA_counties_NAME != "Pennsylvania")

# ## PA Department of Health - daily pdfs ----
# # Uncomment the entire chunk by using Ctrl + Shift + C
# url1 <- "https://www.health.pa.gov/topics/Documents/Diseases%20and%20Conditions/COVID-19%20County%20Data/County%20Case%20Counts_4-16-2021.pdf"
# 
# pdf1 <- pdf_text(url1) %>%  # Select the linked PDF with case data
#   read_lines()              # Read lines into a list of vectors
# 
# cases <- pdf1 %>%           # Select the list of vectors
#   str_squish() %>%          # Remove extra whitespace between elements
#   str_split(pattern=" ")    # Split vector string into pieces (i.e., columns)
# cases <- do.call(rbind,
#                  Filter(function(x) length(x)==6, cases))  # Combine list elements with 6 items
# #cases <- cases[-1,]                                        # Remove first row
# cases <- cases %>%
#   as.data.frame() %>%                             # Convert to a data frame
#   mutate(County=as.factor(V1),                    # Set County to factor
#          Cases=as.numeric(as.character(V3))) %>%  # Set Cases to numeric
#   mutate(County=str_to_sentence(County)) %>%      # Change County from all caps
#   select(7:8)
# head(cases, 5)
# 
# url2 <- "https://www.health.pa.gov/topics/Documents/Diseases%20and%20Conditions/COVID-19%20Death%20Data/Death%20by%20County%20of%20Residence%20--%202021-04-16.pdf"
# 
# pdf2 <- pdf_text(url2) %>%  # Select the linked PDF with case data
#   read_lines()              # Read lines into a list of vectors
# 
# deaths <- pdf2 %>%          # Select the list of vectors
#   str_squish() %>%          # Remove extra whitespace between elements
#   str_split(pattern=" ")    # Split vector string into pieces (i.e., columns)
# deaths <- do.call(rbind,
#                   Filter(function(x) length(x)==4, deaths)) # Combine list elements with 3 items
# deaths <- deaths %>%
#   as.data.frame() %>%                                       # Convert to a data frame
#   mutate(County=as.factor(V1),                              # Set County to factor
#          Deaths=as.numeric(gsub(",","",V2))) %>%            # Set Cases to numeric
#   select(5:6)
# head(deaths, 5)
# 
# df <- merge(cases, deaths, by="County", all.x=TRUE)    # Merge by County
# df <- df %>%                                           # Set data structure for variables
#   mutate(County=as.factor(County),                     # Set County to factor
#          Cases=as.numeric(Cases),                      # Set Cases to numeric
#          Deaths=as.numeric(gsub(",","", Deaths))) %>%  # Set Deaths to numeric
#   mutate(Deaths=ifelse(is.na(Deaths),0, Deaths))       # Change NAs to 0
# head(df, 10)
# 
# names(df) <- c("PA_counties_NAME", "Covid_cases", "Covid_deaths")



# Update dataset1 ---------------------------------------------------------
# Load data
df2 <- read.table("/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_pa_data-update.csv",
                  sep=",",
                  header=TRUE)

df2 <- df2 %>% mutate(Covid_cases=df$Covid_cases,
                      Covid_deaths=df$Covid_deaths)

write.table(df2,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_pa_data-update.csv",
            sep=",",
            row.names=FALSE)


# Update dataset2 ---------------------------------------------------------
# Load data
df3 <- read.table("/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_pa_data.csv",
                  sep=",",
                  header=TRUE)

df3 <- df3 %>% mutate(COVID19_cases=df$Covid_cases,
                      COVID19_deaths=df$Covid_deaths)

write.table(df3,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_pa_data.csv",
            sep=",",
            row.names=FALSE)


# Update time-series dataset ----------------------------------------------
# Load data
df4 <- read.table("covid19_pa_counties.csv",
                  sep=",",
                  header=TRUE)

df4 <- df4 %>% mutate(Date=as.Date(Date, format="%Y-%m-%d"))

df <- df %>% 
  mutate(Date=Sys.Date()) %>%
  mutate(County=PA_counties_NAME,
         Date=Date,
         Cases=as.integer(Covid_cases),
         Deaths=as.integer(Covid_deaths)) %>%
  select(County, Date, Cases, Deaths)

df4 <- df4 %>%
  add_row(County=df$County,
          Date=df$Date,
          Cases=df$Cases,
          Deaths=df$Deaths)

df4 <- df4 %>% arrange(County, Date)

write.table(df4,
            "covid19_pa_counties.csv",
            sep=",",
            row.names=FALSE)
##################################################################################
# Load and update dataset4
df5 <- read.table("/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_daily_total_pa.csv",
                  sep=",",
                  header=TRUE)

df5 <- df5 %>% mutate(Date=as.Date(Date, format="%Y-%m-%d"))

df5 <- df5 %>%
  #mutate(Date=as.Date(Date, format="%m/%d/%y")) %>%
  add_row(Date=Sys.Date(),
          Cases=sum(df$Cases),
          New=NA) %>%
  mutate(New=Cases-lag(Cases, default=2))

df5 <- df5 %>% arrange(Date)

write.table(df5,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_daily_total_pa.csv",
            sep=",",
            row.names=FALSE)

df5b <- df5 %>%
  mutate(Date2=Date+1)

write.table(df5b,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/Hub/daily_totals_pa.csv",
            sep=",",
            row.names=FALSE)

df5c <- df5 %>%
  mutate(New7=rollapply(New,7,mean,fill=0,align="right"),
         Incidence7=round(New7/(12801989/100000),1))

write.table(df5c,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/Updates/daily_cases_pa.csv",
            sep=",",
            row.names=FALSE)

df5d <- df5c %>%
  mutate(Date=Date+1)

write.table(df5c,
            "data/daily_cases_pa.csv",
            sep=",",
            row.names=FALSE)
##################################################################################
# Load and update incidence dataset
df6 <- read.table("/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_pa_incidence.csv",
                  sep=",",
                  header=TRUE)

df6 <- df6 %>% 
  arrange(County) %>%
  mutate(Cases=df$Cases,
         Incidence=round(Cases/(Population/100000),1))

write.table(df6,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_pa_incidence.csv",
            sep=",",
            row.names=FALSE)
##################################################################################
# Incidence data over time for Lehigh Valley
lvc <- c("Lehigh","Carbon","Northampton")
lv <- df4 %>% 
  filter(County %in% lvc) %>%
  group_by(County) %>% 
  mutate(New=Cases-lag(Cases, default=0))


lv <- lv %>%
  group_by(County) %>%
  mutate(New7=rollapply(New,7,mean,fill=0,align="right"),
         Incidence7=ifelse(County=="Lehigh",
                            round(New7/(368100/100000),1),
                            ifelse(County=="Northampton",
                                   round(New7/(304807/100000),1),
                                   round(New7/(64227/100000),1))))

write.table(lv,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_lv_incidence.csv",
            sep=",",
            row.names=FALSE)

lvc2 <- c("Lehigh","Northampton")
lv2 <- df4 %>% 
  filter(County %in% lvc2) %>%
  group_by(County) %>% 
  mutate(New=Cases-lag(Cases, default=0))


lv2 <- lv2 %>%
  group_by(County) %>%
  mutate(New7=rollapply(New,7,mean,fill=0,align="right"),
         Incidence7=ifelse(County=="Lehigh",
                            round(New7/(368100/100000),1),
                            round(New7/(304807/100000),1)))

write.table(lv2,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/Updates/covid19_lv.csv",
            sep=",",
            row.names=FALSE)

lv3 <- df5c %>%
  mutate(County="Pennsylvania") %>%
  select(County, Date, Cases, New, New7, Incidence7) %>%
  add_row(County=lv2$County,
          Date=lv2$Date,
          Cases=lv2$Cases,
          New=lv2$New,
          New7=lv2$New7,
          Incidence7=lv2$Incidence7)

write.table(lv3,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/Updates/covid19_lv3.csv",
            sep=",",
            row.names=FALSE)
##################################################################################
# Figure of incidence in PA and Lehigh Valley over time
pa <- df5 %>%
  mutate(New7=zoo::rollapply(New,7,mean,fill=0,align="right"),
         Incidence7=New7/(12807060/100000))

lvi <- lv %>% select(County, Date, Incidence7) %>% as.data.frame()

pai <- pa %>% mutate(County="Pennsylvania") %>% select(County, Date, Incidence7)

Data <- lvi %>% add_row(County=pai$County,
                        Date=pai$Date,
                        Incidence7=pai$Incidence7)

plot <- ggplot(data=Data, aes(x=Date,
                              y=Incidence7,
                              color=County,
                              group=County,
                              fill=County,
                              size=County,
                              linetype=County)) +
  geom_line(position="identity") +
  geom_area(position="identity", alpha=0.1, show.legend=FALSE) +
  scale_color_manual(values=c(Carbon="Brown",
                              Lehigh="Blue",
                              Northampton="Orange",
                              Pennsylvania="Black"),
                     labels=c("Carbon County",
                              "Lehigh County",
                              "Northampton County",
                              "Pennsylvania")) +
  scale_fill_manual(values=c(Carbon="Brown",
                             Lehigh="Blue",
                             Northampton="Orange",
                             Pennsylvania="Black"),
                    labels=c("Carbon County",
                             "Lehigh County",
                             "Northampton County",
                             "Pennsylvania")) +
  scale_size_manual(values=c(0.15,0.15,0.15,0.5),
                    labels=c("Carbon County",
                             "Lehigh County",
                             "Northampton County",
                             "Pennsylvania")) +
  scale_linetype_manual(values=c(1,1,1,1),
                        labels=c("Carbon County",
                                 "Lehigh County",
                                 "Northampton County",
                                 "Pennsylvania")) +
  labs(y="Avg. daily new cases per 100,000 residents\n ",
       caption="Data source: Pennsylvania Department of Health") +
  expand_limits(y=c(0,40)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_date(expand=c(0.01,0), date_breaks = "1 month", date_labels = "%b") +
  ggtitle(label="How has COVID-19 incidence changed over time in PA and the Lehigh Valley?",
          subtitle=paste("Data as of 12:00 p.m. ET", Sys.Date())) +
  theme(panel.background=element_blank(),
        panel.grid=element_blank(),
        plot.title=element_text(size= 10, hjust=0.5, color="#4e4d47", 
                                margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                   margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        strip.background=element_rect(color="black", size=0.25),
        axis.line=element_line(size=0.25),
        axis.ticks=element_line(size=0.25),
        axis.text=element_text(size=7, color="black"),
        axis.title=element_text(size=8, color="black"),
        axis.title.x=element_blank(),
        plot.caption=element_text(size=6, color="black"),
        legend.justification="top",
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.position=c(0.1,1.04),
        legend.text=element_text(size=6, color="black"),
        legend.key=element_blank(),
        legend.key.width=unit(1.2,"line"),
        legend.key.size = unit(1, 'lines')); plot

jpeg(file="/Users/jeremymack/Google Drive/R/PA_LV_cases.jpeg",
     width=7,height=4.5,
     units="in",
     res=1200)
plot
dev.off()
##################################################################################
# Figure of PA new cases over time
new <- ggplot(data=df5c, aes(x=Date, y=New7)) +
  geom_line(color="#e08f38") +
  geom_col(aes(y=New), alpha=0.3, width=0.7, fill="#e08f38") +
  labs(y="Daily new cases\n ",
       caption="Data source: Johns Hopkins University Center for Systems Science and Engineering") +
  expand_limits(y=c(0,2500)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_date(expand=c(0.01,0), date_breaks = "1 month", date_labels = "%b") +
  ggtitle(label="How have the number of new COVID-19 cases changed over time in PA?",
          subtitle=paste("Data as of 12:00 p.m. ET", Sys.Date())) +
  theme(panel.background=element_blank(),
        panel.grid=element_blank(),
        plot.title=element_text(size= 10, hjust=0.5, color="#4e4d47", 
                                margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                   margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        strip.background=element_rect(color="black", size=0.25),
        axis.line=element_line(size=0.25),
        axis.ticks=element_line(size=0.25),
        axis.text=element_text(size=7, color="black"),
        axis.title=element_text(size=8, color="black"),
        axis.title.x=element_blank(),
        plot.caption=element_text(size=6, color="black"),
        legend.justification="top",
        legend.title=element_blank(),
        legend.position=c(0.90,1.04),
        legend.text=element_text(size=6, color="black"),
        legend.key=element_blank(),
        legend.key.width=unit(1.2,"line"),
        legend.key.size = unit(1, 'lines')); new

jpeg(file="/Users/jeremymack/Google Drive/R/PA_new_cases.jpeg",
     width=7,height=4.5,
     units="in",
     res=1200)
new
dev.off()
##################################################################################
# County and State level data from Johns Hopkins CSSE
ts <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
ts <- read.csv(ts, sep=",", header=TRUE)

ts2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
ts2 <- read.csv(ts2, sep=",", header=TRUE)

ts1 <- ts %>% 
  filter(FIPS >= 1001 & FIPS <= 56045) %>%
  select(5:7, 12:ncol(ts))

ts1 <- ts1 %>% 
  gather(date, cases, 4:ncol(ts1), factor_key=TRUE) %>% 
  arrange(FIPS, date) %>% 
  group_by(FIPS) %>% 
  as.data.frame()

count <- ts1 %>% 
  group_by(FIPS) %>% 
  summarize(count=length(FIPS))

x <- 3144
dates <- data.frame(rep(seq(as.Date('2020-01-22'), 
                            as.Date('2020-01-22') + as.numeric(count[1,2]-1),
                            by = 'days'),
                        times = x))
names(dates) <- "date"

ts2 <- ts2 %>% 
  filter(FIPS >= 1001 & FIPS <= 56045) %>%
  select(5:7, 13:ncol(ts2))

ts2 <- ts2 %>% 
  gather(date, deaths, 4:ncol(ts2), factor_key=TRUE) %>% 
  arrange(FIPS, date) %>% 
  group_by(FIPS) %>% 
  as.data.frame()

df.cases <- cbind(ts1, dates)
df.cases <- df.cases[,c(1:3,6,5)]
names(df.cases)[1:3] <- c("fips","admin2","state")

df.deaths <- cbind(ts2, dates)
df.deaths <- df.deaths[,c(1:3,6,5)]
names(df.deaths)[1:3] <- c("fips","admin2","state")

df.us <- df.cases %>%
  mutate(deaths=df.deaths$deaths)

df.us <- df.us %>%
  mutate(fips = ifelse(fips==46102,46113,fips)) %>%
  mutate(fips = ifelse(fips==2158,2270,fips))

df.county <- df.us %>%
  group_by(state, admin2, fips) %>%
  filter(row_number()==n()) %>%
  arrange(state, admin2) %>%
  mutate(date=date+2)

df.county <- tibble::rowid_to_column(df.county, "id")


write.table(df.county,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/Updates/covid19_county_data2.csv",
            sep=",",
            row.names=FALSE)

FID <- c(21, 36, 42, 24, 49, 43, 5, 3, 1, 22, 26, 4, 41, 28, 13, 29, 34, 15, 17, 18, 8, 7,
         33, 44, 19, 32, 50, 35, 47, 9, 6, 45, 27, 23, 40, 20, 30, 48, 25, 2, 12, 39, 14,
         51, 37, 10, 16, 38, 11, 31, 46)

df.state <- df.county %>%
  group_by(state, date) %>%
  summarize(Confirmed=sum(cases),
            Deaths=sum(deaths)) %>%
  arrange(state) %>%
  add_column(as.integer(FID))

names(df.state)[5] <- "FID"
  
write.table(df.state,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/Updates/covid19_state_data.csv",
            sep=",",
            row.names=FALSE)
