library("rvest")
library("tidyverse")
library("zoo")
library("pdftools")
library("maps")

devtools::install_github("yonghah/esri2sf")
library("esri2sf")

url <- 'https://services2.arcgis.com/xtuWQvb2YQnp0z3F/arcgis/rest/services/Pennsylvania_Public_COVID19_Dashboard_Data/FeatureServer/0'

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

##################################################################################
# Load and update dataset1
df2 <- read.table("/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_pa_data-update.csv",
                  sep=",",
                  header=TRUE)

df2 <- df2 %>% mutate(Covid_cases=df$Covid_cases,
                      Covid_deaths=df$Covid_deaths)

write.table(df2,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_pa_data-update.csv",
            sep=",",
            row.names=FALSE)
##################################################################################
# Load and update dataset2
df3 <- read.table("/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_pa_data.csv",
                  sep=",",
                  header=TRUE)

df3 <- df3 %>% mutate(COVID19_cases=df$Covid_cases,
                      COVID19_deaths=df$Covid_deaths)

write.table(df3,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_pa_data.csv",
            sep=",",
            row.names=FALSE)
##################################################################################
# Load and update dataset4
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
  mutate(New14=rollapply(New,14,sum,fill=0,align="right"),
         Incidence14=ifelse(County=="Lehigh",
                            round(New14/(368100/100000),1),
                            ifelse(County=="Northampton",
                                   round(New14/(304807/100000),1),
                                   round(New14/(64227/100000),1))))

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

pa <- df5 %>%
  mutate(New14=zoo::rollapply(New,14,sum,fill=0,align="right"),
         Incidence14=New14/(12807060/100000))

lvi <- lv %>% select(County, Date, Incidence14) %>% as.data.frame()

pai <- pa %>% mutate(County="Pennsylvania") %>% select(County, Date, Incidence14)

Data <- lvi %>% add_row(County=pai$County,
                        Date=pai$Date,
                        Incidence14=pai$Incidence14)

plot <- ggplot(data=Data, aes(x=Date,
                              y=Incidence14,
                              color=County,
                              group=County,
                              fill=County,
                              size=County,
                              linetype=County)) +
  geom_hline(yintercept=50, linetype=3) +
  geom_vline(xintercept=as.Date("2020-04-14"), linetype=3, color="brown") +
  geom_line(position="identity") +
  geom_area(position="identity", alpha=0.1, show.legend=FALSE) +
  annotate(geom="segment",
           xend=as.Date("2020-04-14") + 0.25,
           x=as.Date("2020-04-20"),
           y=436,
           yend=436,
           size=0.25,
           arrow = arrow(length = unit(0.01, "npc"))) +
  annotate(geom="text", x=as.Date("2020-04-20") + 0.4, y=436, 
           label="Two weeks, following Stay at Home Order", 
           hjust=0, size=2) +
  annotate(geom="segment",
           x=as.Date("2020-03-05")+0.15,
           xend=as.Date("2020-03-05")+0.15,
           yend=52,
           y=120,
           size=0.25,
           arrow = arrow(length = unit(0.01, "npc"))) +
  annotate(geom="text", x=as.Date("2020-03-05"), y=150,
           label="Benchmark goal\nof 50 new cases\nper 100,000 residents",
           hjust=0, size=2) +
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
  scale_size_manual(values=c(0.25,0.25,0.25,0.5),
                    labels=c("Carbon County",
                             "Lehigh County",
                             "Northampton County",
                             "Pennsylvania")) +
  scale_linetype_manual(values=c(1,1,1,2),
                        labels=c("Carbon County",
                                 "Lehigh County",
                                 "Northampton County",
                                 "Pennsylvania")) +
  labs(y="New cases per 100,000 residents in 14 days\n ",
       caption="Data source: PA Department of Health") +
  expand_limits(y=c(0,450)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.background=element_blank(),
        panel.grid=element_blank(),
        plot.title=element_text(size=9, color="black"),
        strip.background=element_rect(color="black", size=0.25),
        axis.line=element_line(size=0.25),
        axis.ticks=element_line(size=0.25),
        axis.text=element_text(size=7, color="black"),
        axis.title=element_text(size=8, color="black"),
        axis.title.x=element_blank(),
        plot.caption=element_text(size=6, color="black"),
        legend.justification="top",
        legend.title=element_blank(),
        legend.position=c(0.11,1.04),
        legend.text=element_text(size=6, color="black"),
        legend.key=element_blank(),
        legend.key.width=unit(1.2,"line"),
        legend.key.size = unit(1, 'lines')); plot

jpeg(file="/Users/jeremymack/Documents/Lehigh/R/gogs/covid19/PA_LV_cases.jpeg",
     width=7,height=3.5,
     units="in",
     res=600)
plot
dev.off()
##################################################################################

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

x <- 3142
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
  arrange(state, admin2)

df.county <- tibble::rowid_to_column(df.county, "id")


write.table(df.county,
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_county_data2.csv",
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
            "/Users/jeremymack/Documents/Lehigh/GIS/Projects/COVID19/covid19_state_data.csv",
            sep=",",
            row.names=FALSE)

