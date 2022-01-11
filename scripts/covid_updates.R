# Load packages -----------------------------------------------------------
pacman::p_load(tidyverse,git2r,zoo)

# US time series data -----------------------------------------------------
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
  mutate(date=date+1) %>%
  tibble()

df.county <- df.county %>%
  rename(date_report=date,
         county=admin2)

write_csv(df.county,"data/df_county.csv")

df.pa <- df.us %>%
  filter(state=="Pennsylvania") %>%
  select(admin2,date,cases,deaths) %>%
  group_by(admin2) %>%
  arrange(admin2,date) %>%
  rename(County=admin2,
         Date=date,
         Cases=cases,
         Deaths=deaths) %>%
  tibble()

df.pa.sum <- df.pa %>%
  group_by(Date) %>%
  summarize(Cases=sum(Cases,na.rm=TRUE)) %>%
  mutate(Date=Date+1,
         New=Cases-lag(Cases, default=0)) %>%
  mutate(New14=rollapply(New,14,mean,fill=0,align="right"),
         Incidence14=round(New14/(12801989/100000),1)) %>%
  tibble()

write_csv(df.pa.sum,"data/daily_cases_pa.csv")

# PA and Lehigh Valley plot of incidence time series ----------------------
lv.counties <- c("Lehigh","Northampton")

df.lv <- df.pa %>% 
  filter(County %in% lv.counties) %>%
  group_by(County) %>% 
  mutate(New=Cases-lag(Cases, default=0))

df.lv <- df.lv %>%
  group_by(County) %>%
  mutate(Date=Date+1,
         New14=rollapply(New,14,mean,fill=0,align="right"),
         Incidence14=case_when(
           County=="Lehigh" ~ round(New14/(368100/100000),1),
           TRUE ~ round(New14/(304807/100000),1)
         )) %>%
  tibble()

write_csv(df.lv,"data/covid19_lv.csv")

df.pa.sum <- df.pa.sum %>%
  mutate(County="Pennsylvania") %>%
  relocate(County)

df.lv <- df.lv %>%
  add_row(df.pa.sum)

theme <- theme(panel.background=element_blank(),
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
               legend.position=c(0.5,0.96),
               legend.direction='horizontal',
               legend.text=element_text(size=6, color="black"),
               legend.key=element_blank(),
               legend.key.width=unit(1.2,"line"),
               legend.key.size = unit(1, 'lines'))

plot <- ggplot(data=df.lv,
       aes(x=Date,
           y=Incidence14,
           color=County,
           group=County,
           fill=County,
           size=County,
           linetype=County)) +
  geom_line(position="identity", size=0.35, alpha=0.7) +
  geom_area(position="identity", alpha=0.1, show.legend=FALSE) +
  scale_color_manual(values=c(Lehigh="Blue",
                              Northampton="Orange",
                              Pennsylvania="Black"),
                     labels=c("Lehigh County",
                              "Northampton County",
                              "Pennsylvania")) +
  scale_fill_manual(values=c(Carbon="Brown",
                             Lehigh="Blue",
                             Northampton="Orange",
                             Pennsylvania="Black"),
                    labels=c("Lehigh County",
                             "Northampton County",
                             "Pennsylvania")) +
  scale_size_manual(values=c(0.15,0.15,0.15,0.5),
                    labels=c("Lehigh County",
                             "Northampton County",
                             "Pennsylvania")) +
  scale_linetype_manual(values=c(1,1,1,1),
                        labels=c("Lehigh County",
                                 "Northampton County",
                                 "Pennsylvania")) +
  labs(y="Daily new cases per 100,000 residents (14-day avg.)\n ",
       caption="\nData source: Pennsylvania Department of Health") +
  scale_y_continuous(expand=c(0,0), limits=c(0,260), breaks=seq(0,260,20)) +
  scale_x_date(expand=c(0.01,0), date_breaks = "1 month", date_labels = "%b\n'%y") +
  ggtitle(label="How has COVID-19 incidence changed over time in PA and the Lehigh Valley?",
          subtitle=paste("Data as of 6:00 a.m. ET", Sys.Date())) +
  theme; plot

jpeg(file="output/PA_LV_cases.jpeg",
     width=7,height=4.5,
     units="in",
     res=1200)
plot
dev.off()

# Lehigh Valley hospitalizations ------------------------------------------
beds.L <- read_csv('https://data.pa.gov/resource/kayn-sjhx.csv?county=Lehigh')
beds.N <- read_csv('https://data.pa.gov/resource/kayn-sjhx.csv?county=Northampton')
beds   <- beds.L %>% add_row(beds.N)

beds.total <- beds %>% 
  group_by(date) %>% 
  summarize(total=sum(covid_patients_mean,na.rm=TRUE))

write_csv(beds.total,"data/hospitalizations.csv")

hosp <- beds.total %>%
  ggplot(aes(as.Date(date),total)) +
  geom_line(position="identity", size=0.35, alpha=0.7) +
  geom_area(position="identity", alpha=0.1, show.legend=FALSE) +
  labs(y="Number of COVID-19 patients hospitalized (14-day avg.)\n ",
       caption="\nData source: Pennsylvania Department of Health") +
  scale_y_continuous(expand=c(0,0), limits=c(0,500), breaks=seq(0,500,50)) +
  scale_x_date(expand=c(0.01,0),
               date_breaks = "1 month",
               date_labels = "%b\n'%y") +
  ggtitle(label="How have hospitalizations from COVID-19 changed over time in the Lehigh Valley?",
          subtitle=paste("Data as of 12:00 p.m. ET", Sys.Date())) +
  theme; hosp

jpeg(file="output/LV_hospitalizations.jpeg",
     width=7,height=4.5,
     units="in",
     res=1200)
hosp
dev.off()

# Figure of PA new cases over time ----------------------------------------
new <- ggplot(data=df.pa.sum, aes(x=Date, y=New14)) +
  geom_col(aes(y=New), alpha=0.7, width=0.7, fill="gray") +
  geom_line(color="#e08f38", size=0.5, alpha=0.7) +
  labs(y="Daily new cases\n ",
       caption="\nData source: Pennsylvania Department of Health") +
  #expand_limits(y=c(0,30000)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,35000), breaks=seq(0,35000,5000)) +
  scale_x_date(expand=c(0.01,0), date_breaks = "1 month", date_labels = "%b\n'%y") +
  ggtitle(label="How have the number of new COVID-19 cases changed over time in PA?",
          subtitle=paste("Data as of 6:00 a.m. ET", Sys.Date())) +
  theme; new

jpeg(file="output/PA_new_cases.jpeg",
     width=7,height=4.5,
     units="in",
     res=1200)
new
dev.off()


# Update files on Google Drive --------------------------------------------
library(googledrive)

drive_auth(
  email="jsm4@lehigh.edu",
  path="data/client_secret_1055497976267-6fk2msjr8u13ldumbkfsu346rrd2tvo8.apps.googleusercontent.com.json"
)

drive_update(as_id('1h5bqK5doNf2MsleGsyXrzVh297yGLcR9'), 'output/PA_LV_cases.jpeg')

drive_update(as_id('1NNP5cpheokl4rwAVLtiKriWIWvU3oG2J'), 'output/PA_new_cases.jpeg')

drive_update(as_id('1NKnjpgc3g0UE6Ftroslm1AbttHzKAF0G'), 'output/LV_hospitalizations.jpeg')

# Update files on Github --------------------------------------------------
source("scripts/git.R")

# Set working directory for use in git functions
dir <- "/Users/jeremymack/Documents/GitHub/covid19"

gitstatus()
gitadd()
gitcommit()
gitpush()
