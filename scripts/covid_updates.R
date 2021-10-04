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
  mutate(New7=rollapply(New,7,mean,fill=0,align="right"),
         Incidence7=round(New7/(12801989/100000),1)) %>%
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
         New7=rollapply(New,7,mean,fill=0,align="right"),
         Incidence7=case_when(
           County=="Lehigh" ~ round(New7/(368100/100000),1),
           TRUE ~ round(New7/(304807/100000),1)
         )) %>%
  tibble()

write_csv(df.lv,"data/covid19_lv.csv")

df.pa.sum <- df.pa.sum %>%
  mutate(County="Pennsylvania") %>%
  relocate(County)

df.lv <- df.lv %>%
  add_row(df.pa.sum)

plot <- ggplot(data=df.lv, aes(x=Date,
                              y=Incidence7,
                              color=County,
                              group=County,
                              fill=County,
                              size=County,
                              linetype=County)) +
  geom_line(position="identity") +
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
  labs(y="Avg. daily new cases per 100,000 residents\n ",
       caption="Data source: Pennsylvania Department of Health") +
  expand_limits(y=c(0,40)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,140), breaks=seq(0,140,20)) +
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

jpeg(file="/Users/jeremymack/Google Drive/Lehigh/R/PA_LV_cases.jpeg",
     width=7,height=4.5,
     units="in",
     res=1200)
plot
dev.off()

# Figure of PA new cases over time ----------------------------------------
new <- ggplot(data=df.pa.sum, aes(x=Date, y=New7)) +
  geom_line(color="#e08f38") +
  geom_col(aes(y=New), alpha=0.3, width=0.7, fill="#e08f38") +
  labs(y="Daily new cases\n ",
       caption="Data source: Johns Hopkins University Center for Systems Science and Engineering") +
  #expand_limits(y=c(0,2500)) +
  expand_limits(y=c(0,15000)) +
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

jpeg(file="/Users/jeremymack/Google Drive/Lehigh/R/PA_new_cases.jpeg",
     width=7,height=4.5,
     units="in",
     res=1200)
new
dev.off()

# Update files on Github --------------------------------------------------

source("scripts/git.R")

# Set working directory for use in git functions
dir <- "/Users/jeremymack/Documents/GitHub/covid19"

gitstatus()
gitadd()
gitcommit()
gitpush()
