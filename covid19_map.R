library(dplyr)
library(ggplot2)
library(usmap)

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-03-2020.csv"
ts <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

df <- read.csv(url, sep=",", header=TRUE)
df.us <- df %>% filter(Country_Region=="US") %>% as.data.frame()

ts <- read.csv(ts, sep=",", header=TRUE)
ts <- ts %>% group_by(Province_State) %>% select(7,12:ncol(.)) %>% summarise_all(funs(sum))

remove <- c("Guam", 
            "Puerto Rico", 
            "Virgin Islands",
            "United States Virgin Islands", 
            "US", 
            "Grand Princess", 
            "Diamond Princess",
            "American Samoa",
            "Wuhan Evacuee",
            "Northern Mariana Islands",
            "Recovered")

df.us <- df.us %>% 
  filter(!Province_State %in% remove) %>% 
  group_by(Province_State) %>% 
  summarize(cases=sum(Confirmed)) %>% 
  as.data.frame()

names(df.us)[1] <- "full"

df.center <- state.center %>%
  as.data.frame() %>%
  mutate(lon=x,
         lat=y,
         full=state.name) %>%
  select(lon,lat,full)

df.center <- merge(df.center, df.us, by="full", all.y=TRUE)
df.center[2,2] <- -152.47
df.center[2,3] <- 64.731667
df.center[9,2] <- -77.016389
df.center[9,3] <- 38.904722
df.center[12,2] <- -155.5
df.center[12,3] <- 19.566667

df.center <- df.center[,c(2:4,1)]
df.center2 <- df.center %>% filter(full != "New York")
df.center3 <- df.center %>% filter(full == "New York")

df.center.T <- usmap_transform(df.center)
df.center2.T <- usmap_transform(df.center2)
df.center3.T <- usmap_transform(df.center3)

p <- plot_usmap(size=0.1) +
  geom_point(data = df.center2.T,
             aes(x = lon.1, y = lat.1, size = cases),
             color = "red", alpha = 0.25) +
  scale_size_area(max_size=10) +
  geom_point(data = df.center3.T,
             aes(x = lon.1, y = lat.1),
             color = "red", alpha = 0.25, size=15) +  
  labs(title = "US COVID-19 Cases",
       subtitle = paste("Source: CDC, ", Sys.Date(), sep="")) +
  theme(legend.position = "right")

ggplotly(p)

ts <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

ts <- read.csv(ts, sep=",", header=TRUE)
ts2 <- ts %>% group_by(Province_State) %>% select(7,12:ncol(ts)) %>% summarise_all(funs(sum))

remove <- c("Guam", 
            "Puerto Rico", 
            "Virgin Islands",
            "United States Virgin Islands", 
            "US", 
            "Grand Princess", 
            "Diamond Princess",
            "American Samoa",
            "Wuhan Evacuee",
            "Northern Mariana Islands",
            "Recovered")

ts2 <- ts2 %>% 
  filter(!Province_State %in% remove) %>% 
  as.data.frame()

df <- ts2 %>%
  gather(date, cases, 2:ncol(ts2), factor_key=TRUE)

x <- 51
dates <- data.frame(rep(
  seq(as.Date('2020-01-22'), as.Date('2020-01-22')+(ncol(ts)-12), by = 'days'), 
  times = x))
names(dates) <- "date"
dates <- dates %>% arrange(date)

df <- cbind(df, dates); df <- df[,c(1,4,3)]; names(df)[1] <- "full"

df.center <- state.center %>%
  as.data.frame() %>%
  mutate(lon=x,
         lat=y,
         full=state.name) %>%
  select(lon,lat,full)

df <- merge(df,df.center,by="full",all.x=TRUE)

df <- df %>% mutate(lon=ifelse(full=="Alaska",-152.47,
                               ifelse(full=="District of Columbia",-77.016389,
                                      ifelse(full=="Hawaii",-155.5,lon))))

df <- df %>% mutate(lat=ifelse(full=="Alaska",64.731667,
                               ifelse(full=="District of Columbia",38.904722,
                                      ifelse(full=="Hawaii",19.566667,lat))))

ts.PA <- ts %>% filter(Province_State == "Pennsylvania")
ts.PA2 <- ts.PA %>% select(5,12:ncol(ts.PA))

remove2 <- c(80042, 90042)

ts.PA2 <- ts.PA2 %>% 
  filter(!FIPS %in% remove2) %>% 
  as.data.frame()

df.PA <- ts.PA2 %>%
  gather(date, cases, 2:ncol(ts.PA2), factor_key=TRUE)

x2 <- 67
dates2 <- data.frame(rep(
  seq(as.Date('2020-01-22'), as.Date('2020-01-22')+(ncol(ts)-12), by = 'days'), 
  times = x2))
names(dates2) <- "date"
dates2 <- dates2 %>% arrange(date)

df.PA <- cbind(df.PA, dates2); df.PA <- df.PA[,c(1,4,3)]
names(df.PA)[1] <- "fips"


  