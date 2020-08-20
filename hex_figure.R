library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(rgeos)
library(tidyverse)
library(zoo)

pop <- read.table("population.csv", sep=",", header=TRUE)
pop <- pop %>% mutate(population=as.numeric(gsub(",","",population)))

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

df.us <- ts2 %>%
  gather(date, cases, 2:ncol(ts2), factor_key=TRUE)

x <- 51
dates <- data.frame(rep(
  seq(as.Date('2020-01-22'), as.Date('2020-01-22')+(ncol(ts)-12), by = 'days'), 
  times = x))
names(dates) <- "date"
dates <- dates %>% arrange(date)
df.us <- cbind(df.us, dates); df.us <- df.us[,c(1,4,3)]; names(df.us)[1] <- "state"

df.us <- df.us %>%
  #filter(date>=as.Date("2020-03-01")) %>%
  arrange(state,date) %>%
  group_by(state) %>%
  mutate(new=cases-lag(cases, default=0)) %>%
  mutate(new7=rollapply(new,7,mean,fill=0,align="right")) %>%
  mutate(change=round(((new7-lag(new7,14,0))/lag(new7,14,0))*100))

# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

df.us2 <- df.us %>% filter(date==Sys.Date()-1)

spdf_fortified2 <- spdf_fortified %>%
  left_join(. , df.us2, by=c("id"="state"))

spdf_fortified2$bin <- cut(spdf_fortified2$change,
                           breaks=c(-100,-50,-5,5,50,100,1000),
                           labels=c("-100%",
                                    "-50%",
                                    "-5%",
                                    "+5%",
                                    "+50%",
                                    "+100%"))

cols <- c("-100%"="#25108C", "-50%"="#1619D5", "-5%"="#837BFB",
          "+100%"="#8C1111", "+50%"="#D51616", "+5%"="#FB7B7B")

usa <- ggplot() +
  geom_polygon(data=spdf_fortified2, aes(x=long, y=lat, fill=bin, group=group), color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id), col="white") +
  theme_void() +
  coord_map() +
  scale_fill_manual(values=cols) +
  theme(
    legend.position=c(0.5, 0.88),
    legend.direction="horizontal",
    legend.title=element_text(size= 8, hjust=0.5, color="#4e4d47", face="bold",
                              margin=margin(b = -0.1, t = 0.1, unit = "cm")),
    text=element_text(color="#22211d"),
    plot.background=element_rect(fill="#f5f5f2", color=NA), 
    panel.background=element_rect(fill="#f5f5f2", color=NA), 
    legend.background=element_rect(fill="#f5f5f2", color=NA),
    plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                            margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                               margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))) +
  ggtitle(label="Where New Cases Are Going Up, Leveling Off, Or\nGoing Down",
          subtitle=paste("Data as of 11:59 p.m. ET", Sys.Date()-1)) +
  labs(fill="NEW DAILY CASES VS. 2 WEEKS AGO") +
  guides(fill=guide_legend(nrow=1,
                           label.position="bottom",
                           label.hjust=-0.5,
                           keywidth=2.5,
                           keyheight=0.75,
                           title.position="top")); usa

jpeg(file="/Users/jeremymack/Documents/Lehigh/R/gogs/covid19/USA_cases.jpeg",
     width=7,height=4.75,
     units="in",
     res=1200)
usa
dev.off()

