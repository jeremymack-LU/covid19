library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(rgeos)
library(tidyverse)
library(zoo)
library(patchwork)

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

df.us2 <- merge(df.us2, pop, by="state")

df.us2 <- df.us2 %>% mutate(pop100=population/100000,
                            new7b=round(new7/pop100,1))

spdf_fortified2 <- spdf_fortified %>%
  left_join(. , df.us2, by=c("id"="state"))

spdf_fortified2$bin <- cut(spdf_fortified2$new7b,
                           breaks=c(-20,0,0.9,9,24.9,100),
                           labels=c("X",
                                    "A",
                                    "B",
                                    "C",
                                    "D"))

cols <- c("X"="lightgray","A"="#197d7d", "B"="#dbc037", "C"="#e08f38", "D"="#8C1111")

usa <- ggplot() +
  geom_polygon(data=spdf_fortified2, aes(x=long, y=lat, fill=bin, group=group), color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id), col="white", size=2.5) +
  theme_void() +
  coord_map() +
  scale_fill_manual(values=cols) +
  theme(
    legend.position="none",
    text=element_text(color="#22211d"),
    plot.background=element_rect(fill="white", color=NA), 
    panel.background=element_rect(fill="white", color=NA), 
    plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                            margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                               margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))) +
  ggtitle(label="Which Places Have The Most New Daily Cases?",
          subtitle=paste("Data as of 11:59 p.m. ET", Sys.Date()-1))

leg <- ggplot() +
  annotate(geom="rect", xmin=0, xmax=2, ymin=1.8, ymax=2, color=NA, fill="#8C1111") +
  annotate(geom="text", label="RED", x=1, y=1.9, color="white", size=3) +
  annotate(geom="text",
           label="Threshold:",
           x=0, hjust=0, y=1.75, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                    25+ daily\nnew cases per 100,000\npeople",
           x=0, hjust=0, y=1.75, vjust=1, size=2.5) +
  annotate(geom="text",
           label="Indicates:",
           x=0, hjust=0, y=1.4, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                  unchecked\ncommunity spread",
           x=0, hjust=0, y=1.4, vjust=1, size=2.5) +
  annotate(geom="rect", xmin=2.5, xmax=4.5, ymin=1.8, ymax=2, color=NA, fill="#e08f38") +
  annotate(geom="text", label="ORANGE", x=3.5, y=1.9, color="white", size=3) +
  annotate(geom="text",
           label="Threshold:",
           x=2.5, hjust=0, y=1.75, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                    10-24 daily\nnew cases per 100,000\npeople",
           x=2.5, hjust=0, y=1.75, vjust=1, size=2.5) +
  annotate(geom="text",
           label="Indicates:",
           x=2.5, hjust=0, y=1.4, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                  escalating\ncommunity spread",
           x=2.5, hjust=0, y=1.4, vjust=1, size=2.5) +
  annotate(geom="rect", xmin=5, xmax=7, ymin=1.8, ymax=2, color=NA, fill="#dbc037") +
  annotate(geom="text", label="YELLOW", x=6, y=1.9, color="white", size=3) +
  annotate(geom="text",
           label="Threshold:",
           x=5, hjust=0, y=1.75, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                    1-9 daily\nnew cases per 100,000\npeople",
           x=5, hjust=0, y=1.75, vjust=1, size=2.5) +
  annotate(geom="text",
           label="Indicates:",
           x=5, hjust=0, y=1.4, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                  potential\ncommunity spread",
           x=5, hjust=0, y=1.4, vjust=1, size=2.5) +
  annotate(geom="rect", xmin=7.5, xmax=9.5, ymin=1.8, ymax=2, color=NA, fill="#197d7d") +
  annotate(geom="text", label="GREEN", x=8.5, y=1.9, color="white", size=3) +
  annotate(geom="text",
           label="Threshold:",
           x=7.5, hjust=0, y=1.75, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                    < 1 daily\nnew cases per 100,000\npeople",
           x=7.5, hjust=0, y=1.75, vjust=1, size=2.5) +
  annotate(geom="text",
           label="Indicates:",
           x=7.5, hjust=0, y=1.4, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                  close to\ncontainment",
           x=7.5, hjust=0, y=1.4, vjust=1, size=2.5) +
  scale_y_continuous(limits=c(1,2)) +
  theme_void() +
  theme(
    text=element_text(color="#22211d"),
    plot.background=element_rect(fill="white", color=NA), 
    panel.background=element_rect(fill="white", color=NA), 
    legend.background=element_rect(fill="white", color=NA)
  )
  

jpeg(file="/Users/jeremymack/Documents/Lehigh/R/gogs/covid19/USA_cases2.jpeg",
     width=7,height=4.75,
     units="in",
     res=1200)
usa / leg + plot_layout(heights = c(2, 1))
dev.off()

jpeg(file="/Users/jeremymack/Google Drive/R/USA_cases2.jpeg",
     width=7,height=4.75,
     units="in",
     res=1200)
usa / leg + plot_layout(heights = c(2, 1))
dev.off()
