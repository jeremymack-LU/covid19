library("rvest")
library("tidyverse")
library("zoo")
library("pdftools")
library("maps")
library("readr")
library("sf")
library("albersusa")
library("viridis")
library("geojsonio")
library("broom")
library("rgeos")
library("patchwork")

ts <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
ts <- read.csv(ts, sep=",", header=TRUE)
pop <- read.table("population.csv", sep=",", header=TRUE)
pop <- pop %>% mutate(population=as.numeric(gsub(",","",population)))
pa.pop <- read.table("pa_population.csv", sep=",", header=TRUE); names(pa.pop)[1] <- "admin2"

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

df.us <- cbind(ts1, dates)
df.us <- df.us[,c(1:3,6,5)]
names(df.us)[1:3] <- c("fips","admin2","state")

df.us <- df.us %>%
  mutate(fips = ifelse(fips==46102,46113,fips)) %>%
  mutate(fips = ifelse(fips==2158,2270,fips))

df.us2 <- df.us %>%
  group_by(date) %>%
  summarise(cases=sum(cases)) %>%
  mutate(new=cases-lag(cases, default=0)) %>%
  mutate(new7=rollapply(new,7,mean,fill=0,align="right"))

df.states <- df.us %>%
  group_by(date, state) %>%
  summarize(cases=sum(cases)) %>%
  arrange(state, date) %>%
  as.data.frame()

df.states <- df.states %>% 
  group_by(state) %>%
  mutate(new=cases-lag(cases, default=0)) %>%
  mutate(new7=rollapply(new,7,mean,fill=0,align="right"))

df.states <- df.states %>%
  group_by(state) %>%
  slice(n()) %>%
  left_join(., pop, by="state") %>%
  mutate(pop100=population/100000,
         new7b=round(new7/pop100,1))

df.pa <- df.us %>% 
  filter(state=="Pennsylvania") %>%
  group_by(admin2) %>%
  mutate(new=cases-lag(cases, default=0)) %>%
  mutate(new7=rollapply(new,7,mean,fill=0,align="right"))

df.sub1 <- df.pa %>% filter(date==Sys.Date()-1)
df.sub1 <- left_join(df.sub1, pa.pop, by="admin2")

df1 <- df.sub1 %>% mutate(pop100=population/100000,
                          new7b=round(new7/pop100,1))

cty_sf <- counties_sf("longlat")
cty_sf <- cty_sf %>% mutate(fips=as.integer(stringr::str_remove(fips, "^0+")))
cty_sf2 <- cty_sf %>% right_join(df1, by = "fips")

cty_sf2$bin <- cut(cty_sf2$new7b,
                   breaks=c(-0.1,0.9,9.9,24.9,100),
                   labels=c("A",
                            "B",
                            "C",
                            "D"))

cols <- c("A"="#197d7d", "B"="#dbc037", "C"="#e08f38", "D"="#8C1111")

p2 <- ggplot() +
  geom_sf(data=cty_sf2, aes(fill=bin),
          size=0.25, color="white") +
  scale_fill_manual(values=cols) +
  theme_void() +
  theme(
    legend.position="none",
    text=element_text(color="#22211d"),
    plot.background=element_rect(fill="white", color=NA), 
    panel.background=element_rect(fill="white", color=NA), 
    plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                            margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                               margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

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


# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

spdf_fortified2 <- spdf_fortified %>%
  left_join(. , df.states, by=c("id"="state"))

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
                               margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

(usa + p2) / leg + plot_layout(heights = c(2, 1)) + plot_annotation(
  title='Which Places Have The Most New Daily Cases?',
  subtitle=paste("Data as of 11:59 p.m. ET", Sys.Date()-1)
) & theme(plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                                  margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                     margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

jpeg(file="/Users/jeremymack/Google Drive/R/USA_cases2.jpeg",
     width=7,height=4.75,
     units="in",
     res=1200)
(usa + p2) / leg + plot_layout(heights = c(2, 1)) + plot_annotation(
  title='Which Places Have The Most New Daily Cases?',
  subtitle=paste("Data as of 11:59 p.m. ET", Sys.Date()-1)
) & theme(plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                                  margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                     margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))
dev.off()
