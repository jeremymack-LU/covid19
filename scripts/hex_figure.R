# Description -------------------------------------------------------------
# Code for creating figure of COVID-19 incidence in US and PA counties
# Updated: Updated June 10, 2021

# Load packages -----------------------------------------------------------
pacman::p_load(rvest,tidyverse,zoo,pdftools,maps,readr,sf,albersusa,
               viridis,geojsonio,broom,rgeos,patchwork,mapproj)

# Load data ---------------------------------------------------------------
# Time series data from Johns Hopkins CSSE github account
ts <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
ts <- read.csv(ts, sep=",", header=TRUE)
# Load US and PA population data
pop <- read.table("data/population.csv", sep=",", header=TRUE)
pop <- pop %>% mutate(population=as.numeric(gsub(",","",population)))
pa.pop <- read.table("data/pa_population.csv", sep=",", header=TRUE); names(pa.pop)[1] <- "admin2"

# Create dataframes for plotting ------------------------------------------
# Subset to include 50 states and DC
ts1 <- ts %>% 
  filter(FIPS >= 1001 & FIPS <= 56045) %>%
  select(5:7, 12:ncol(ts))

# Convert from wide to long format
ts1 <- ts1 %>% 
  gather(date, cases, 4:ncol(ts1), factor_key=TRUE) %>% 
  arrange(FIPS, date) %>% 
  group_by(FIPS) %>% 
  as.data.frame()

# Summarize and count the number of counties
count <- ts1 %>% 
  group_by(FIPS) %>% 
  summarize(count=length(FIPS))
x <- nrow(count)

# Create a data frame of dates
dates <- data.frame(rep(seq(as.Date('2020-01-22'), 
                            as.Date('2020-01-22') + as.numeric(count[1,2]-1),
                            by = 'days'),
                        times = x))
names(dates) <- "date"

# Combine the two data frames (dates and cases)
df.us <- cbind(ts1, dates)
df.us <- df.us[,c(1:3,6,5)]
names(df.us)[1:3] <- c("fips","admin2","state")

# Update two fips codes to match
df.us <- df.us %>%
  mutate(fips=ifelse(fips==46102,46113,fips)) %>%
  mutate(fips=ifelse(fips==2158,2270,fips))

# df.us2 <- df.us %>%
#   group_by(date) %>%
#   summarise(cases=sum(cases)) %>%
#   mutate(new=cases-lag(cases, default=0)) %>%
#   mutate(new7=rollapply(new,7,mean,fill=0,align="right"))

# Create state data frame
df.states <- df.us %>%
  group_by(date, state) %>%
  summarize(cases=sum(cases)) %>%
  arrange(state, date) %>%
  as.data.frame()

# Calculate new cases and average new cases over 7 days
df.states <- df.states %>% 
  group_by(state) %>%
  mutate(new=cases-lag(cases, default=0)) %>%
  mutate(new7=rollapply(new,7,mean,fill=0,align="right")) %>%
  # Select most recent date
  slice(n()) %>%
  # Add population data
  left_join(., pop, by="state") %>%
  # Calculate new cases per capita
  mutate(pop100=population/100000,
         new7b=round(new7/pop100,1))

# Create Pennsylvania data frame
df.pa <- df.us %>% 
  filter(state=="Pennsylvania") %>%
  group_by(admin2) %>%
  mutate(new=cases-lag(cases, default=0)) %>%
  mutate(new7=rollapply(new,7,mean,fill=0,align="right")) %>%
  # Select most recent date
  slice(n()) %>%
  # Add population data
  left_join(., pa.pop, by="admin2") %>%
  # Calculate new cases per capita
  mutate(pop100=population/100000,
         new7b=round(new7/pop100,1))

# df.sub1 <- df.pa %>% filter(date==Sys.Date()-1)
# df.sub1 <- left_join(df.sub1, pa.pop, by="admin2")
# 
# df1 <- df.sub1 %>% mutate(pop100=population/100000,
#                           new7b=round(new7/pop100,1))

cty.sf <- counties_sf("longlat")

cty.sf <- cty.sf %>% 
  mutate(fips=as.integer(stringr::str_remove(fips, "^0+"))) %>%
  right_join(.,df.pa, by = "fips") %>%
  mutate(bin=cut(new7b,
                 breaks=c(-200,0.9,9.9,24.9,1050),
                 labels=c("A","B","C","D")))

#cty_sf2 <- cty_sf %>% right_join(df.pa, by = "fips")

# cty_sf2$bin <- cut(cty_sf2$new7b,
#                    breaks=c(-200,0.9,9.9,24.9,1050),
#                    labels=c("A","B","C","D"))

cols <- c("A"="#197d7d", "B"="#dbc037", "C"="#e08f38", "D"="#8C1111")

# Pennsylvania plot - incidence by county ---------------------------------
pa <- ggplot() +
  geom_sf(data=cty.sf, aes(fill=bin),
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

# US hex plot - incidence by state ----------------------------------------
# Load us hexgrid json file
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

# Bit of reformatting
spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

spdf_fortified2 <- spdf_fortified %>%
  left_join(. , df.states, by=c("id"="state"))

spdf_fortified2$bin <- cut(spdf_fortified2$new7b,
                           breaks=c(-200,0,0.9,9,24.9,10000),
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

# Custom legend for plots -------------------------------------------------
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

# Patchwork plots and export ----------------------------------------------
(usa + pa) / leg + plot_layout(heights = c(2, 1)) + plot_annotation(
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
(usa + pa) / leg + plot_layout(heights = c(2, 1)) + plot_annotation(
  title='Which Places Have The Most New Daily Cases?',
  subtitle=paste("Data as of 11:59 p.m. ET", Sys.Date()-1)
) & theme(plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                                  margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                     margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))
dev.off()
