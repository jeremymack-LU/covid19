# Description -------------------------------------------------------------
# Code for creating figure of COVID-19 incidence in US and PA counties
# Updated: Updated June 10, 2021

# Load packages -----------------------------------------------------------
pacman::p_load(rvest,tidyverse,zoo,pdftools,maps,readr,sf,albersusa,tidycensus,
               viridis,geojsonio,broom,rgeos,patchwork,mapproj,RSocrata)

# Load data ---------------------------------------------------------------
# Time series data from New York Times github account
ts <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
ts <- read_csv(ts)

# # Load US and PA population data
# pop <- read.table("data/population.csv", sep=",", header=TRUE)
# pop <- pop %>% mutate(population=as.numeric(gsub(",","",population)))
# pa.pop <- read.table("data/pa_population.csv", sep=",", header=TRUE); names(pa.pop)[1] <- "admin2"

# Load census population data
# To update census data use the following code
# census <- get_acs(geography="county",variables="B01003_001",year = 2019)
# write_csv(census, "data/census.csv")
pop <- read_csv("data/census.csv")
pop <- pop %>%
  mutate(state=str_extract(NAME, '\\b[^,]+$')) %>%
  select(state,GEOID,estimate) %>%
  rename(fips=GEOID,
         population=estimate) %>%
  arrange(fips) %>%
  mutate(fips=factor(fips))

pop.states <- pop %>%
  group_by(state) %>%
  summarize(population=sum(population,na.rm=TRUE))

# Create a time series dataset for states
ts.states <- ts %>%
  arrange(state,county,date) %>%
  group_by(state,date) %>%
  summarize(cases=sum(cases)) %>%
  tibble() %>%
  group_by(state) %>%
  mutate(new=cases-lag(cases, default=0)) %>%
  mutate(new7avg=rollapply(new,7,mean,fill=0,align="right"),
         new7sum=rollapply(new,7,sum,fill=0,align="right"))

# Create a time series dataset for counties
ts.counties <- ts %>%
  arrange(state,county,date) %>%
  group_by(fips) %>%
  mutate(new=cases-lag(cases, default=0)) %>%
  mutate(new7avg=rollapply(new,7,mean,fill=0,align="right"),
         new7sum=rollapply(new,7,sum,fill=0,align="right"))

# Select most recent date (removing territories)
rm <- c("Guam","Northern Mariana Islands","Puerto Rico","Virgin Islands")
ts.counties.recent <- ts.counties %>%
  filter(!state %in% rm) %>%
  # Select most recent date
  slice(n()) %>%
  # Add population data
  left_join(pop, by=c("fips")) %>%
  # Calculate new cases per capita
  mutate(pop100=population/100000,
         new7avgP=round(new7avg/pop100,1),
         new7sumP=round(new7sum/pop100,1),
         fips=factor(fips))

# Select most recent date (removing territories)
ts.recent <- ts.states %>%
  filter(!state %in% rm) %>%
  # Select most recent date
  slice(n()) %>%
  # Add population data
  left_join(pop.states, by="state") %>%
  # Calculate new cases per capita
  mutate(pop100=population/100000,
         new7avgP=round(new7avg/pop100,1),
         new7sumP=round(new7sum/pop100,1))

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
  left_join(. , ts.recent, by=c("id"="state"))

spdf_fortified2 <- spdf_fortified2 %>% 
  mutate(bin=cut(new7avgP,
                 breaks=c(-200,0,0.9,9,24.9,10000),
                 labels=c("X","A","B","C","D")),
         bin2=case_when(
           new7sumP <= 10 ~ "Low",
           new7sumP > 10 & new7sumP <= 49 ~ "Moderate",
           new7sumP > 50 & new7sumP <= 99 ~ "Substantial",
           TRUE ~ "High"
         ))
  
# State labels
labels <- tibble(
  state=spdf@data$google_name,
  label=spdf@data$iso3166_2
)


# USA Hexagon Plot - incidence --------------------------------------------
# Set colors for plotting
cols1 <- c("X"="lightgray","A"="#197d7d", "B"="#dbc037", "C"="#e08f38", "D"="#8C1111")
cols2 <- c("No data"="lightgray",
           "Low"="#CEF2F2",
           "Moderate"="#F2E191",
           "Substantial"="#B5482A",
           "High"="#6E1F09")
cols3 <- c("No data"="lightgray",
           "Low"="#197d7d",
           "Moderate"="#dbc037",
           "Substantial"="#e08f38",
           "High"="#8C1111")

usa.hex <- ggplot() +
  geom_polygon(data=spdf_fortified2, aes(x=long, y=lat, fill=bin, group=group), color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id), col="white", size=2.5) +
  theme_void() +
  coord_map() +
  scale_fill_manual(values=cols1) +
  theme(
    legend.position="none",
    text=element_text(color="#22211d"),
    plot.background=element_rect(fill="white", color=NA), 
    panel.background=element_rect(fill="white", color=NA), 
    plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                            margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                               margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

# PA plot - incidence by county -------------------------------------------
# Create Pennsylvania data frame
ts.pa <- ts %>% 
  filter(state=="Pennsylvania") %>%
  arrange(county,date) %>%
  group_by(county) %>%
  mutate(new=cases-lag(cases, default=0)) %>%
  mutate(new7avg=rollapply(new,7,mean,fill=0,align="right"),
         new7sum=rollapply(new,7,sum,fill=0,align="right"),
         fips=factor(fips)) %>%
  # Select most recent date
  slice(n()) %>%
  # Add population data
  left_join(pop, by=c("fips")) %>%
  # Calculate new cases per capita
  mutate(pop100=population/100000,
         new7avgP=round(new7avg/pop100,1),
         new7sumP=round(new7sum/pop100,1)) %>%
  filter(county != "Unknown")

cty.sf <- counties_sf("longlat")
st_crs(cty.sf) <- st_crs(cty.sf)

pa.sf <- cty.sf %>%
  #mutate(fips=as.integer(stringr::str_remove(fips, "^0+"))) %>%
  #rename(county=name) %>%
  mutate(fips=factor(fips)) %>%
  right_join(ts.pa, by = "fips") %>%
  mutate(bin=cut(new7avgP,
                 breaks=c(-200,0.9,9.9,24.9,1050),
                 labels=c("A","B","C","D"))) %>%
  mutate(bin2=case_when(
    new7sumP <= 10 ~ "Low",
    new7sumP > 10 & new7sumP <= 49 ~ "Moderate",
    new7sumP > 50 & new7sumP <= 99 ~ "Substantial",
    TRUE ~ "High"
  ))
  
cty.sf <- cty.sf %>% 
  #mutate(fips=as.integer(stringr::str_remove(fips, "^0+"))) %>%
  #rename(county=name) %>%
  mutate(fips=factor(fips)) %>%
  right_join(ts.counties.recent, by = "fips") %>%
  mutate(bin=cut(new7avgP,
                 breaks=c(-200,0.9,9.9,24.9,1050),
                 labels=c("A","B","C","D"))) %>%
  mutate(bin2=case_when(
    new7sumP <= 10 ~ "Low",
    new7sumP > 10 & new7sumP <= 49 ~ "Moderate",
    new7sumP > 50 & new7sumP <= 99 ~ "Substantial",
    TRUE ~ "High"
  ))

# US plot - weekly cases per capita by county -----------------------------
states.sf <- usa_sf("longlat")
st_crs(states.sf) <- st_crs(states.sf)

usa <- ggplot() +
  geom_sf(data=cty.sf, aes(fill=bin2),
          size=0.25, color="white") +
  geom_sf(data=states.sf, color="white",fill=NA, size=0.3) +
  scale_fill_manual(values=cols3) +
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

# Pennsylvania plot - incidence by county ---------------------------------
pa1 <- ggplot() +
  geom_sf(data=pa.sf, aes(fill=bin),
          size=0.25, color="white") +
  scale_fill_manual(values=cols1) +
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

# Pennsylvania plot - weekly cases per capita by county -------------------
pa2 <- ggplot() +
  geom_sf(data=cty.sf[cty.sf$state.x=="Pennsylvania",],
          aes(fill=bin2), size=0.25, color="white") +
  scale_fill_manual(values=cols3) +
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

# Custom legend for plots -------------------------------------------------
source("scripts/custom_legend.R")

# Patchwork plots and export ----------------------------------------------
(usa.hex + pa1) / leg + plot_layout(heights = c(2, 1)) + plot_annotation(
  title='Which Places Have The Most New Daily Cases?',
  subtitle=paste("Data as of 11:59 p.m. ET", Sys.Date()-1)
) & theme(plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                                  margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                     margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

((usa+pa2+plot_layout(width=c(1,1)))/leg2)+plot_layout(heights = c(2,1)) + plot_annotation(
  title='CDC Advises Masking Indoors In Counties With Substantial\nOr High Coronavirus Spread?',
  subtitle=paste("Data as of 11:59 p.m. ET", Sys.Date()-1)
) & theme(plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47",
                                  margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                     margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

jpeg(file="output/USA_cases2.jpeg",
     width=7,height=4.75,
     units="in",
     res=1200)
(usa.hex + pa1) / leg + plot_layout(heights = c(2, 1)) + plot_annotation(
  title='Which Places Have The Most New Daily Cases?',
  subtitle=paste("Data as of 11:59 p.m. ET", Sys.Date()-1)
) & theme(plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                                  margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                     margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))
dev.off()

jpeg(file="output/CDC_masking_us.jpeg",
     width=7,height=6,
     units="in",
     res=1200)
usa/leg2+plot_layout(heights = c(2,1)) + plot_annotation(
  title='CDC Advises Masking Indoors In Counties With Substantial\nOr High Coronavirus Spread?',
  subtitle=paste("Data as of 11:59 p.m. ET", Sys.Date()-1)
) & theme(plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47",
                                  margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                     margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))
dev.off()

jpeg(file="output/CDC_masking_pa.jpeg",
     width=7,height=6,
     units="in",
     res=1200)
pa2/leg2+plot_layout(heights = c(2,1)) + plot_annotation(
  title='CDC Advises Masking Indoors In Counties With Substantial\nOr High Coronavirus Spread?',
  subtitle=paste("Data as of 11:59 p.m. ET", Sys.Date()-1)
) & theme(plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47",
                                  margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                     margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))
dev.off()

# Vaccination plot --------------------------------------------------------
# Load US vaccination data
us.vac <- read.socrata(
  url = "https://data.cdc.gov/resource/unsk-b7fc.csv",
  app_token = "FEdwTLmlOc5M85tR9R7nIn7VG"
)

rm <- c("AS","FM","GU","MH","MP","PR","RP","US","VI")

us.vac <- us.vac %>%
  filter(date==as.Date("2021-07-22")) %>%
  mutate(location_n=nchar(location)) %>%
  filter(location_n == 2,
         !location %in% rm) %>%
  select(location,series_complete_pop_pct) %>%
  left_join(labels, by=c("location"="label"))

spdf_fortified2 <- spdf_fortified2 %>%
  left_join(us.vac, by=c("id"="state"))

# spdf_fortified2 <- spdf_fortified2 %>% 
#   mutate(bin2=cut(series_complete_pop_pct,
#                   breaks=c(0,24.9,49.9,75.9,100),
#                   labels=c("A","B","C","D")))
# 
# cols2 <- c("A"="#e1e7f0", "B"="#8ba4c9", "C"="#3865a8", "D"="#031f4a")

vac.label <- scales::label_percent(accuracy=1)

usa.vac <- ggplot() +
  geom_polygon(data=spdf_fortified2, 
               aes(x=long, y=lat, fill=series_complete_pop_pct/100, group=group),
               color="white") +
  geom_text(data=centers,
            aes(x=x, y=y, label=id),
            col="white", size=2.5) +
  theme_void() +
  coord_map() +
  scale_fill_gradient(high = "#132B43",
                      low = "#a4d2f5",
                      labels = vac.label) +
  theme(legend.position=c(0.5,0.95),
        legend.title=element_blank(),
        legend.direction="horizontal",
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size=7),
        text=element_text(color="#22211d"),
        plot.background=element_rect(fill="white", color=NA),
        panel.background=element_rect(fill="white", color=NA))

# Load PA vaccination data
pa.vac <- read_csv("https://data.pa.gov/resource/gcnb-epac.csv")

pa.vac <- pa.vac %>%
  rename(population=county_population,
         partial=partially_covered,
         full=fully_covered) %>%
  mutate(per.partial=(partial/population)*100,
         per.full=(full/population)*100,
         per.one=((partial+full)/population)*100,
         county=case_when(
           county == "Mckean" ~ "McKean",
           TRUE ~ county
         ))

pa.sf <- pa.sf %>% 
  #mutate(fips=as.integer(stringr::str_remove(fips, "^0+"))) %>%
  left_join(pa.vac, by = "county") %>%
  mutate(bin3=cut(per.full,
                 breaks=c(0,24.9,49.9,75.9,100),
                 labels=c("A","B","C","D")),
         bin4=cut(per.one,
                  breaks=c(0,24.9,49.9,75.9,100),
                  labels=c("A","B","C","D")))

pa.vac <- ggplot() +
  geom_sf(data=pa.sf, aes(fill=per.full/100),
          size=0.25, color="white") +
  #scale_fill_manual(values=cols) +
  scale_fill_gradient(high = "#132B43",
                      low = "#a4d2f5",
                      labels = vac.label) +
  theme_void() +
  theme(legend.position=c(0.5,0.95),
        legend.title=element_blank(),
        legend.direction="horizontal",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size=7),
        text=element_text(color="#22211d"),
        plot.background=element_rect(fill="white", color=NA),
        panel.background=element_rect(fill="white", color=NA))

usa.vac + pa.vac + plot_annotation(
  title='Who Has Vaccinated More Of Their Population?',
  subtitle=paste("Percentage of population as of", format(Sys.Date()-1, "%B %d\n\n"))) & 
  theme(plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47",
                                margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                   margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

jpeg(file="output/vaccination.jpeg",
     width=7,height=4.75,
     units="in",
     res=1200)
usa.vac + pa.vac + plot_annotation(
  title='Who Has Vaccinated More Of Their Population?',
  subtitle=paste("Percentage of population as of", format(Sys.Date()-1, "%B %d\n\n"))) & 
  theme(plot.title=element_text(size= 12, hjust=0.5,color="#4e4d47",
                                margin=margin(b = 0, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                   margin=margin(b = 0, t = 0.4, l = 2, unit = "cm")))
dev.off()

# Update files on Google Drive --------------------------------------------
library(googledrive)

drive_auth(
  email="jsm4@lehigh.edu",
  path="data/client_secret_1055497976267-6fk2msjr8u13ldumbkfsu346rrd2tvo8.apps.googleusercontent.com.json"
)

drive_update(as_id('10jY1mgIreHg2CAEsvM4h9AHmt2D4l-GE'), 'output/CDC_masking_pa.jpeg')

drive_update(as_id('16RbIjZD5KfMwSWKW8t4WpE_ZjlCBicit'), 'output/CDC_masking_us.jpeg')

drive_update(as_id('1z6Ykx6VCRw_e_q92Ee-5XQhCM1Dd04NF'), 'output/vaccination.jpeg')

drive_update(as_id('1_KHASISfH00e5s0jqeCitfk2V1ND2oSN'), 'output/USA_cases2.jpeg')
