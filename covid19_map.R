url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-19-2020.csv"

df <- read.csv(url, sep=",", header=TRUE)
df.us <- df %>% filter(Country.Region=="US") %>% as.data.frame()

remove <- c("Guam", "Puerto Rico", "Virgin Islands",
            "United States Virgin Islands", "US", 
            "Grand Princess", "Diamond Princess")

df.us2 <- df.us %>% filter(!Province.State %in% remove) %>% as.data.frame()

