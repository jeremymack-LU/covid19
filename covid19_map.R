library(dplyr)

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-23-2020.csv"

df <- read.csv(url, sep=",", header=TRUE)
df.us <- df %>% filter(Country_Region=="US") %>% as.data.frame()

remove <- c("Guam", 
            "Puerto Rico", 
            "Virgin Islands",
            "United States Virgin Islands", 
            "US", 
            "Grand Princess", 
            "Diamond Princess",
            "American Samoa",
            "Wuhan Evacuee",
            "Northern Mariana Islands")

df.us2 <- df.us %>% filter(!Province_State %in% remove) %>% group_by(Province_State) %>% summarize(Cases=sum(Confirmed)) %>% as.data.frame()


