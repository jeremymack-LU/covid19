pacman::p_load(tidyverse,cronR)

setwd("/Users/jeremymack/Documents/GitHub/covid19")

# County and State level data from Johns Hopkins CSSE
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

write_csv(df.county,"/Users/jeremymack/Documents/GitHub/covid19/data/df_county.csv")

# gitstatus <- function(dir = getwd()){
#   cmd <- paste("git status ",dir,sep="")
#   system(cmd)
# }

dir <- "/Users/jeremymack/Documents/GitHub/covid19"

gitstatus <- function(){
  cmd_list <- list(
    cmd1 = paste("cd",dir),
    cmd2 = "git status"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  system(cmd)
}

gitadd <- function(){
  cmd_list <- list(
    cmd1 = paste("cd",dir),
    cmd2 = "git add ."
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  system(cmd)
}

# gitadd <- function(){
#   cmd <- "git add ."
#   system(cmd)
# }

gitcommit <- function(){
  cmd_list <- list(
    cmd1 = paste("cd",dir),
    cmd2 = "git commit -m 'updates'"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  system(cmd)
}

# gitcommit <- function(){
#   cmd <- "git commit -m 'updates'"
#   system(cmd)
# }

gitpush <- function(){
  cmd_list <- list(
    cmd1 = paste("cd",dir),
    cmd2 = "git push"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  system(cmd)
}

# gitpush <- function(){
#   cmd <- "git push"
#   system(cmd)
# }

gitstatus()
gitadd()
gitcommit()
gitpush()
