library(dplyr)
library(ggplot2)
library(usmap)
library(tidyr)
library(shiny)
library(shinydashboard)
library(viridis)
library(zoo)
library(maps)
library(ggthemes)
library(mapproj)
library(sf)
library(albersusa)
library(stringr)


ts <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
ts <- read.csv(ts, sep=",", header=TRUE)

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

ui <- dashboardPage(
  dashboardHeader(title="US COVID-19 Cases"),
  dashboardSidebar(
    fluidRow(
      dateInput(inputId="date",
                label=HTML(paste(p(HTML('&nbsp;'),strong("Select date:")))),
                value=Sys.Date()-1,
                min="2020-01-22",
                max=Sys.Date()-1,
                startview="month")),
    fluidRow(
      selectInput(inputId="state",
                  label=HTML(paste(p(HTML('&nbsp;'),strong("Select state:")))),
                  choices=unique(df.us$state),
                  selected="Pennsylvania")),
    fluidRow(align="center",
               tableOutput("table")),
    fluidRow(align="center",
             tableOutput("table2"))),
  dashboardBody(
    fluidRow(
      box(title = textOutput('title1'),
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          plotOutput('plot1', height = "300px", width = "100%")),
      box(title = textOutput('title2'),
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          plotOutput('plot2', height = "300px", width = "100%"))),
    fluidRow(
      box(title = textOutput('title3'),
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          align = "center",
          plotOutput('plot3', height = "300px", width = "100%")),
      box(title = textOutput('title4'),
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          align = "center",
          plotOutput('plot4', height = "300px", width = "100%"))
    )
  ))

server <- function(input, output) {
  date1 <- reactive({
    req(input$date)
    print(input$date)
  })
  
  output$title1 <- reactive({
    req(input$state)
    print(paste("Nationwide COVID-19 Cases on ",
                input$date, 
                sep=""))
  })
  
  output$title2 <- reactive({
    req(input$state)
    print(paste(input$state,
                " COVID-19 Cases on ", 
                input$date, 
                sep=""))
  })
  
  output$title3 <- reactive({
    req(input$date)
    print(paste("Total COVID-19 cases from Jan 22 to ", 
                format(tail(df.us$date,1), "%b %d"), 
                sep=""))
  })
  
  output$title4 <- reactive({
    req(input$date)
    print(paste("New COVID-19 cases from Jan 22 to ", 
                format(tail(df.us$date,1), "%b %d"), 
                sep=""))
  })
  
  df.sub1 <- reactive({
    req(input$date)
    df.us %>% 
      filter(date==input$date)
  })
  
  df.sub2 <- reactive({
    req(input$date)
    req(input$state)
    df.us %>% 
      filter(date==input$date, state==input$state)
  })
  
  df.sub2b <- reactive({
    req(input$state)
    df.us %>% 
      filter(state==input$state, cases==1) %>%
      arrange(date)
  })
  
  output$table <- renderTable(bordered=TRUE, align="l",{
    req(input$date)
    req(input$state)
    tbl <- df.us %>%
      filter(date==input$date) %>%
      summarize("United States"=sum(cases),
                "State"=sum(df.sub2()$cases)) %>%
      gather("Group:", "Total Cases:", 1:2, factor_key=TRUE)
    
    tbl[,1] <- as.character(tbl[,1])
    tbl[2,1] <- input$state
    names(tbl)[1] <- ""
    
    print(tbl)
  })
  
  output$table2 <- renderTable(bordered=TRUE, align="l",{
    tbl2 <- df.sub2() %>%
      arrange(desc(cases)) %>% 
      mutate("County:"=admin2,
             "Total Cases:"=cases) %>%
      select("County:", "Total Cases:")
    
    tbl2 <- head(tbl2, 5)
    
    print(tbl2)
  })
  
  output$plot1 <- renderPlot(height=300, {
    df1 <- df.sub1()
    df1 <- df1 %>%
      group_by(state) %>%
      select(cases) %>%
      summarise_all(funs(sum))
    
    us_sf <- usa_sf("longlat")
    names(us_sf)[3] <- "state"
    us_sf <- us_sf %>% left_join(df1, by = "state")
    
    p1 <- ggplot() +
      geom_sf(data=us_sf, aes(fill=cases),
              size=0.0725, color="black") +
      scale_fill_viridis(option="A",
                         discrete = F,
                         name = paste("Cases: ", input$date, sep=""),
                         direction = -1,
                         guide = guide_colourbar(
                           direction = "horizontal",
                           barheight = unit(2, units = "mm"),
                           barwidth = unit(100, units = "mm"),
                           draw.ulim = F,
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5)) +
      theme(panel.grid = element_blank(),
            line = element_blank(),
            rect = element_blank(),
            axis.text = element_blank(),
            plot.background = element_rect(fill = "white"),
            legend.position = "bottom")
      
      p1
  })
  
  output$plot2 <- renderPlot(height=300, {
    df2 <- df.sub2()
    df2b <- df.sub2b()
    
    cty_sf <- counties_sf("longlat")
    cty_sf <- cty_sf %>% mutate(fips=as.integer(stringr::str_remove(fips, "^0+")))
    cty_sf <- cty_sf %>% filter(state==input$state)
    cty_sf2 <- cty_sf %>% left_join(df2, by = "fips")
    
    p2 <- {
      if(date1() >= df2b$date[1] & date1() <= Sys.Date()-1)
        ggplot() +
        geom_sf(data=cty_sf2, aes(fill=cases),
                size=0.0725, color="black") +
        scale_fill_viridis(option="A",
                           discrete = F,
                           name = paste("Cases: ", input$date, sep=""),
                           direction = -1,
                           guide = guide_colourbar(
                             direction = "horizontal",
                             barheight = unit(2, units = "mm"),
                             barwidth = unit(100, units = "mm"),
                             draw.ulim = F,
                             title.position = 'top',
                             title.hjust = 0.5,
                             label.hjust = 0.5)) +
        theme(panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              axis.text = element_blank(),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom")
        
      else ggplot() +
        geom_sf(data=cty_sf2, fill="white",
                size=0.0725, color="black") +
        theme(panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              axis.text = element_blank(),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom")
    }
    p2
  })
  
  output$plot3 <- renderPlot(height=300, {
    df3 <- df.us %>%
      group_by(state, date) %>%
      select(cases) %>%
      summarise_all(funs(sum))
      
    ggplot() + geom_line(data=df3, aes(x=date, y=cases/1000, group=state), alpha=0.1) +
      geom_line(data=df3[df3$state==input$state,], aes(x=date, y=cases/1000), size=1.5) +
      geom_vline(xintercept=input$date) +
      labs(x="Date",
           y="COVID-19 cases (in thousands)") +
      theme(panel.background=element_blank(),
            panel.grid=element_blank(),
            plot.title=element_text(size=9, color="black"),
            strip.background=element_rect(color="black", size=0.25),
            axis.line=element_line(size=0.25),
            axis.ticks=element_line(size=0.25),
            axis.text=element_text(size=7, color="black"),
            axis.title=element_text(size=8, color="black"),
            legend.text=element_text(size=7, color="black"),
            legend.justification="top",
            legend.title=element_blank(),
            legend.position=c(0.12,1),
            legend.key=element_blank())
  })
  
  output$plot4 <- renderPlot(height=300, {
    df4 <- df.us %>%
      group_by(state, date) %>%
      select(cases) %>%
      summarise_all(funs(sum)) 
    df4 <- df4 %>%
      group_by(state) %>%
      mutate(new=cases-lag(cases,default=0)) %>%
      mutate(new7=rollapply(new,7,mean,fill=0,partial=TRUE,align="right"))
    
    ggplot() + geom_line(data=df4, aes(x=date, y=new7, group=state), alpha=0.1) +
      geom_line(data=df4[df4$state==input$state,], aes(x=date, y=new7), size=1.5) +
      geom_vline(xintercept=input$date) +
      labs(x="Date",
           y="New COVID-19 cases") +
      theme(panel.background=element_blank(),
            panel.grid=element_blank(),
            plot.title=element_text(size=9, color="black"),
            strip.background=element_rect(color="black", size=0.25),
            axis.line=element_line(size=0.25),
            axis.ticks=element_line(size=0.25),
            axis.text=element_text(size=7, color="black"),
            axis.title=element_text(size=8, color="black"),
            legend.text=element_text(size=7, color="black"),
            legend.justification="top",
            legend.title=element_blank(),
            legend.position=c(0.12,1),
            legend.key=element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)