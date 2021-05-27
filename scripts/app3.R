library(tidyverse)
library(ggiraph)
library(shiny)
library(shinydashboard)
library(albersusa)
library(viridis)
library(zoo)

ts <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
ts <- read_csv(ts)

setwd("/Users/jeremymack/Documents/GitHub/covid19")

states.pop <- read_csv("data/us_population2020.csv")
states.pop <- states.pop %>%
  mutate(pop100=pop2020/100000)

df.states <- ts %>% 
  filter(FIPS >= 1001 & FIPS <= 56045) %>%
  select(5:7, 12:ncol(ts)) %>%
  gather(date, cases, 4:ncol(.), factor_key=TRUE) %>%
  mutate(date=as.Date(as.character(date),
                      format="%m/%d/%y")) %>%
  arrange(FIPS, date) %>%
  group_by(Province_State,date) %>%
  summarize(cases=sum(cases, na.rm=TRUE)) %>%
  rename(state=Province_State)

df.states <- df.states %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(new=cases-lag(cases, default=0)) %>%
  mutate(new7=rollapply(new,7,mean,fill=0,align="right")) %>%
  tibble()

us.sf <- usa_sf("longlat")
names(us.sf)[3] <- "state"

ui <- fixedPage(
  titlePanel("US COVID-19 Cases"),
  theme=bslib::bs_theme(bootswatch = "yeti"),
  tabsetPanel(
    tabPanel("Nationwide",
             fluidRow(
               column(width=1),
               column(width=11,
                 dateInput(inputId="date",
                           label=div(HTML("<h5><b>Select date:</b></h5>"),
                                     align="center"),
                           value=Sys.Date()-1,
                           min="2020-01-22",
                           max=Sys.Date()-1,
                           startview="month"))),
             fluidRow(
               column(width=8,
                      girafeOutput('plot1', width="100%"),
                      tableOutput('table1')),
               column(width=4,
                      plotOutput('plot2', width="100%")))
             )))

server <- function(input, output, session) {
  df.sub1 <- reactive({
    req(input$date)
    df.states %>% 
      filter(date==input$date) %>%
      left_join(states.pop, by="state") %>%
      mutate(new7b=new7/pop100)
  })
  
  df.sub2 <- reactive({
    df.sub1() %>%
      select(state,date,cases,new7,new7b) %>%
      arrange(desc(new7b)) %>%
      mutate(cases=as.integer(cases),
             new7=round(new7,1),
             new7b=round(new7b,1)) %>%
      rename(State=state,
             Date=date,
             'Total cases'=cases,
             'New cases<br>(7-day avg.)'=new7,
             "New cases per capita\n(7-day avg.)"=new7b) %>%
      mutate(Date=as.character(Date))})
  
  output$plot1 <- renderGirafe({
    df.sub <- df.sub1()
    
    us.sf <- us.sf %>% 
      left_join(df.sub, by = "state") %>%
      mutate(label=paste0(state,": ",round(new7b,1)," new cases\nper 100,000 people"))
    
    p1 <- ggplot() +
      geom_sf_interactive(data=us.sf, aes(fill=new7b, tooltip=label, data_id=state),
              size=0.0725, color="black") +
      scale_fill_viridis(option="A",
                         discrete = F,
                         name = paste("New cases per 100K residents: ", input$date, sep=""),
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
    
    girafe(ggobj=p1,
           options=list(
             opts_hover_inv(css="opacity:0.5;")
           ))
  })
  
  output$table1 <- renderTable(
    head(df.sub2(),5),
    digits=1
  )
  
  output$plot2 <- renderPlot(height=700, {
    df.sum <- df.states %>%
      group_by(date) %>%
      summarize(total=sum(cases)) %>%
      mutate(new=total-lag(total,default=0)) %>%
      mutate(new7=rollapply(new,7,mean,fill=0,partial=TRUE,align="right"))
    
    new <- ggplot(data=df.sum, aes(x=date, y=new7/1000)) +
      coord_flip()+
      geom_line(color="#e08f38") +
      geom_col(aes(y=new/1000), alpha=0.3, width=0.7, fill="#e08f38") +
      geom_vline(xintercept=input$date) +
      expand_limits(y=c(0,300)) +
      scale_y_continuous(expand=c(0.01,0)) +
      scale_x_date(expand=c(0.01,0), date_breaks="1 month", date_labels="%b%y") +
      theme(panel.background=element_blank(),
            panel.grid=element_blank(),
            strip.background=element_rect(color="black", size=0.25),
            axis.line=element_line(size=0.25),
            axis.ticks=element_line(size=0.25),
            axis.text=element_text(size=11, color="black"),
            axis.title=element_blank(),
            legend.justification="top",
            legend.title=element_blank(),
            legend.position=c(0.90,1.04),
            legend.text=element_text(size=6, color="black"),
            legend.key=element_blank(),
            legend.key.width=unit(1.2,"line"),
            legend.key.size = unit(1, 'lines'))
    
    new
  })
  
}

shinyApp(ui, server)
