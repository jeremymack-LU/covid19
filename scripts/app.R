library(dplyr)
library(ggplot2)
library(usmap)
library(tidyr)
library(shiny)
library(shinydashboard)
library(viridis)
library(zoo)

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
            "Recovered",
            "US Military",
            "Veteran Hospitals",
            "Federal Bureau of Prisons")

ts2 <- ts2 %>% 
  filter(!Province_State %in% remove) %>% 
  as.data.frame()

df <- ts2 %>%
  gather(date, cases, 2:ncol(ts2), factor_key=TRUE)

x <- 51
dates <- data.frame(rep(
  seq(as.Date('2020-01-22'), as.Date('2020-01-22')+(ncol(ts)-12), by = 'days'), 
  times = x))
names(dates) <- "date"
dates <- dates %>% arrange(date)

df <- cbind(df, dates); df <- df[,c(1,4,3)]; names(df)[1] <- "state"

df2 <- df %>% 
  group_by(state) %>%
  mutate(new=cases-lag(cases, default=0)) %>%
  mutate(new7=rollapply(new,7,mean,fill=0,align="right"))

ts.pa <- ts %>% filter(Province_State == "Pennsylvania")
ts.pa2 <- ts.pa %>% select(5,12:ncol(ts.pa))

remove2 <- c(80042, 90042)

ts.pa2 <- ts.pa2 %>% 
  filter(!FIPS %in% remove2) %>% 
  as.data.frame()

df.pa <- ts.pa2 %>%
  gather(date, cases, 2:ncol(ts.pa2), factor_key=TRUE)

x2 <- 67
dates2 <- data.frame(rep(
  seq(as.Date('2020-01-22'), as.Date('2020-01-22')+(ncol(ts)-12), by = 'days'), 
  times = x2))
names(dates2) <- "date"
dates2 <- dates2 %>% arrange(date)

df.pa <- cbind(df.pa, dates2); df.pa <- df.pa[,c(1,4,3)]
names(df.pa)[1] <- "fips"

# Load US and PA population data
pop.us <- read.table("data/us_population.csv", sep=",", header=TRUE)
pop.us <- pop.us %>% mutate(population=as.numeric(gsub(",","",pop_2015)))

# Load PA population data
pop.pa  <- read.table("data/pa_population.csv", sep=",", header=TRUE); names(pop.pa)[1] <- "admin2"
fips.pa <- fips.pa <- read.csv("fips_pa.csv", header=TRUE)
pop.pa  <- pop.pa %>% left_join(fips.pa, by="admin2")

ui <- dashboardPage(
  skin="black",
  dashboardHeader(title="US COVID-19 Cases"),
  dashboardSidebar(
    fluidRow(
      dateInput(inputId="date",
                label=HTML(paste(p(HTML('&nbsp;'),strong("Select date:")))),
                value=Sys.Date()-1,
                min="2020-01-22",
                max=Sys.Date()-1,
                startview="month")),
    fluidRow(align="center",
      tableOutput("table")),
    fluidRow(
      selectInput(inputId="state",
                  label=HTML(paste(p(HTML('&nbsp;'),strong("Select state:")))),
                  choices=unique(df$state))
    )),
  dashboardBody(
    fluidRow(
      box(title = "COVID-19 Cases - Nationwide",
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          plotOutput('plot1', height = "300px", width = "100%")),
      box(title = "COVID-19 Cases - Pennsylvania",
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          plotOutput('plot2', height = "300px", width = "100%"))),
    fluidRow(
      box(title = textOutput('title1'),
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          align = "center",
          plotOutput('plot3b', height = "300px", width = "100%")),
      box(title = textOutput('title2'),
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          align = "center",
          plotOutput('plot4b', height = "300px", width = "100%"))
    )
  ))

server <- function(input, output) {
  
  date1 <- reactive({
    req(input$date)
    print(input$date)
  })
  
  output$title1 <- reactive({
    req(input$date)
    print(paste("Total COVID-19 cases from Jan 22 to ", 
                format(tail(df$date,1), "%b %d"), 
                sep=""))
  })
  
  output$title2 <- reactive({
    req(input$date)
    print(paste("New COVID-19 cases from Jan 22 to ", 
                format(tail(df$date,1), "%b %d"), 
                sep=""))
  })
  
  df.sub1 <- reactive({
    req(input$date)
    df %>% 
      filter(date==input$date) %>%
      left_join(pop, by="state") %>%
      mutate(percap=cases/(population/100000))
  })
  
  df.sub2 <- reactive({
    req(input$date)
    df.pa %>%
      filter(date==input$date) %>%
      left_join(pop.pa, by="fips") %>%
      mutate(percap=cases/(population/100000))
  })
  
  df.sub3 <- reactive({
    req(input$date)
    df %>% 
      filter(date<=input$date) %>%
      group_by(date) %>%
      summarize(total=sum(cases)/1000) %>%
      mutate(new=total - lag(total, default = first(total))) %>%
      gather(group,cases,2:3,factor_key=TRUE) %>% 
      arrange(date,desc(group)) %>%
      as.data.frame()
  })
  
  df.sub4 <- reactive({
    req(input$date)
    df.PA %>% 
      filter(date<=input$date) %>%
      group_by(date) %>%
      summarize(total=sum(cases)/1000) %>%
      mutate(new=total - lag(total, default = first(total))) %>%
      gather(group,cases,2:3,factor_key=TRUE) %>% 
      arrange(date,desc(group)) %>%
      as.data.frame()
  })
  
  output$table <- renderTable(bordered=TRUE, align="c",{
    req(input$date)
    df %>%
      filter(date==input$date) %>%
      summarize("United States"=sum(cases),
                "Pennsylvania"=sum(cases[state=="Pennsylvania"]),
                "Lehigh Valley"=df.sub2()$cases[df.sub2()$fips==42025]+df.sub2()$cases[df.sub2()$fips==42077]+df.sub2()$cases[df.sub2()$fips==42095]) %>%
      gather("Group:", "Total Cases:", 1:3, factor_key=TRUE)
  })
  
  output$plot1 <- renderPlot({
    df1 <- df.sub1()
    plot_usmap(data=df1,
               values="percap",
               size=0.1,
               regions="state") +
      scale_fill_viridis(option="E",
                         discrete = F,
                         name = paste("Cases: ", df1$date[1]),
                         direction = -1,
                         guide = guide_colourbar(
                           direction = "horizontal",
                           barheight = unit(2, units = "mm"),
                           barwidth = unit(100, units = "mm"),
                           draw.ulim = F,
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5)) +
      theme(legend.position = "bottom")
  })
  
  output$plot2 <- renderPlot({
    df2 <- df.sub2()
    pa.county <- map_data("county") %>%
      filter(region=="pennsylvania")
    
    pa.fips <- county.fips %>%
      separate(polyname, c("region", "subregion"), sep = ",") %>%
      filter(region=="pennsylvania")
    
    pa.data <- left_join(pa.county, pa.fips, c("region", "subregion"))
    pa.data <- left_join(pa.data, df2, "fips")
    p <- {
      if(date1() >= as.Date("2020-03-06") & date1() <= Sys.Date()-1) 
        ggplot(pa.data) +
        geom_polygon(aes(long, lat, group=group, fill=cases),
                     color = "black", size = 0.1) + 
        ggthemes::theme_map() + 
        coord_map("mercator") +
        scale_fill_viridis(option="A",
                           discrete = F,
                           name = paste("Pennsylvania Cases: ", input$date),
                           direction = -1,
                           guide = guide_colourbar(
                             direction = "horizontal",
                             barheight = unit(2, units = "mm"),
                             barwidth = unit(100, units = "mm"),
                             draw.ulim = F,
                             title.position = 'top',
                             title.hjust = 0.5,
                             label.hjust = 0.5)) +
        theme(legend.position = "bottom")
      else ggplot(pa.county) +
        geom_polygon(aes(long,lat,group=group), 
                     color="black", size=0.1, fill="white") +
        ggthemes::theme_map() + 
        coord_map("mercator")
    }
    p
  })
  
  output$plot3 <- renderPlot(height=300, {
    df3 <- df.sub3()
    ggplot(data=df3, aes(x=date,y=cases, group=group, color=group)) +
      geom_line(size=0.25) +
      geom_point(size=1) + 
      labs(x="Date",
           y="COVID-19 cases (in thousands)") +
      scale_x_date(date_labels="%m/%d",
                   limits=c(as.Date("2020-01-22"),tail(df3$date,1)+1)) +
      scale_color_manual(values=c("black","blue"),
                         labels=c("Total cases","New cases")) +
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
    df4 <- df.sub4()
    ggplot(data=df4, aes(x=date,y=cases, group=group, color=group)) +
      geom_line(size=0.25) +
      geom_point(size=1) + 
      labs(x="Date",
           y="COVID-19 cases (in thousands)") +
      scale_x_date(date_labels="%m/%d",
                   limits=c(as.Date("2020-01-22"),tail(df4$date,1)+1)) +
      scale_color_manual(values=c("black","blue"),
                         labels=c("Total cases","New cases")) +
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
  
  output$plot3b <- renderPlot(height=300, {
    ggplot() + geom_line(data=df, aes(x=date, y=cases/1000, group=state), alpha=0.1) +
      geom_line(data=df[df$state==input$state,], aes(x=date, y=cases/1000), size=1.5) +
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
  
  output$plot4b <- renderPlot(height=300, {
    ggplot() + geom_line(data=df2, aes(x=date, y=new7, group=state), alpha=0.1) +
      geom_line(data=df2[df$state==input$state,], aes(x=date, y=new7), size=1.5) +
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
