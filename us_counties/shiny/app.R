library(shiny)
library(plotly)
library(rjson)
library(tidyverse)

df <- read_delim(url("https://tsoleary.github.io/gym_class/us_counties/data/clean/combined_data.tsv"), delim = "\t") %>%
  mutate(pop_d = pop_est_2019/area)

df_rank <- df %>% mutate(rank_education = min_rank(desc(education)),
                         rank_life = min_rank(desc(life)),
                         rank_unemployment = min_rank(unemployment),
                         rank_sun = min_rank(desc(sun))) 

counties <- rjson::fromJSON(file='https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json')

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Where in the US are you best suited to live?"),
  
  # Output: Histogram ----
  fluidRow(
    column(12,
           # Output: Tabset w/ plot, summary, and table ----
           tabsetPanel(type = "tabs",
                       tabPanel("Map", plotlyOutput(outputId = "distPlot")),
                       tabPanel("Table", tableOutput("table"))
           )
           
           
    )
  ),
  
  
  hr(),
  
  fluidRow(
    column(4,
           h3("Weather"),
           # Input: Slider for the number of bins ----
           sliderInput(inputId = "temp_jan_range",
                       label = "Coldest Month (°F)",
                       min = 10,
                       max = 75,
                       value = c(10,75)),
           
           # Input: Slider for the number of bins ----
           sliderInput(inputId = "temp_jul_range",
                       label = "Warmest Month (°F)",
                       min = 55,
                       max = 110,
                       value = c(55,110)),
           # Input: Slider for the number of bins ----
           sliderInput(inputId = "sun_w",
                       label = "Sunshine",
                       min = 0,
                       max = 5,
                       value = 5)
    ),
    column(4,
           h3("Population"),
           # Input: Slider for the number of bins ----
           sliderInput(inputId = "rural_urban_range",
                       label = "Urban to Rural",
                       min = 1,
                       max = 9,
                       value = c(1, 12)),
           
           actionButton("update", "Update Map!")
           
    ),
    column(4,
           h3("Quality of Life Metrics"),
           # Input: Slider for the number of bins ----
           sliderInput(inputId = "une_w",
                       label = "Unemployment",
                       min = 0,
                       max = 5,
                       value = 5),
           # Input: Slider for the number of bins ----
           sliderInput(inputId = "edu_w",
                       label = "Education",
                       min = 0,
                       max = 5,
                       value = 5),
           # Input: Slider for the number of bins ----
           sliderInput(inputId = "lif_w",
                       label = "Life Expectancy",
                       min = 0,
                       max = 5,
                       value = 5)
    )
  )
)



# Define server logic required to draw plot
server <- function(input, output) {
  
  datasetInput <- eventReactive(input$update,
                                df_rank %>%
                                  filter(temp_jan >= input$temp_jan_range[1] &
                                           temp_jan <= input$temp_jan_range[2]) %>%
                                  filter(temp_jul >= input$temp_jul_range[1] &
                                           temp_jul <= input$temp_jul_range[2]) %>%
                                  filter(rural_urban_cont_2013 >= input$rural_urban_range[1] &
                                           rural_urban_cont_2013 <= input$rural_urban_range[2]) %>%
                                  mutate(w_rank_une = rank_unemployment * input$une_w,
                                         w_rank_edu = rank_education * input$edu_w,
                                         w_rank_lif = rank_life * input$lif_w,
                                         w_rank_sun = rank_sun * input$sun_w) %>%
                                  pivot_longer(contains("w_rank_"),
                                               names_to = "type",
                                               values_to = "w_rank") %>%
                                  group_by(id) %>%
                                  filter(!is.na(w_rank)) %>%
                                  summarise(rank_avg = mean(w_rank)) %>%
                                  mutate(rank_me = min_rank(rank_avg)), ignoreNULL = FALSE)
  
  # Show the first "n" observations ----
  output$table <- renderTable({
    dat <- datasetInput()
    dat <- full_join(df, dat)
    dat %>%
      arrange(rank_me) %>%
      rename(Rank = rank_me) %>%
      select(Rank, County, temp_jan, temp_jul, sun, 
             rural_urban_cont_2013, education, life, unemployment) %>%
      head(20)

  })
  
  output$distPlot <- renderPlotly({
    
    dat <- datasetInput()
    
    dat <- full_join(df, dat)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = col2rgb('white')
    )
    
    fig <- plot_ly(width = "1200px", height = "1000px") %>% 
      add_trace(
        type = "choropleth",
        geojson = counties,
        locations = str_pad(dat$id, 5, pad = "0"),
        z = dat$rank_me,
        colorscale = "Viridis",
        zmin = 1,
        zmax = max(dat$rank_me, na.rm = TRUE),
        text = dat$County,
        marker = list(line = list(width = 0))) %>% 
      colorbar(title = "Rank") %>% 
      layout(legend = list(x = 0.1, y = 0.9)) %>%
      # layout(autosize = F,
      #        margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)) %>%
      layout(geo = g)
    
    fig
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)