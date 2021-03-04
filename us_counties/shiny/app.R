library(shiny)
library(plotly)
library(rjson)
library(tidyverse)

df <- read_delim(url("https://tsoleary.github.io/gym_class/us_counties/data/clean/combined_data.tsv"), delim = "\t")

df_rank <- df %>% mutate(rank_income = min_rank(desc(income)),
         rank_education = min_rank(desc(education)),
         rank_life = min_rank(desc(life)),
         rank_obesity = min_rank(obesity),
         rank_disability = min_rank(disability),
         rank_unemployment = min_rank(unemployment)) 

counties <- rjson::fromJSON(file='https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json')

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Where in the US are you best suited to live?"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Include clarifying text ----
      helpText("How much do you care about the following things,",
               "in the place you live? A 0 indicates you don't care",
               "at all and a 5 indicates that it is very important to you."),
      
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "obs_w",
                  label = "Obesity:",
                  min = 0,
                  max = 5,
                  value = 5),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "une_w",
                  label = "Unemployment:",
                  min = 0,
                  max = 5,
                  value = 5),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "dis_w",
                  label = "Disability:",
                  min = 0,
                  max = 5,
                  value = 5),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "inc_w",
                  label = "Income:",
                  min = 0,
                  max = 5,
                  value = 5),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "edu_w",
                  label = "Education:",
                  min = 0,
                  max = 5,
                  value = 5),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "lif_w",
                  label = "Life Expectancy:",
                  min = 0,
                  max = 5,
                  value = 5),
      
      actionButton("update", "Update Map!")
      
      ),

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotlyOutput(outputId = "distPlot", width = "100%")
      
    )
  )
)

# Define server logic required to draw plot
server <- function(input, output) {
  
  datasetInput <- eventReactive(input$update,
    df_rank %>%
      mutate(w_rank_dis = rank_disability * input$dis_w,
             w_rank_inc = rank_income * input$inc_w,
             w_rank_obs = rank_obesity * input$obs_w,
             w_rank_une = rank_unemployment * input$une_w,
             w_rank_edu = rank_education * input$edu_w,
             w_rank_lif = rank_life * input$lif_w) %>%
      pivot_longer(contains("w_rank_"),
                   names_to = "type",
                   values_to = "w_rank") %>%
      group_by(id) %>%
      summarise(rank_avg = mean(w_rank)) %>%
      mutate(rank_me = min_rank(rank_avg)), ignoreNULL = FALSE)
  
  
  output$distPlot <- renderPlotly({
    
    dat <- datasetInput()
    
    dat <- full_join(df, dat)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = col2rgb('white')
    )
    
    fig <- plot_ly() %>% 
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
      layout(autosize = F,
             margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)) %>%
      layout(geo = g)
    
    fig
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)