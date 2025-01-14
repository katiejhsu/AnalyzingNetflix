library(ggplot2)
library(plotly)
library(bslib)
library(shinythemes)
library(shiny)
library(htmltools)
library(leaflet)
library(devtools)
library(slickR)
library(htmlwidgets)
library(shiny)
library(TidyDensity)
library(tidyverse)
library(DT)
library(rsconnect)

joined_data <- read.csv("joined_data.csv")

plotlyOutput(plot_ly(x = diamonds$cut) %>%
               config(displayModeBar = FALSE) %>%
               layout(margin = list(t = 0, b = 0, l = 0, r = 0))) 

leafletOutput(leafletOptions(attributionControl = FALSE) %>%
                leaflet(options = .) %>%
                addTiles())

## OVERVIEW TAB INFO
imgbanner <- tags$div(
  class = "image-banner",
  tags$img(src = "netflix.png", alt = "Image 1", class = "active"),
  tags$img(src = "netflix2.jpg", alt = "Image 2"),
  tags$img(src = "netflix3.png", alt = "Image 3"),
  tags$img(src = "netflix4.jpg", alt = "Image 4"),
  tags$img(src = "netflix5.jpg", alt = "Image 5"),
  tags$script(HTML("$(document).ready(function() {
                      var currentIndex = 0;
                      var images = $('.image-banner img');
                      setInterval(function() {
                        images.eq(currentIndex).removeClass('active');
                        currentIndex = (currentIndex + 1) % images.length;
                        images.eq(currentIndex).addClass('active');
                      }, 3000); 
                    });
                  "))
)

netflix_gif <- tags$div(class = "netflix-heading", style = "display: flex; justify-content: center; padding: 0;",
                        tags$img(src = 'netflix.gif', alt = "Heading", style = "width: 200px; height: auto;")
)

jumbotron <- tags$div(class = "jumbotron text-center", style = "margin-bottom:0px;margin-top:-50px;margin-left:14px;justify-content: center; background-color:black;",
                      tags$h1(class = 'jumbotron-heading', style = 'font-size: 50px; margin-bottom:0px;margin-top:0px; text-align: center; padding-bottom: 5vh;font-weight: bold; color: #B81D24;', 'Analyzing NETFLIX Media Products'),
                      # tags$p(style = 'font-size: 18px; text-align: center; color: grey; font-weight: bold;', 'Our project will analyze the several well-known Netflix movies and TV series for their performance in Netflix as well as in other various platforms.'),
                      tags$p(style = 'font-size: 18px; text-align: center; color: grey; font-weight: bold;', "Our project will analyze the several well-known Netflix movies and TV series for their overall ratings when compared against several different variables, including its release date, its genre, its country of production, and its prevalence in other streaming platforms. The datasets that we are using are a combination of data from two
datasets: one regarding TV shows that has data on
rating and which streaming platforms it is available
on, and the other regarding media on Netflix (both
TV shows and movies). Its purpose is to have one
unified dataset that has important rating information
regarding many different types of media (not just tv
shows), from many different streaming platforms
(not just Netflix) to have a more clear understanding
of media ranked against each other. However, some limitations of these datasets include some movies being released earlier than others, thus they have more accurate rating comapred to the movies or TV Shows that are released later on; some movies don't appear in other platforms such as Hulu or Disney, thus they have less audience awareness and exposure, thus might potentially be less rated by other people; and also IMDb and Rotten Tomatoes rating has only been widely used since around the year 1995, hence limiting some data visualizations." ),
)

sidebar_overview_1 <- div(class = "custom-sidebar",
                          h1("Dataset 1"),
                          p("Name: Netflix Shows"),
                          p("Source: ", tags$a(href="https://www.kaggle.com/datasets/shivamb/netflix-shows", "Kaggle")),
                          p("Number of Observations: 8807"),
                          p("Number of Columns: 12"),
                          tags$a(href="https://www.kaggle.com/datasets/shivamb/netflix-shows", class="btn btn-primary learn-more-btn", role="button", 
                                 style = "background-color: #B81D24; border-color:black;", "Learn More")
                          
)
sidebar_overview_2 <- div(class = "custom-sidebar-2",
                          h1("Dataset 2"),
                          p("Name: TV shows on Netflix, Prime Video, Hulu and Disney+"),
                          p("Source: ", tags$a(href="https://www.kaggle.com/datasets/ruchi798/tv-shows-on-netflix-prime-video-hulu-and-disney", "Kaggle")),
                          p("Number of Observations: 5368"),
                          p("Number of Columns: 12"),
                          tags$a(href="https://www.kaggle.com/datasets/ruchi798/tv-shows-on-netflix-prime-video-hulu-and-disney", class="btn btn-primary learn-more-btn", role="button", 
                                 style = "background-color: #B81D24; border-color:black;", "Learn More")
)

centered_sidebars <- div(class = "sidebar-container",
                         sidebar_overview_1,
                         sidebar_overview_2
)

names <- div(class = "names", style = "justify-content: center; align-items: center; text-align: center;",
             h4("CREATED BY", style = "color:#B81D24; font-weight: bold;"),
             p("Agnes Bisma, Enrico Pratama, Katie Hsu, and Ryan Nguyen", style = "color: grey; font-weight: bold; font-size: 18px; padding-bottom: 50px;"))

overview_tab <- tabPanel("Project Overview",
                         imgbanner,
                         netflix_gif,
                         jumbotron,
                         centered_sidebars,
                         names
)

## VIZ 1 TAB INFO
declaration_text_viz1 <- tags$div(class = "jumbotron text-center", style="margin-top: 10px;",
                                  tags$p(style = 'font-size: 16px; text-align: left;', 'This option for choosing the category will select movies or TV shows which have the selected genre was one of their genres, given that each movie or TV show can contain more than one genre. When hovering each individual scatter point, a pop-up displays the title of each movie and its release year on Netflix. From this visualization, we can summarize that most of the movies with most genres are produced in around the year 2000 and after which is shown by the more straighter line produced when selecting all genres. Going into more detail, we can also deduce that the best fit line here is used to determine the stability of the years at which each genre is released, with a straigter line meaning that there is a specific year at which that specific genre is mosty produced during. We have summarized that these genres with the least straight lines stood out the most: Anime Series, Classic Movies, Documentaries, International Movies, Science & Nature TV, Stand-up Comedy & Talk Shows, Teen TV Shows, and TV Sci-Fi & Fantasy.'),
)

heading_viz1 <- tags$div(class = "jumbotron text-center", style="margin-top: 10px;",
                         # tags$h2(class = 'jumbotron-heading', style = 'font-weight: bold;', 'Genre-Year Analysis'),
                         tags$p(style = 'font-size: 14px; text-align: middle;', 'Provides an analysis for the Netflix movies or TV shows with the chosen category(s) with their release year date.')
)



year_range_selector <- checkboxInput("digit_type", "Add year range?", value = FALSE)

# Conditional panel that shows the slider input when the checkbox is checked
condition_panel <- conditionalPanel(
  condition = "input.digit_type == true", 
  sliderInput("digit_count", "Select Year Range",
              min = min(joined_data$Release_year), 
              max = max(joined_data$Release_year), 
              step = 1,
              value = c(min(joined_data$Release_year), max(joined_data$Release_year)))
)


viz_1_sidebar <- sidebarPanel(
  titlePanel("Movies Scatter Plot Filtered by Genre"),
  selectInput(inputId = "genre_selection", 
              label = "Choose Movies Containing Genres:", 
              choices = sort(unique(unlist(strsplit(joined_data$Listed_in, ", ")))),
              multiple = TRUE,
              selected = "Action & Adventure"),
  actionButton("select_all", "Select All Genres"),
  h2("Options for scatter plot"),
  checkboxInput(
    inputId = "color_graph",
    label = "Add colors, label, and best fit line?"
  ),
  year_range_selector,
  condition_panel
)

viz_1_main_panel <- mainPanel(
  plotlyOutput(outputId = "scatterPlot"),
  declaration_text_viz1
)

viz_1_tab <- tabPanel("Genre-Year Analysis",
                      # title_panel,
                      sidebarLayout(
                        viz_1_sidebar,
                        viz_1_main_panel
                      )
)

## VIZ 2 TAB INFO

declaration_text_viz2 <- tags$div(class = "jumbotron text-center", style="margin-top: 10px;",
                                  tags$p(style = 'font-size: 16px; text-align: left;', 'This visualization is a bar graph that shows the number of movies with different types of age ratings in different countries.
When hovering over each individual bar, the amount of movies in that age range is displayed.
From these 5 visualizations, we can make a conclusion that Western countries (in our example the US and UK) in general have a higher average age rating, with the most movies having an 18+ rating. On the other hand, we can make a conclusion that Asian countries (in our example Japan, South Korea and China), have a lower average age rating than the western countries, with most movies having a rating 16+ in Japan and South Korea, and 7+ in China.
'),
)

viz_2_sidebar <- sidebarPanel(
  h2("Available Countries To Choose From:"),
  selectInput(inputId = "country",
              label = h3("Choose Country"),
              choices = sort(unique(joined_data$Country)), 
              selected = "United States") 
)


viz_2_main_panel <- mainPanel(
  h2("Age Rating Per Country"),
  plotlyOutput(outputId = "county_counts"),
  declaration_text_viz2
)

viz_2_tab <- tabPanel("Age-Rating & Country Analysis",
                      sidebarLayout(
                        viz_2_sidebar,
                        viz_2_main_panel
                      )
)

## VIZ 3 TAB INFO

declaration_text_viz3 <- tags$div(class = "jumbotron text-center", style="margin-top: 10px;",
                                  tags$p(style = 'font-size: 16px; text-align: left;', 'This visualization is a density plot that shows the performance of all Netflix TV Shows and Movies compared against a second variable that the user chooses on the left. For the default comparison, this represents the distribution of Overall Score for TV shows or movies, grouped by their Age_years which is how long the product has been released for. The Overall Score is calculated based on the average of IMDb and Rotten Tomatoes scores. The Age_years is determined by the number of years a TV show or movie has been released, with the data starting from the year 1995 for clearer visualizations. From the data density plot, we have deduced that most movies are rated during the year 1995 above, which makes sense since the Rotten Tomatoes rating has only been widely used during the year 1998, and IMDb since the year 1990. Based on the density of the data, distribution peaks are around the 0.6 to 0.8 Overall Score range, showing that a majority of TV shows or movies have scores within this range despite their age. Also, the spread of scores seems to be wider for shows or movies that have been released for a longer period, suggesting that ratings for older shows and movies are more diverse. In terms of the density (the y-axis), shows or movies with an age of around 16 to 22 years have the highest peaks, which means they have a higher frequency of similar scores around the mode. We found it interesting that the density decreases for very new and very old shows or movies, possibly indicating a broader range of opinions for those age groups.'),
)


viz_3_sidebar <- sidebarPanel(
  h2("Overall_score vs. (comparison variable)"),
  p("*IMDb & Rotten Tomatoes has only been widely used since 1995, hence we recommend that you start from the year 1995"),
  sliderInput("digit_count_2", "Select Year Range",
              min = min(joined_data$Release_year), 
              max = max(joined_data$Release_year), 
              step = 1,
              value = c(min(joined_data$Release_year), max(joined_data$Release_year))
  ),
  actionButton("start_1995", "Start from 1995"),
  selectInput("y_axis", "Y Axis:", choices = sort(c("Release_year", "Cross_platform_score", "Age_years", "Rating_category", "Country")))
)

viz_3_main_panel <- mainPanel(
  plotOutput("density_plot"),
  declaration_text_viz3
)

viz_3_tab <- tabPanel("Overall Score Analysis",
                      sidebarLayout(
                        viz_3_sidebar,
                        viz_3_main_panel
                      )
)

## CONCLUSIONS TAB INFO

conclusion_tron <- tags$div(class = "jumbotron text-center", style="margin-top: 10px;",
                            tags$h1(style = 'font-size: 30px; text-align: left;', 'Our findings: '),
                            tags$p(style = 'font-size: 14px; text-align: left;', 'In our examination of Netflix media products and our three aspects of genre distributions over time, age ratings across countries and performance score over time. We have found insightful trends and patterns that shed light on consumer preferences and industry practices. '),
                            tags$h2(style = 'font-size: 30px; text-align: left;', 'Trending genres vs Classic genres: '),
                            tags$p(style = 'font-size: 14px; text-align: left;', 'In our visualization of Genre-Year analysis we conducted how specific genres like Action & Adventure, comedies, crime tv shows experience fluctuations in popularity over time. We observed distinct periods where these genres are “Trending” by the surge in the number of movies and Tv Shows featuring these genres. While classic genres like Anime, Documentaries, Sci-Fi & Fantasy maintain a consistent presence throughout time. 
'),
                            tags$h2(style = 'font-size: 30px; text-align: left;', 'Cultural differences influences content rating worldwide: ' ),
                            tags$p(style = 'font-size: 14px; text-align: left;', 'In our visualization of Age Rating per country we found variations of age ratings that reflect diverse societal attitudes towards mature content and censorship, highlighting the importance of tailoring their content to meet the specific preferences and sensitivities of different audiences. Understanding these cultural nuances is crucial for producers and creators to ensure that their content is more suitable and appealing to viewers worldwide. '),
                            tags$h2(style = 'font-size: 30px; text-align: left;', 'Quality of media products over time: ' ),
                            tags$p(style = 'font-size: 14px; text-align: left;', "In our Overall score analysis we found that since 1995 with the use of IMBd and Rotten tomatoes suggest that the quality of Movies/TV Shows have stayed consistent over time. However, as the content ages the spread of scores widens, indicating a more diverse range of rating influenced by the change in audience's taste and assessments. Shows and Movies around 16 to 22 years old have more people agreeing about the quality of the product, while newer and older ones have more varied opinions that shows changes in what people like. Understanding these trends helps both people making the content and people watching it. "))

conclusion_tab <- tabPanel(
  "Conclusion Tab",
  conclusion_tron
)


ui <- navbarPage(theme = shinytheme("darkly"),
                 includeCSS("www/styles.css"),
                 tags$style(HTML("body { background-color: #000; }")),
                 collapsible = TRUE,
                 windowTitle = "Analyzing Netflix Products",
                 overview_tab,
                 viz_1_tab,
                 viz_2_tab,
                 viz_3_tab,
                 conclusion_tab
)