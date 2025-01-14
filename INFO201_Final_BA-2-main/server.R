library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(leaflet)
library(TidyDensity)
library(tidyverse)
library(DT)
server <- function(input, output, session){
  # Tab 1
  observeEvent(input$select_all, {
    updateSelectInput(session, "genre_selection", 
                      selected = sort(unique(unlist(strsplit(joined_data$Listed_in, ", ")))))
  })
  
  observeEvent(input$start_1995, {
    updateSliderInput(session, "digit_count_2", 
                      value = c(1995, max(joined_data$Release_year)))
  })
  
  output$scatterPlot <- renderPlotly({
    # Filter the dataset based on selected genres
    genres_to_filter <- c(input$genre_selection)
    
    # Create a single pattern string by collapsing the genres list with the OR operator
    pattern <- paste(genres_to_filter, collapse = '|')
    
    selected_df <- joined_data %>%
      filter(str_detect(Listed_in, pattern)) %>% 
      filter(Release_year >= input$digit_count[1] & Release_year <= input$digit_count[2])
    
    
    if (input$color_graph) {
      genre_point <- ggplot(selected_df, aes(x = Listed_in, y = Release_year)) +
        geom_point(aes(color = Listed_in, text = paste("Title:", Title, "<br>Year:", Release_year)), size = 3) + 
        scale_color_viridis_d() + # Use a vibrant color scale
        theme_minimal() +
        geom_smooth(aes(group = 1)) +
        labs(x = "Genre", y = "Release Year", title = "Scatter Plot by Genre") +
        theme(plot.title = element_text(size = 16, face = "bold"), 
              axis.title = element_text(size = 12),
              legend.text = element_text(size = 5), # Make legend text smaller
              legend.title = element_text(size = 12),
              legend.position = "bottom",
              axis.text.x = element_blank(),
              plot.background = element_rect(fill = "#f7f7f7"), # Change plot background
              panel.background = element_rect(fill = "#ffffff"), # Change panel background
              panel.grid.major = element_line(color = "#e5e5e5"), # Change color of major grid lines
              panel.grid.minor = element_blank(), # Remove minor grid lines
              plot.margin = margin(20, 20, 20, 20) # Adjust plot margin
        ) 
      
    } else {
      genre_point <- ggplot(selected_df, aes(x = Listed_in, y = Release_year)) +
        geom_point() +
        labs(x = "Genre", y = "Release Year", title = "Scatter Plot by Genre") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold"), 
              axis.title = element_text(size = 12),
              legend.text = element_text(size = 5), # Adjust legend text size
              legend.title = element_text(size = 12),
              legend.position = "bottom",
              axis.text.x = element_blank() # Remove x-axis labels
        ) 
    }
    
    return(ggplotly(genre_point, tooltip = "text") )
  })
  
  # Tab 2
  output$county_counts <- renderPlotly({
    # Dynamically aggregate data after filtering out NA values
    dynamic_df <- joined_data %>%
      filter(Country == input$country, !is.na(Country), !is.na(Age)) %>% # Filter based on selected country and remove NA values
      group_by(Age) %>% # Group by Age rating
      summarise(Quantity = n()) # Count the number of movies per Age rating
    
    # Plotting
    my_plot <- ggplot(dynamic_df, aes(x = Age, y = Quantity, fill = Age)) +
      geom_col() +
      labs(title = paste("Age Rating Distribution in", input$country),
           x = "Age Rating",
           y = "Number of Movies") +
      theme_minimal()
    
    return(ggplotly(my_plot))
  })
  
  
  # Tab 3
  output$density_plot <- renderPlot({
    selected_df_2 <- joined_data %>%
      filter(Release_year >= input$digit_count_2[1] & Release_year <= input$digit_count_2[2])
    
    if (input$y_axis == "Release_year") {
      genre_point <- ggplot(selected_df_2, aes(Overall_score, fill = factor(Release_year))) +
        geom_density(alpha = 0.8) +
        labs(
          title = paste("Density plot of Overall_score grouped by", input$y_axis),
          subtitle = "*Note: Where Overall_score is determined by the average of IMDb score and Rotten.tomatoes. Start from year 1995 for better visualizations.",
          caption = "Release_year is the year at which the Movie/TV Show has been released on Netflix",
          x = "Overall Score",
          fill = "Year"
        ) +
        theme_minimal() +
        scale_fill_discrete(name = "Year") +
        geom_vline(xintercept = mean(selected_df_2$Overall_score), color = "red", linetype = "dashed") +
        geom_text(aes(x = mean(selected_df_2$Overall_score), label = "Mean"), y = 0.1, vjust = -1, color = "red") +
        theme(legend.position = "bottom", legend.box = "horizontal") # Adjust legend position and orientation
    } else if (input$y_axis == "Cross_platform_score") {
      genre_point <- ggplot(selected_df_2, aes(Overall_score, fill = factor(Cross_platform_score))) +
        geom_density(alpha = 0.8) +
        labs(
          title = paste("Density plot of Overall_score grouped by", input$y_axis),
          subtitle = "*Note: Where Overall_score is determined by the average of IMDb score and Rotten.tomatoes. Start from year 1995 for better visualizations.",
          caption = "Cross Platform Score is determined by how ubiquotious the TV / Movie Show is on other streaming platforms, on a scale of 0 - 1",
          x = "Overall Score",
          fill = "Year"
        ) +
        theme_minimal() +
        scale_fill_discrete(name = "Cross_platform_score") +
        geom_vline(xintercept = mean(selected_df_2$Overall_score), color = "red", linetype = "dashed") +
        geom_text(aes(x = mean(selected_df_2$Overall_score), label = "Mean"), y = 0.1, vjust = -1, color = "red")
    } else if (input$y_axis == "Age_years") {
      genre_point <- ggplot(selected_df_2, aes(Overall_score, fill = factor(Age_years))) +
        geom_density(alpha = 0.8) +
        labs(
          title = paste("Density plot of Overall_score grouped by", input$y_axis),
          subtitle = "*Note: Where Overall_score is determined by the average of IMDb score and Rotten.tomatoes. Start from year 1995 for better visualizations.",
          caption = "Age_years is determined by how long TV Show or Movie has been released for",
          x = "Overall Score",
          fill = "Year"
        ) +
        theme_minimal() +
        scale_fill_discrete(name = "Age_years") +
        geom_vline(xintercept = mean(selected_df_2$Overall_score), color = "red", linetype = "dashed") +
        geom_text(aes(x = mean(selected_df_2$Overall_score), label = "Mean"), y = 0.1, vjust = -1, color = "red")
    } else if (input$y_axis == "Rating_category") {
      genre_point <- ggplot(selected_df_2, aes(Overall_score, fill = factor(Rating_category))) +
        geom_density(alpha = 0.8) +
        labs(
          title = paste("Density plot of Overall_score grouped by", input$y_axis),
          subtitle = "*Note: Where Overall_score is determined by the average of IMDb score and Rotten.tomatoes. Start from year 1995 for better visualizations.",
          caption = "Rating_category is the movie classification ratings",
          x = "Overall Score",
          fill = "Year"
        ) +
        theme_minimal() +
        scale_fill_discrete(name = "Rating_category") +
        geom_vline(xintercept = mean(selected_df_2$Overall_score), color = "red", linetype = "dashed") +
        geom_text(aes(x = mean(selected_df_2$Overall_score), label = "Mean"), y = 0.1, vjust = -1, color = "red")
    }  else if (input$y_axis == "Country") {
      genre_point <- ggplot(selected_df_2, aes(Overall_score, fill = factor(Country))) +
        geom_density(alpha = 0.8) +
        labs(
          title = paste("Density plot of Overall_score grouped by", input$y_axis),
          subtitle = "*Note: Where Overall_score is determined by the average of IMDb score and Rotten.tomatoes. Start from year 1995 for better visualizations.",
          caption = "By Group BA-2",
          x = "Overall Score",
          fill = "Year"
        ) +
        theme_minimal() +
        scale_fill_discrete(name = "Country") +
        geom_vline(xintercept = mean(selected_df_2$Overall_score), color = "red", linetype = "dashed") +
        geom_text(aes(x = mean(selected_df_2$Overall_score), label = "Mean"), y = 0.1, vjust = -1, color = "red")
    } 
    
    print(genre_point)
  })
}