library(shiny) 
library(shinyjs) 
library(plotly) 
library(dplyr) 
library(ggplot2) 
library(GeomMLBStadiums) 
library(sabRmetrics) 
library(tools) 
library(paletteer) 
library(MASS) 


ui <- fluidPage( 
  useShinyjs(), 
  titlePanel("Hitter Profiler"), 
  sidebarLayout( 
    sidebarPanel( 
      width = 3, 
      dateRangeInput(
        "date_range", "Date Range",
        start = min(savant_data_2025$game_date, na.rm = TRUE),
        end = max(savant_data_2025$game_date, na.rm = TRUE)
      ),
      selectInput(
        "player_name", "Select Player",
        choices = sort(unique(savant_data_2025$batter_name))
      ),
      selectInput(
        "stadium", "Select Stadium",
        choices = c("All", sort(unique(savant_data_2025$home_team_full))),
        selected = "All"
      ),
      checkboxGroupInput(
        "pitcher_hand", "Pitcher Handedness",
        choices = sort(unique(savant_data_2025$pitch_hand)),
        selected = sort(unique(savant_data_2025$pitch_hand))
      ),
      checkboxGroupInput(
        "balls", "Balls",
        choices = sort(unique(savant_data_2025$balls)),
        selected = sort(unique(savant_data_2025$balls))
      ),
      checkboxGroupInput(
        "strikes", "Strikes",
        choices = sort(unique(savant_data_2025$strikes)),
        selected = sort(unique(savant_data_2025$strikes))
      ),
      checkboxGroupInput(
        "pitch_type", "Select Pitch Type",
        choices = sort(unique(savant_data_2025$pitch_name))
      ),
      actionButton("view_video", "View Play Video", icon = icon("play"))
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Interactive Spray Chart",
          plotlyOutput("spraychart", height = "700px"),
          br(),
          uiOutput("video_player")
        ),
        tabPanel(
          "Pitch Location Heatmap",
          plotOutput("heatmap_plot", height = "700px", width = "600px")
        ),
        tabPanel(
          "Pitch Usage Tracker",
          plotlyOutput("usage_plot", height = "700px")
        )
        )
      )
    )
  )


server <- function(input, output, session) {
  
  heat_colors_interpolated <- colorRampPalette(
    paletteer::paletteer_d("RColorBrewer::RdBu", n = 9, direction = -1)
  )(16)
  
  values <- reactiveValues(
    closest_pitch = NULL,
    video_url = NULL
  )
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    req(input$date_range, input$player_name)
    df <- savant_data_2025 %>%
      mlbam_xy_transformation() %>%
      filter(
        batter_name == input$player_name,
        game_date >= input$date_range[1],
        game_date <= input$date_range[2],
        strikes %in% input$strikes,
        balls %in% input$balls,
        pitch_hand %in% input$pitcher_hand,
        pitch_name %in% input$pitch_type
      )
    if (input$stadium != "All") {
      df <- df %>% filter(home_team_full == input$stadium)
    }
    df
  })
  
  observe({
    req(input$player_name, input$date_range)
    
    df <- savant_data_2025 %>%
      mlbam_xy_transformation() %>%
      filter(
        batter_name == input$player_name,
        game_date >= input$date_range[1],
        game_date <= input$date_range[2]
      )
    
    if(nrow(df) > 0){
      df_summary <- df %>%
        group_by(pitch_name) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(percentage = count / sum(count) * 100)
      
      selected_pitches <- df_summary %>%
        filter(percentage >= 3) %>%
        pull(pitch_name)
      
      if(length(selected_pitches) == 0){
        selected_pitches <- df_summary %>% pull(pitch_name)
      }
      
      updateCheckboxGroupInput(
        session,
        "pitch_type",
        choices = sort(unique(df_summary$pitch_name)),
        selected = selected_pitches
      )
    }
  })
  
  # Spray Chart
  output$spraychart <- renderPlotly({
    df <- filtered_data()
    if(nrow(df) == 0){
      return(plotly_empty(type="scatter") %>% layout(
        title = list(text = "No hits for the selected player, stadium, or date range."),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(
          list(text = "No hits available", x = 0.5, y = 0.5, xref="paper", yref="paper", showarrow=FALSE, font=list(size=20))
        )
      ))
    }
    
    stadium_id <- if(input$stadium=="All") "generic" else input$stadium
    df <- df %>% mutate(hover_text = paste(
      "Result:", events,
      "<br>EV:", round(launch_speed,1),
      "<br>LA:", round(launch_angle,1),
      "<br>Distance:", hit_distance_sc,
      "<br>Pitch Type:", pitch_name,
      "<br>Stadium:", home_team_full,
      "<br>Date:", game_date,
      "<br>Play ID:", play_id
    ))
    
    g <- ggplot() +
      geom_spraychart(
        stadium_ids = stadium_id,
        stadium_transform_coords = TRUE,
        stadium_segments = "all"
      ) +
      geom_point(data=df, aes(x=hc_x_, y=hc_y_, color=launch_speed, text=hover_text), size=3) +
      scale_color_gradient(low="blue", high="red", name="Exit Velocity") +
      theme_void() + coord_fixed()
    
    ggplotly(g, tooltip="text", source="A") %>% event_register("plotly_click")
  })
  

  observeEvent(event_data("plotly_click", source="A"), {
    clickData <- event_data("plotly_click", source="A")
    req(clickData)
    df <- filtered_data()
    x <- clickData$x
    y <- clickData$y
    values$closest_pitch <- df[which.min((df$hc_x_ - x)^2 + (df$hc_y_ - y)^2),]
    values$video_url <- NULL
  })
  
  observeEvent(input$view_video, {
    req(values$closest_pitch)
    play_id <- values$closest_pitch$play_id[1]
    if(!is.null(play_id) && play_id != ""){
      video_url <- tryCatch({ sabRmetrics::get_video_url(play_id) }, error=function(e) NULL)
      if(!is.null(video_url) && video_url != ""){
        values$video_url <- video_url
      } else {
        showNotification("Video not available for this play.", type="warning")
      }
    }
  })
  
  output$video_player <- renderUI({
    req(values$video_url)
    tags$iframe(
      src = values$video_url,
      width = "100%",
      height = "480",
      frameborder = "0",
      allow = "autoplay; encrypted-media",
      allowfullscreen = NA
    )
  })
  
  output$heatmap_plot <- renderPlot({
    df <- filtered_data() %>% filter(is.finite(plate_x), is.finite(plate_z))
    if(nrow(df) == 0){
      g <- ggplot() + annotate("text", x=0, y=3, label="No data", size=6) + theme_void()
      return(g)
    }
    
    kzone_top <- mean(df$strike_zone_top, na.rm=TRUE)
    kzone_bottom <- mean(df$strike_zone_bottom, na.rm=TRUE)
    if(is.na(kzone_top)|is.na(kzone_bottom)){
      kzone_top <- 3.5
      kzone_bottom <- 1.5
    }
    kzone_left <- -17/24
    kzone_right <- 17/24
    
    ggplot(df, aes(x = plate_x, y = plate_z)) +
      stat_density_2d(
        aes(fill = after_stat(density)),
        geom = "tile",
        contour = FALSE,
        n = 200
      ) +
      scale_fill_gradientn(colours = heat_colors_interpolated) +
      
      annotate("rect", xmin = kzone_left, xmax = kzone_right,
               ymin = kzone_bottom, ymax = kzone_top,
               fill = NA, color = "black", linewidth = 0.6) +
      
      geom_segment(x = (-.708-(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
      geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
      geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
      geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
      geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
      coord_fixed(xlim = c(-2.5,2.5), ylim = c(-0.5,5), expand = FALSE) +
      scale_x_continuous(limits = c(-2.5, 2.5)) +
      scale_y_continuous(limits = c(-0.5, 5)) +
      theme_void() +
      labs(fill="Pitch Density")
  })
  
  
  

  output$usage_plot <- renderPlotly({
    df <- filtered_data()
    if(nrow(df)==0){
      return(plotly_empty(type="pie"))
    }
    
    df_summary <- df %>%
      group_by(pitch_name) %>%
      summarise(count=n(), .groups="drop") %>%
      mutate(percentage = round(count / sum(count) * 100, 1),
             label_text = paste0(pitch_name, ": ", percentage, "%"))
    
    plot_ly(df_summary, labels=~pitch_name, values=~percentage, type="pie",
            textinfo="label+percent", hoverinfo="text", text=~label_text)
  })


}

shinyApp(ui, server)

