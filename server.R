server <- function(input, output, session) {
  
  ### Tab 1 - Map
  output$map <- renderLeaflet({
    
    all_schools %>% 
      mutate(colour = case_when(school_type == "Primary" ~ "blue",
                                school_type == "Secondary" ~ "red",
                                .default = "green")) %>% 
      filter(la_name %in% input$la_input,
             school_type %in% input$type_input,
             pupil_roll %in% c(input$roll_input[1]:input$roll_input[2])) %>%
      leaflet() %>% 
      addTiles() %>%
      addCircleMarkers(lat = ~latitude, lng = ~longitude,
                       radius = 4, weight = 2,
                       popup = ~school_name,
                       color = ~colour)
    })
  
  ### Tab 2 - Local Authorities
  output$la_faceted <- renderPlot({
    
    if(input$la_input_tab2 == "Number of Schools") {
      la_plot <- all_schools %>% 
        group_by(la_name) %>% 
        mutate(la_school_count = n()) %>% 
        ungroup() %>% 
        ggplot(aes(x = reorder(la_name, la_school_count),
                   fill = la_name)) +
        geom_bar(show.legend = FALSE) +
        labs(x = "",
             y = "Number of Schools") +
        coord_flip() +
        facet_grid(~school_type, scale = "free")
    }
    
    if(input$la_input_tab2 == "Median Pupil Roll") {
      la_plot <- all_schools %>% 
        group_by(la_name, school_type) %>% 
        summarise(median_roll = round(median(pupil_roll), 0)) %>%  
        ggplot(aes(x = reorder(la_name, median_roll), y = median_roll,
                   fill = la_name)) +
        geom_col(show.legend = FALSE) +
        labs(x = "",
             y = "Median Pupil Roll") +
        coord_flip() +
        facet_grid(~school_type, scales = "free")
    }
    
    if(input$la_input_tab2 == "Mean Deprivation") {
      la_plot <- all_schools %>% 
        group_by(la_name) %>% 
        summarise(mean_deprivation = round(mean(dzsimd_percentile), 0)) %>%  
        ggplot(aes(x = reorder(la_name, mean_deprivation), y = mean_deprivation,
                   fill = la_name)) +
        geom_col(show.legend = FALSE) +
        labs(x = "",
             y = "Mean Deprivation Rank") +
        coord_flip()
    }
    
    la_plot
  })
  
  
  ### Tab 3 - Deprivation
  # Scatter diagrams
  output$dep_scatter <- renderPlot({
    
    if(input$factor_input == "Attainment") {
      scatter_plot <- sec_school_rank %>% 
        ggplot(aes(x = dzsimd_percentile, y = five_highers)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = "Deprivation Ranking",
             y = "Proportion of Pupils Gaining at Least Five Highers") +
        scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1))
    }
    
    if(input$factor_input == "Pupil:Teacher Ratio") {
      scatter_plot <- sec_school_rank %>% 
        ggplot(aes(x = dzsimd_percentile, y = pupil_teacher_ratio)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = "Deprivation Ranking",
             y = "Pupils per Teacher")
    }
    
    if(input$factor_input == "Rank") {
      scatter_plot <- sec_school_rank %>% 
        ggplot(aes(x = dzsimd_percentile, y = rank)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = "Deprivation Ranking",
             y = "Rank")
    }
    
    scatter_plot
  })
  
  # Column Plots
  output$dep_column <- renderPlot({
    
    if(input$factor_input == "Attainment") {
      col_plot <- sec_school_rank %>% 
        mutate(dzsimd_percentile = round(dzsimd_percentile, -1)) %>% 
        group_by(dzsimd_percentile) %>% 
        summarise(mean_five_highers = mean(five_highers)) %>% 
        ggplot(aes(x = dzsimd_percentile, y = mean_five_highers)) +
        geom_col() +
        labs(x = "Deprivation Ranking",
             y = "Proportion of Pupils Gaining at Least Five Highers") +
        scale_x_continuous(breaks = seq(0, 100, 10)) +
        scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.6, 0.1))
    }
    
    if(input$factor_input == "Pupil:Teacher Ratio") {
      col_plot <- sec_school_rank %>% 
        mutate(dzsimd_percentile = round(dzsimd_percentile, -1)) %>% 
        group_by(dzsimd_percentile) %>% 
        summarise(mean_pt_ratio = mean(pupil_teacher_ratio)) %>% 
        ggplot(aes(x = dzsimd_percentile, y = mean_pt_ratio)) +
        geom_col() +
        labs(x = "Deprivation Ranking",
             y = "Average Pupils per FTE Teacher") +
        scale_x_continuous(breaks = seq(0, 100, 10)) +
        scale_y_continuous(breaks = seq(0, 20, 2))
    }
    
    if(input$factor_input == "Rank") {
      col_plot <- sec_school_rank %>% 
        mutate(dzsimd_percentile = round(dzsimd_percentile, -1)) %>% 
        group_by(dzsimd_percentile) %>% 
        summarise(mean_rank = mean(rank)) %>% 
        ggplot(aes(x = dzsimd_percentile, y = mean_rank)) +
        geom_col() +
        labs(x = "Deprivation Ranking",
             y = "Average Rank") +
        scale_x_continuous(breaks = seq(0, 100, 10)) +
        scale_y_continuous(breaks = seq(0, 300, 50))
    }
    
    col_plot
  })
  
  # Conclusions
  output$dep_conclusion <- renderText({
    
    if(input$factor_input == "Attainment") {
      dep_conclusion <-
        "The scatter diagram shows a very weak positive correllation, showing
      that schools in areas with lower deprevation, on average, have better
      pupil attainment. When schools in similar deprivation groups are
      aggregated, the column chart confirms that there is a considerable gap
      (over 20%) between attainment in the most and least deprived parts of
      Scotland. On this basis alone we might suggest that the Scottish
      government's drive to close the attainment gap still has some way to go."
    }
    
    if(input$factor_input == "Pupil:Teacher Ratio") {
      dep_conclusion <-
        "The scatter diagram shows very weak correllation, with an almost
      horizontal linear regression. When schools in similar deprivation groups
      are aggregated, the column chart shows little difference in the ratio
      of pupils to teachers in the groupings. Reassuringly, there is a
      marginally better pupil:teacher ratio in the most deprived areas. The 
      \"Attainment Scotland Fund\" is intended to introduce measures that may 
      help pupil attainment in the most deprived areas but there is little 
      evidence that reducing class size has been a priority of head teachers.
      This is not surprising as studies have shown that reducing class sizes
      has a very low effect size compared to other measures that should be
      considered (Hattie, 2008)."
    }
    
    if(input$factor_input == "Rank") {
      dep_conclusion <-
        "The scatter diagram shows a very weak correllation, but the regression
      line has negative gradient showing that schools in areas with lower
      deprevation, on average, are ranked worse than schools in the least
      deprived areas. When schools in similar deprivation groups are
      aggregated, the column chart confirms that there is a clear trend towards
      areas of high deprivation being ranked worse, with their counterparts in
      areas of low deprivation being ranked amongst the best. Given that the
      rankings are based on the same attainment data as the first graphs, it is
      not a surprise that we reach the same conclusion."
    }
    dep_conclusion
  })
  
}