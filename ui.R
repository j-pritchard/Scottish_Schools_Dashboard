ui <- fluidPage(
  
  tabsetPanel(
    # Tab 1 - Map
    tabPanel("Map", fluidPage(
      fluidRow(tags$h3("Scottish Schools Map")),
      
      fluidRow(
        column(9, leafletOutput("map", height = 500)),
        
        column(3, checkboxGroupInput(inputId = "type_input",
                                     label = "Type of School",
                                     choices = c("Primary",
                                                 "Secondary",
                                                 "Special"),
                                     select = "Secondary",
                                     inline = TRUE),
               
               shinyWidgets::pickerInput(inputId = "la_input",
                                         label = "Local Authorities",
                                         choices = la_list,
                                         select = "Scottish Borders",
                                         options = list("actions-box" = TRUE),
                                         multiple = TRUE),
               
               sliderInput(inputId = "roll_input",
                           label = "Number of Pupils",
                           min = 0,
                           max = 2500,
                           value = c(0, 2500)))),
    )),
    
    # Tab 2 -
    tabPanel("Local Authorities", fluidPage(
      fluidRow(tags$h3("Scottish Local Authorities")),
      fluidRow(
        "In Scotland, local authorities oversee the delivery of education and of
        early learning and childcare in the 32 council areas. The make-up of the
        local authorities varies, with some being compact urban areas, while
        others span vast rural areas. In this tab, the plots compare some
        statistics for schools in each local authority.", br(), br()),
      fluidRow(
        radioButtons(inputId = "la_input_tab2",
                     label = "Comparison",
                     choices = c("Number of Schools",
                                 "Median Pupil Roll",
                                 "Mean Deprivation"),
                     inline = TRUE)),
      
      fluidRow(plotOutput("la_faceted"))
    )),
    
    # Tab 3 -
    tabPanel("Deprivation", fluidPage(
      fluidRow(tags$h3("Deprivation Rank")),
      fluidRow("It has been a Scottish government goal, since 2015, to close the
               gap in attainment between pupils from deprived areas and those
               from less deprived areas.", br(),  
               "Schools are assigned a deprivation ranking based on the Scottish
               Index of Multiple Deprivation (SIMD) 2020. The rank number
               indicates the rank of the data zone which the school is located
               in, with 0 being the most deprived areas, and 100 being least
               deprived.", br(),  
               "The graphs on this page examine whether there is a correlation
               between deprivation and other factors. Only secondary schools are
               shown in the tables as there are clearer attainment metrics to
               measure these against. In this case the proportion of pupils
               gaining at least five higher qualifications has been used (known
               as \"5@6\").", br(), br()),
      fluidRow(
        radioButtons(inputId = "factor_input",
                     label = "Factor",
                     choices = c("Attainment", "Pupil:Teacher Ratio", "Rank"),
                     inline = TRUE)),
      fluidRow(column(6, plotOutput("dep_scatter", height = 300)),
               column(6, plotOutput("dep_column", height = 300))),
      fluidRow(
        textOutput("dep_conclusion")
      )
    )),
    
    # Tab 4 - About
    tabPanel("About",
             fluidPage(
               fluidRow(tags$h3("Schools Data"),
    "Information about Scottish schools is updated by the Scottish Government
    annually for the purposes of monitoring overall performance, equality and
    individual policies. The dataset is reflective of the July 2022 schools
    locations data (published 10th October 2022) and July 2022 school roll,
    FTE teachers, proportion minority background, and proportion 20% most
    deprived data (published 21st March 2023). The data can be found ",
    tags$a("here",
    href = "https://www.data.gov.uk/dataset/9a6f9d86-9698-4a5d-a2c8-89f3b212c52c/scottish-school-roll-and-locations#licence-info"),
    ".", br(),
    br(),
    tags$h3("Attainment Data"),
    "School attainment data was taken from an article appearing in the Scottish
    Daily Express on 26th May 2023 titled ",
    tags$a("Scottish school league table 2023: Full list of the best-performing
    high schools in Scotland",
    href = "https://www.scottishdailyexpress.co.uk/news/scottish-news/scottish-school-league-table-full-30081502"),
    ". The data supplied in the article originated from the Scottish Government
    web site, which was then analysed by the Scotsman newspaper before appearing
    in the Daily Express. Judging schools on just one metric can be misleading
    as it oversimplifies all of the achievements of pupils from a range of
    backgrounds. However, media and parents continue to use this approach to
    rank secondary schools.")
             ))
    )
  )