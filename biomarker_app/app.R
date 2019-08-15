library(shiny)
library(tidyverse)
library(Cairo) # for better graphics

# TODO: remove brushing from plots that don't allow it.

set.seed(88)

# generate patient numbers, ages, genders, and treatment plans
simulated_data_population <- tibble(
  Patient = as.integer(1:200),
  Treatment = as.character(sample(
    x = c("Treatment 1", "Treatment 2", "Treatment 3"),
    size = 200, replace = TRUE
  )),
  Gender = sample(
    x = c("Female", "Male"),
    size = 200, replace = TRUE
  ),
  Age = as.integer(runif(n = 200, min = 18, max = 90))
)


# generate fake data based the above population
simulated_data <- replicate(n = 10, expr = simulated_data_population, simplify = FALSE) %>%
  bind_rows() %>%
  mutate(
    noise = rnorm(n = 2000, mean = 0, sd = 4),
    Time = sort(rep(0:9, 200)),
    Biomarker_1 = dplyr::case_when(
      Gender == "Female" & Treatment == "Treatment 3" ~ sin(Time) * 20 + 100 - 2 * Time + noise,
      Gender == "Male" & Treatment == "Treatment 3" ~ 70 + noise * 2,
      Treatment == "Treatment 2" ~ 50 + noise,
      Treatment == "Treatment 1" ~ 30 + noise
    ),
    Biomarker_2 = case_when(
      as.numeric(Patient) %% 2 == 1 ~ 70 + noise * 3,
      as.numeric(Patient) %% 2 == 0 ~ 20 + noise * 2
    ),
    Weight = case_when(
      Gender == "Female" ~ 130 + noise * 20,
      Gender == "Male" ~ 170 + noise * 30
    )
  ) %>%
  mutate(
    Treatment = factor(Treatment),
    Gender = factor(Gender),
    Time = factor(Time),
  ) %>%
  select(-noise)

# change default ggplot color options
options(ggplot2.continuous.colour = "viridis", shiny.usecairo = T)

# UI

ui <- navbarPage(
  "Mock Clinical Trial Dashboard by David Noonan",
  tabPanel(
    "Population Summary",
    titlePanel("Population Summary"),
    sidebarLayout(
      sidebarPanel(
        helpText("Patient summary by category"),
        hr(),
        selectInput("a1", "Category:",
          choices = simulated_data %>% select_if(function(x) is.factor(x)) %>% select(-Time) %>% colnames(), selected = "Gender"
        ),
        hr(),
        uiOutput("a2_reactive"),
        hr(),
        downloadButton("download_summary_plot", "Download Plot"),
        hr(),
        downloadButton("download_summary_table", "Download Table")
      ),
      # Create a spot for the barplot
      mainPanel(
        plotOutput("SummaryPlot"),
        tableOutput("SummaryTable")
      )
    )
  ),
  tabPanel(
    "Plot Variable Relationships",
    titlePanel("Plot Variable Relationships"),
    sidebarLayout(
      sidebarPanel(
        helpText("Select Variables"),
        hr(),
        selectInput(
          "b1", "Variable 1:",
          choices <- simulated_data %>% select(-Patient, -Time) %>% colnames(),
          selected = "Biomarker_1"
        ),
        hr(),
        uiOutput("b2_reactive"),
        hr(),
        uiOutput("b3_reactive"),
        hr(),
        downloadButton("download_relationship_plot", "Download Plot"),
        hr(),
        downloadButton("download_relationship_table", "Download Table")
      ),
      mainPanel(
        plotOutput("RelationshipPlot",
          brush = "RelationshipBrush"
        ),
        helpText("Select data points to view in table:"),
        tableOutput("RelationshipTable")
      )
    )
  ),
  tabPanel(
    "Time Trajectories",
    titlePanel("Time Trajectories"),
    sidebarLayout(
      sidebarPanel(
        helpText("Select Variables"),
        hr(),
        selectInput(
          "c1", "Variable 1:",
          choices = c("Biomarker_1", "Biomarker_2"), selected = "Biomarker_1"
        ),
        hr(),
        uiOutput("c2_reactive"), # Time trajectory plots 2nd dropdown menu
        hr(),
        uiOutput("c3_reactive"), # Time trajectory plots 3rd dropdown menu
        hr(),
        downloadButton("download_trajectory_plot", "Download Plot"),
        hr(),
        downloadButton("download_time_trajectory_table", "Download Table")
      ),
      mainPanel(
        plotOutput("TimeTrajectoryPlot",
          brush = "TrajectoryBrush"
        ),
        helpText("Select data points to view in table:"),
        tableOutput("TrajectoryTable")
      )
    )
  )
)
# Server

server <- function(input, output) {

  # named vector of source dataframe's classes

  simulated_data_classes <- sapply(simulated_data, class)

  #### Population Summary Tab ####

  # add faceting input option with choices dependent on category input selection
  output$a2_reactive <- renderUI({
    selectInput("a2", "Facet by:",
      choices = c("None", simulated_data[, colnames(simulated_data) != input$a1] %>%
        select_if(function(x) is.factor(x)) %>%
        select(-Time) %>%
        colnames()),
      selected = "None"
    )
  })

  # filter out redundant observations when "Time" is not selected
  simulated_data_not_redundant <- reactive({
    if (input$a1 != "Time" & input$a2 != "Time") {
      simulated_data %>% filter(Time == 0)
    } else {
      simulated_data
    }
  })

  # function to summarize population in table format

  summarize_count_Weight_bm <- function(df) {
    df %>%
      summarize(
        `Count` = n(),
        `Mean Weight` = mean(Weight),
        `Mean Biomarker A` = mean(Biomarker_1),
        `Mean Biomarker B` = mean(Biomarker_2)
      )
  }

  summary_plot_base <- reactive({
    simulated_data_not_redundant() %>%
      ggplot(aes_string(x = {{ input$a1 }})) +
      # note: double curly brackets are a new feature in rlang to quote and unquote arguments in one step
      # https://www.tidyverse.org/articles/2019/06/rlang-0-4-0/
      geom_bar(stat = "count", aes_string(fill = {{ input$a1 }})) +
      theme_minimal() +
      scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
  })

  summary_plot <- reactive({
    if (input$a2 == "None") {
      summary_plot_base()
    } else {
      summary_plot_base() +
        facet_grid(~ get(input$a2))
    }
  })

  # Render population summary plot
  output$SummaryPlot <- renderPlot(
    summary_plot()
  )

  # create summary table based on category and facet input
  summary_table <- reactive({
    if (input$a2 == "None") {
      simulated_data_not_redundant() %>%
        group_by_at(input$a1) %>%
        summarize_count_Weight_bm()
    } else {
      simulated_data_not_redundant() %>%
        group_by_at(c(input$a1, input$a2)) %>%
        summarize_count_Weight_bm()
    }
  })

  # Render summary table

  output$SummaryTable <- renderTable(
    summary_table()
  )

  ##### Relationships Plot tab ####

  # filter Time==0 if it is not selected for relationship plots
  simulated_data_not_redundant_relationships <- reactive({
    if (input$b1 != "Time" & input$b2 != "Time") {
      simulated_data %>% filter(Time == 0)
    } else {
      simulated_data
    }
  })

  # if first input is factor, second input drop-down omits factors
  relationships_plot_second_choices <- reactive({
    if (simulated_data_classes[input$b1] == "factor") {
      choices <- c(simulated_data[, !(colnames(simulated_data) %in% c(input$b1, "Patient"))] %>% select_if(function(x) !is.factor(x)) %>%
        colnames(), "None")
    } else {
      choices <- c(simulated_data[, !(colnames(simulated_data) %in% c(input$b1, "Patient"))] %>%
        colnames(), "None")
    }
  })


  # relationship variables input rendered
  output$b2_reactive <- renderUI({
    selectInput("b2", "Variable 2:",
      choices = relationships_plot_second_choices(), selected = "None"
    )
  })

  # change relationship plot type based on input
  relationship_variable_classes <- reactive({
    c(simulated_data_classes[input$b1], simulated_data_classes[input$b2])
  })


  relationship_plot_type <- reactive({
    case_when(
      all(relationship_variable_classes() %in% c("numeric", "integer")) ~ "point_plot",
      relationship_variable_classes()[1] == "factor" & is.na(relationship_variable_classes()[2]) ~ "bar_plot",
      relationship_variable_classes()[1] == "factor" & relationship_variable_classes()[2] != "factor" ~ "box_plot",
      relationship_variable_classes()[1] != "factor" & relationship_variable_classes()[2] == "factor" ~ "box_plot2",
      is.na(relationship_variable_classes()[2]) & relationship_variable_classes()[1] != "factor" ~ "histogram"
    )
  })

  relationship_plot_base <- reactive({
    switch(relationship_plot_type(),
      "point_plot" =
        simulated_data %>%
          ggplot(aes_string(
            x = {{ input$b1 }},
            y = {{ input$b2 }}
          )) +
          geom_point(),
      "box_plot1" =
        simulated_data %>%
          ggplot(aes_string(
            x = {{ input$b1 }},
            y = {{ input$b2 }}
          )) +
          geom_jitter(width = 0.1, alpha = 0.1) + geom_boxplot(width = 0.05, fill = "wheat", color = "red"),
      "box_plot2" =
        simulated_data %>%
          ggplot(aes_string(
            x = {{ input$b2 }},
            y = {{ input$b1 }}
          )) +
          geom_jitter(width = 0.1, alpha = 0.1) + geom_boxplot(width = 0.05, fill = "wheat", color = "red"),
      "histogram" =
        simulated_data %>%
          ggplot(aes_string(x = {{ input$b1 }})) +
          geom_histogram(),
      "bar_plot" =
        simulated_data %>%
          filter(Time == 0) %>%
          ggplot(aes_string(
            x = {{ input$b1 }}
          )) +
          geom_bar(stat = "count", aes_string(fill = {{ input$b1 }}))
    )
  })

  # Facet option for relationship plots
  output$b3_reactive <- renderUI({
    selectInput("b3", "Facet by :",
      choices = c("None", simulated_data[, !(colnames(simulated_data) %in% c(input$b1, input$b2, "Time", "Patient"))] %>%
        select_if(function(x) is.factor(x)) %>%
        colnames()), selected = "None"
    )
  })

  # Apply faceting to relationship plot if selected
  relationship_plot <- reactive({
    if (input$b3 == "None") {
      relationship_plot_base() +
        theme_minimal()
    }
    else {
      relationship_plot_base() +
        theme_minimal() +
        facet_grid(~ get({{ input$b3 }}))
    }
  })

  # Relationship plot output
  output$RelationshipPlot <- renderPlot({
    relationship_plot()
  })

  relationship_table_data <- reactive({
    brushedPoints(simulated_data, input$RelationshipBrush, allRows = TRUE) %>%
      filter(selected_ == TRUE) %>%
      select(-selected_)
  })

  # Relationship table output
  output$RelationshipTable <- renderTable(
    relationship_table_data()
  )

  #### Time-Trajectory Plot Tab ####

  # relationship variables input, restrict to no factors
  output$c2_reactive <- renderUI({
    selectInput("c2", "Color by :",
      choices = c("None", simulated_data[, !(colnames(simulated_data) %in% c("Time", "Patient", input$c1))] %>%
        colnames()), selected = "None"
    )
  })

  output$c3_reactive <- renderUI({
    selectInput("c3", "Facet by :",
      choices = c("None", simulated_data[, !(colnames(simulated_data) %in% c(input$c1, input$c2, "Time", "Patient"))] %>%
        select_if(function(x) is.factor(x)) %>%
        colnames()), selected = "None"
    )
  })

  time_trajectory_plot_base <- reactive({
    if (input$c2 == "None") {
      simulated_data %>%
        ggplot(aes_string(x = "Time", y = {{ input$c1 }}, group = "Patient")) +
        geom_path(size = 1, alpha = 0.1) +
        geom_point(alpha = .2)
    } else {
      simulated_data %>%
        ggplot(aes_string(x = "Time", y = {{ input$c1 }}, group = "Patient", color = {{ input$c2 }})) +
        geom_path(size = 1, alpha = 0.1) +
        geom_point(alpha = .2) +
        guides(color = guide_legend(override.aes = list(linetype = 1, size = 5, alpha = 1)))
    }
  })

  time_trajectory_plot <- reactive({
    if (input$c3 == "None") {
      time_trajectory_plot_base() +
        theme_minimal()
    }
    else {
      time_trajectory_plot_base() +
        facet_grid({{ input$c3 }}) +
        theme_minimal()
    }
  })

  output$TimeTrajectoryPlot <- renderPlot({
    time_trajectory_plot()
  })

  trajectory_table_data <- reactive({
    brushedPoints(simulated_data, input$TrajectoryBrush, allRows = TRUE) %>%
      filter(selected_ == TRUE) %>%
      select(-selected_)
  })

  output$TrajectoryTable <- renderTable(
    trajectory_table_data()
  )

  #### Downloads ####

  download_plot <- function(plot) {
    downloadHandler(
      filename = "filename.png",
      content = function(file) {
        ggsave(plot, filename = file)
      }
    )
  }

  download_table <- function(table_output) {
    downloadHandler(
      filename = "filename.csv",
      content = function(file) {
        write_csv(x = table_output, path = file)
      }
    )
  }

  output$download_summary_plot <- download_plot(summary_plot())

  output$download_summary_table <- download_table(summary_table())

  output$download_relationship_plot <- download_plot(relationship_plot())

  output$download_relationship_table <- download_table(relationship_table_data())

  output$download_time_trajectory_plot <- download_plot(time_trajectory_plot())

  output$download_time_trajectory_table <- download_plot(trajectory_table_data())
}

shinyApp(ui = ui, server = server)
