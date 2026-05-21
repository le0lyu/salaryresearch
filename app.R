library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(plotly)
library(broom)
library(scales)

df <- read.csv("data.csv")
names(df) <- tolower(names(df))

if ("experience_years" %in% names(df)) df$years_experience <- df$experience_years
if ("remote_work" %in% names(df)) df$remote_status <- df$remote_work
if ("work_mode" %in% names(df)) df$remote_status <- df$work_mode

df$remote_status <- as.character(df$remote_status)
if ("education_level" %in% names(df)) df$education_level <- as.character(df$education_level)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("Tech Salary Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Controls"),
      sliderInput(
        "salary_range", "Salary range",
        min = floor(min(df$salary, na.rm = TRUE)),
        max = ceiling(max(df$salary, na.rm = TRUE)),
        value = c(floor(min(df$salary, na.rm = TRUE)), ceiling(max(df$salary, na.rm = TRUE))),
        pre = "$", sep = ","
      ),
      sliderInput(
        "exp_range", "Years of experience",
        min = floor(min(df$years_experience, na.rm = TRUE)),
        max = ceiling(max(df$years_experience, na.rm = TRUE)),
        value = c(floor(min(df$years_experience, na.rm = TRUE)), ceiling(max(df$years_experience, na.rm = TRUE))),
        step = 1
      ),
      sliderInput(
        "skills_range", "Skills count",
        min = floor(min(df$skills_count, na.rm = TRUE)),
        max = ceiling(max(df$skills_count, na.rm = TRUE)),
        value = c(floor(min(df$skills_count, na.rm = TRUE)), ceiling(max(df$skills_count, na.rm = TRUE))),
        step = 1
      ),
      selectInput(
        "remote_filter", "Remote status",
        choices = c("All", sort(unique(df$remote_status))),
        selected = "All"
      ),
      sliderInput(
        "sample_n", "Points shown on scatterplots",
        min = 1000, max = 15000, value = 5000, step = 1000
      ),
      hr(),
      h4("Salary predictor"),
      numericInput("pred_exp", "Years of experience", value = 10, min = 0, max = 40, step = 1),
      numericInput("pred_skills", "Skills count", value = 10, min = 1, max = 30, step = 1)
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        column(4, card(card_body(h5("Filtered Rows"), textOutput("n_obs")))),
        column(4, card(card_body(h5("Median Salary"), textOutput("median_salary")))),
        column(4, card(card_body(h5("Mean Salary"), textOutput("mean_salary"))))
      ),
      
      br(),
      p("This app examines the factors that influence the salary of individuals in the technology sector. The main focus is salary, years of experience, remote work status, number of skills, and the overall strength of those relationships."),
      
      navset_tab(
        nav_panel(
          "Overview",
          h3("Salary Distribution"),
          plotlyOutput("salary_hist", height = "350px"),
          br(),
          DTOutput("salary_summary"),
          br(),
          p("Salary is the primary variable of interest in this study, and more specifically the outcome variable. An understanding of salary is key to understanding the underlying factors that drive career and personal growth in the technology sector."),
          p("At a median salary of $143,453, the median salary within this dataset is significantly higher than the national median salary of $43,222 as of 2022. Although the salary variable ranges from $32,867 to $333,046 with a range of $300,179, the low inter-quartile range of $26,039 suggests a strong concentration around the median salary values."),
          
          br(),
          h3("Salary Prediction"),
          plotlyOutput("prediction_plot", height = "320px"),
          br(),
          htmlOutput("prediction_text")
        ),
        
        nav_panel(
          "Experience",
          h3("Experience vs Salary"),
          plotlyOutput("exp_plot", height = "380px"),
          br(),
          p("According to the linear regression model, the y-intercept at 0 years of experience is about $118,692, and for each year of experience the model predicts an increase in salary of around $2,701. This suggests a positive correlation between years of experience and salary."),
          p("However, the R-squared value of 0.192 means that less than 20% of the variation can be explained by the relationship between salary and years of experience. Despite the statistical significance, the relationship is not especially strong."),
          
          br(),
          h3("Residuals vs Experience Years"),
          plotlyOutput("exp_resid", height = "320px"),
          br(),
          p("Looking at the residual chart, there are no other significant patterns within the residuals, therefore the conclusions made by the previous regression analysis are statistically valid. However, the variance of salary appears to increase with years of experience, suggesting a wider range of possible salaries at higher levels of experience.")
        ),
        
        nav_panel(
          "Remote and Skills",
          h3("Remote Work and Salary"),
          plotlyOutput("remote_plot", height = "360px"),
          br(),
          p("The number of workers in each remote-work category is roughly similar, with slightly higher numbers in fully in-person and fully remote work than in hybrid work. Even with the rise in popularity of remote work, fully in-person work remains the most common."),
          p("The two-sample t-test gives a p-value of 0.836, so there is not enough evidence at the 95% confidence level to suggest that the salary for individuals working fully remote is different from that of individuals working fully in person."),
          
          br(),
          h3("Skills vs Salary"),
          plotlyOutput("skills_plot", height = "380px"),
          br(),
          p("The linear regression model for skills count shows a statistically significant relationship with salary. However, the R-squared value of 0.016 means that only around 1.6% of the variation can be explained by the difference in an individual's number of skills."),
          p("Therefore, the number of skills is an extremely weak factor affecting salary. The relationship may be statistically significant, but it is not strong enough to meaningfully explain salary differences on its own."),
          
          br(),
          h3("Residuals vs Skills Count"),
          plotlyOutput("skills_resid", height = "320px"),
          br(),
          p("There are no other patterns within the residuals of the linear regression, therefore the conditions for inference are met. Because the residual plot does not show any major structure, the inference from the regression model remains reasonable."),
          
          br(),
          h3("Skills Count by Education Level"),
          plotlyOutput("edu_skills_plot", height = "340px"),
          br(),
          p("Looking at the boxplots showing the relationship between education level and number of skills, the boxplots are essentially identical. Therefore, education level and number of skills appear to be almost completely independent in this dataset."),
          p("Though this could be true in the sample, it is unlikely to happen in a real-world sample where education would normally be expected to increase the number of skills held by an individual. This unusually high level of independence raises concern about how realistic the dataset may be.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_df <- reactive({
    d <- df |>
      filter(
        salary >= input$salary_range[1],
        salary <= input$salary_range[2],
        years_experience >= input$exp_range[1],
        years_experience <= input$exp_range[2],
        skills_count >= input$skills_range[1],
        skills_count <= input$skills_range[2]
      )
    
    if (input$remote_filter != "All") {
      d <- d |> filter(remote_status == input$remote_filter)
    }
    
    d
  })
  
  sampled_df <- reactive({
    d <- filtered_df()
    n <- min(nrow(d), input$sample_n)
    if (nrow(d) <= n) return(d)
    d |> slice_sample(n = n)
  })
  
  output$n_obs <- renderText({
    comma(nrow(filtered_df()))
  })
  
  output$median_salary <- renderText({
    dollar(median(filtered_df()$salary, na.rm = TRUE))
  })
  
  output$mean_salary <- renderText({
    dollar(mean(filtered_df()$salary, na.rm = TRUE))
  })
  
  output$salary_hist <- renderPlotly({
    d <- filtered_df()
    plot_ly(
      d,
      x = ~salary,
      type = "histogram",
      nbinsx = 40,
      marker = list(color = "#2c7fb8")
    ) |>
      layout(
        xaxis = list(title = "Salary"),
        yaxis = list(title = "Count"),
        bargap = 0.05
      )
  })
  
  output$salary_summary <- renderDT({
    d <- filtered_df()
    tbl <- data.frame(
      Minimum = dollar(min(d$salary, na.rm = TRUE)),
      Q1 = dollar(unname(quantile(d$salary, 0.25, na.rm = TRUE))),
      Median = dollar(median(d$salary, na.rm = TRUE)),
      Q3 = dollar(unname(quantile(d$salary, 0.75, na.rm = TRUE))),
      Maximum = dollar(max(d$salary, na.rm = TRUE))
    )
    datatable(tbl, options = list(dom = "t"), rownames = FALSE)
  })
  
  output$prediction_plot <- renderPlotly({
    d <- filtered_df()
    m_exp <- lm(salary ~ years_experience, data = d)
    
    x_vals <- seq(min(d$years_experience, na.rm = TRUE), max(d$years_experience, na.rm = TRUE), length.out = 100)
    pred_df <- data.frame(
      years_experience = x_vals,
      pred_salary = predict(m_exp, newdata = data.frame(years_experience = x_vals))
    )
    
    pred_point <- predict(m_exp, newdata = data.frame(years_experience = input$pred_exp))
    
    plot_ly() |>
      add_lines(
        data = pred_df,
        x = ~years_experience,
        y = ~pred_salary,
        line = list(color = "black", width = 3),
        name = "Predicted salary"
      ) |>
      add_markers(
        x = input$pred_exp,
        y = pred_point,
        marker = list(size = 10, color = "#d95f0e"),
        name = "Prediction"
      ) |>
      layout(
        xaxis = list(title = "Years of Experience"),
        yaxis = list(title = "Predicted Salary"),
        showlegend = FALSE
      )
  })
  
  output$prediction_text <- renderUI({
    d <- filtered_df()
    m_exp <- lm(salary ~ years_experience, data = d)
    m_sk <- lm(salary ~ skills_count, data = d)
    
    pred_exp <- predict(m_exp, newdata = data.frame(years_experience = input$pred_exp))
    pred_skills <- predict(m_sk, newdata = data.frame(skills_count = input$pred_skills))
    
    HTML(paste0(
      "<p>Using the filtered data, the experience model predicts a salary of <b>",
      dollar(pred_exp),
      "</b> for someone with ",
      input$pred_exp,
      " years of experience. The skills-only model predicts <b>",
      dollar(pred_skills),
      "</b> for someone with ",
      input$pred_skills,
      " skills, but the skills model is much weaker overall.</p>"
    ))
  })
  
  output$exp_plot <- renderPlotly({
    d <- sampled_df()
    full_d <- filtered_df()
    m <- lm(salary ~ years_experience, data = full_d)
    
    x_line <- seq(min(full_d$years_experience, na.rm = TRUE), max(full_d$years_experience, na.rm = TRUE), length.out = 100)
    line_df <- data.frame(
      years_experience = x_line,
      salary = predict(m, newdata = data.frame(years_experience = x_line))
    )
    
    plot_ly(
      d,
      x = ~years_experience,
      y = ~salary,
      type = "scatter",
      mode = "markers",
      marker = list(size = 4, opacity = 0.18, color = "#3182bd"),
      hoverinfo = "skip"
    ) |>
      add_lines(
        data = line_df,
        x = ~years_experience,
        y = ~salary,
        line = list(color = "black", width = 3),
        inherit = FALSE
      ) |>
      layout(
        xaxis = list(title = "Years of Experience"),
        yaxis = list(title = "Salary"),
        showlegend = FALSE
      )
  })
  
  output$exp_resid <- renderPlotly({
    d <- sampled_df()
    full_d <- filtered_df()
    m <- lm(salary ~ years_experience, data = full_d)
    d$resid_exp <- d$salary - predict(m, newdata = d)
    
    plot_ly(
      d,
      x = ~years_experience,
      y = ~resid_exp,
      type = "scatter",
      mode = "markers",
      marker = list(size = 4, opacity = 0.14, color = "black"),
      hoverinfo = "skip"
    ) |>
      layout(
        xaxis = list(title = "Years of Experience"),
        yaxis = list(title = "Residuals"),
        shapes = list(list(
          type = "line",
          x0 = min(d$years_experience, na.rm = TRUE),
          x1 = max(d$years_experience, na.rm = TRUE),
          y0 = 0, y1 = 0,
          line = list(color = "red", width = 2)
        )),
        showlegend = FALSE
      )
  })
  
  output$remote_plot <- renderPlotly({
    d <- filtered_df() |> filter(!is.na(remote_status))
    plot_ly(
      d,
      x = ~remote_status,
      y = ~salary,
      type = "box",
      color = ~remote_status,
      boxpoints = FALSE
    ) |>
      layout(
        xaxis = list(title = "Remote Status"),
        yaxis = list(title = "Salary"),
        showlegend = FALSE
      )
  })
  
  output$skills_plot <- renderPlotly({
    d <- sampled_df()
    full_d <- filtered_df()
    m <- lm(salary ~ skills_count, data = full_d)
    
    x_line <- seq(min(full_d$skills_count, na.rm = TRUE), max(full_d$skills_count, na.rm = TRUE), length.out = 100)
    line_df <- data.frame(
      skills_count = x_line,
      salary = predict(m, newdata = data.frame(skills_count = x_line))
    )
    
    plot_ly(
      d,
      x = ~skills_count,
      y = ~salary,
      type = "scatter",
      mode = "markers",
      marker = list(size = 4, opacity = 0.16, color = "#31a354"),
      hoverinfo = "skip"
    ) |>
      add_lines(
        data = line_df,
        x = ~skills_count,
        y = ~salary,
        line = list(color = "black", width = 3),
        inherit = FALSE
      ) |>
      layout(
        xaxis = list(title = "Skills Count"),
        yaxis = list(title = "Salary"),
        showlegend = FALSE
      )
  })
  
  output$skills_resid <- renderPlotly({
    d <- sampled_df()
    full_d <- filtered_df()
    m <- lm(salary ~ skills_count, data = full_d)
    d$resid_sk <- d$salary - predict(m, newdata = d)
    
    plot_ly(
      d,
      x = ~skills_count,
      y = ~resid_sk,
      type = "scatter",
      mode = "markers",
      marker = list(size = 4, opacity = 0.12, color = "black"),
      hoverinfo = "skip"
    ) |>
      layout(
        xaxis = list(title = "Skills Count"),
        yaxis = list(title = "Residuals"),
        shapes = list(list(
          type = "line",
          x0 = min(d$skills_count, na.rm = TRUE),
          x1 = max(d$skills_count, na.rm = TRUE),
          y0 = 0, y1 = 0,
          line = list(color = "red", width = 2)
        )),
        showlegend = FALSE
      )
  })
  
  output$edu_skills_plot <- renderPlotly({
    if (!"education_level" %in% names(filtered_df())) return(NULL)
    d <- filtered_df() |> filter(!is.na(education_level))
    
    plot_ly(
      d,
      x = ~education_level,
      y = ~skills_count,
      type = "box",
      color = ~education_level,
      boxpoints = FALSE
    ) |>
      layout(
        xaxis = list(title = "Education Level"),
        yaxis = list(title = "Skills Count"),
        showlegend = FALSE
      )
  })
}

shinyApp(ui, server)