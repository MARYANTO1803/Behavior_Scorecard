library(shiny)
library(shinydashboard)
library(scorecard)
library(rsconnect)

options(scipen = 9999)

ui <- dashboardPage(
  dashboardHeader(title = "Behaviour Scorecard"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("Import Data", tabName = "import_data", icon = icon("book")),
    menuItem(
      "New Customer & Predict",
      tabName = "predict",
      icon = icon("search"),
      badgeLabel = "New",
      badgeColor = "red"
    )
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "dashboard",
      fluidRow(
        box(
          width = 3,
          background = "navy",
          height = 250,
          title = "Total Data",
          solidHeader = TRUE,
          status = "info",
          valueBoxOutput("total_scorecard_ds", width = 12)
        ),
        box(
          width = 3,
          background = "navy",
          height = 250,
          title = "Recommendation",
          solidHeader = TRUE,
          status = "info",
          valueBoxOutput("good_ds", width = 12)
        ),
        box(
          width = 3,
          background = "navy",
          height = 250,
          title = "Recommendation",
          solidHeader = TRUE,
          status = "info",
          valueBoxOutput("bad_ds", width = 12)
        ),
        box(
          width = 3,
          title = "Chart",
          background = "navy",
          height = 250,
          solidHeader = TRUE,
          status = "info",
          plotlyOutput(outputId = "plot_scorecard_recom")
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "Data Scorecard",
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "success",
          dataTableOutput(outputId = "df_scorecard_ds")
        )
      ),
      fluidRow(
        box(
          width = 4,
          title = "Sex",
          solidHeader = TRUE,
          status = "success",
          plotlyOutput(outputId = "plot_scorecard_sex")
        ),
        box(
          width = 4,
          title = "Marriage",
          solidHeader = TRUE,
          status = "success",
          plotlyOutput(outputId = "plot_scorecard_marriage")
        ),
        box(
          width = 4,
          title = "Limit Balance",
          solidHeader = TRUE,
          status = "success",
          sliderInput(
            inputId = "bins_1",
            label = "Select Bins:",
            min = 2,
            max = 50,
            value = 30
          ),
          plotlyOutput(outputId = "plot_scorecard_hist", height = 300)
        )
        
      ),
      fluidRow(
        box(
          width = 6,
          title = "Education",
          solidHeader = TRUE,
          status = "success",
          plotlyOutput(outputId = "plot_scorecard_education")
        ),
        box(
          width = 6,
          title = "Age",
          solidHeader = TRUE,
          status = "success",
          plotlyOutput(outputId = "plot_scorecard_age")
        )
      )
    ),
    tabItem(
      tabName = "import_data",
      fluidRow(
        box(
          width = 3,
          title = "Input File",
          solidHeader = TRUE,
          status = "warning",
          fileInput("file1", "Choose Excel File", accept = ".xlsx")
        ),
        box(
          width = 3,
          title = "Nilai Cut Off",
          solidHeader = TRUE,
          status = "warning",
          numericInput(
            inputId = "nilai_cutoff_1",
            label = "",
            value = 0
          ),
          actionButton("input_cutoff_1", "Select")
        ),
        box(
          width = 2,
          background = "navy",
          title = "Total Data",
          solidHeader = TRUE,
          status = "info",
          valueBoxOutput("total_scorecard_1", width = 12)
        ),
        box(
          width = 2,
          background = "navy",
          title = "Recommendation",
          solidHeader = TRUE,
          status = "info",
          valueBoxOutput("good_1", width = 12)
        ),
        box(
          width = 2,
          background = "navy",
          title = "Recommendation",
          solidHeader = TRUE,
          status = "info",
          valueBoxOutput("bad_1", width = 12)
        )
      ),
      fluidRow(
        box(
          width = 11,
          title = "Data Scorecard",
          solidHeader = TRUE,
          status = "success",
          collapsible = TRUE,
          dataTableOutput("df_scorecard_1"),
          actionButton("clear1", "Clear")
        ),
        box(
          width = 1,
          title = "Action",
          solidHeader = TRUE,
          status = "warning",
          actionButton("reload1", "Reload"),
          tags$br(),
          tags$br(actionButton("save1", "Save")),
        )
      )
    ),
    tabItem(
      tabName = "predict",
      fluidRow(
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Limit Balance",
          numericInput(
            inputId = "limit_bal",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Sex",
          selectInput(
            inputId = "sex",
            label = "1:Male 2:Female",
            choices = c(1, 2)
          )
        ),
        box(
          width = 3,
          solidHeader = TRUE,
          status = "primary",
          title = "Education",
          selectInput(
            inputId = "education",
            label = "1:Pascasarjana 2:Sarjana 3:SMA 4:Lainnya",
            choices = c(1, 2, 3, 4)
          )
        ),
        box(
          width = 3,
          solidHeader = TRUE,
          status = "primary",
          title = "Marriage",
          selectInput(
            inputId = "marriage",
            label = "1:Menikah 2:Lajang 3:Lainnya",
            choices = c(1, 2, 3)
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Age",
          numericInput(
            inputId = "age",
            label = "",
            value = 0
          )
        )
      ),
      fluidRow(
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Pay 1",
          numericInput(
            inputId = "pay_1",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Pay 2",
          numericInput(
            inputId = "pay_2",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Pay 3",
          numericInput(
            inputId = "pay_3",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Pay 4",
          numericInput(
            inputId = "pay_4",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Pay 5",
          numericInput(
            inputId = "pay_5",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Pay 6",
          numericInput(
            inputId = "pay_6",
            label = "",
            value = 0
          )
        )
      ),
      fluidRow(
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Bill Amt 1",
          numericInput(
            inputId = "bill_amt_1",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Bill Amt 2",
          numericInput(
            inputId = "bill_amt_2",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Bill Amt 3",
          numericInput(
            inputId = "bill_amt_3",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Bill Amt 4",
          numericInput(
            inputId = "bill_amt_4",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Bill Amt 5",
          numericInput(
            inputId = "bill_amt_5",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Bill Amt 6",
          numericInput(
            inputId = "bill_amt_6",
            label = "",
            value = 0
          )
        )
      ),
      fluidRow(
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Pay Amt 1",
          numericInput(
            inputId = "pay_amt_1",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Pay Amt 2",
          numericInput(
            inputId = "pay_amt_2",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Pay Amt 3",
          numericInput(
            inputId = "pay_amt_3",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Pay Amt 4",
          numericInput(
            inputId = "pay_amt_4",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Pay Amt 5",
          numericInput(
            inputId = "pay_amt_5",
            label = "",
            value = 0
          )
        ),
        box(
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          title = "Pay Amt 6",
          numericInput(
            inputId = "pay_amt_6",
            label = "",
            value = 0
          )
        ),
      ),
      fluidRow(
        box(
          width = 3,
          solidHeader = TRUE,
          status = "primary",
          title = "Cut Off",
          numericInput(
            inputId = "cut_off",
            label = "",
            value = 0
          ),
          actionButton("predict_cutoff", "Predict")
        ),
        box(
          width = 3,
          background = "navy",
          title = "Score",
          solidHeader = TRUE,
          status = "info",
          valueBoxOutput("score", width = 12),
        ),
        box(
          width = 3,
          background = "navy",
          title = "Recommendation",
          solidHeader = TRUE,
          status = "info",
          valueBoxOutput("recommendation", width = 12)
        ),
      ),
      fluidRow(
        box(
          width = 11,
          title = "Data Scorecard",
          solidHeader = TRUE,
          status = "success",
          collapsible = TRUE,
          dataTableOutput("df_scorecard_2"),
          actionButton("clear2", "Clear")
        ),
        box(
          width = 1,
          title = "Action",
          solidHeader = TRUE,
          status = "warning",
          actionButton("reload2", "Reload"),
          tags$br(),
          tags$br(actionButton("save2", "Save")),
        )
      )
    )
  ))
  
)
