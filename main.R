# Title: Canvas App
#
# Date: 2/4/2024
#
# Version: 1.1
#
# Author: William Kerns
#
# Purpose: Canvas needed a way to view enrollment for classes offered in Canvas
#
# Solution: This pulls in multiple datasets from Canvas API and presents them in a chart.
#           Canvas can then search by date and export via CSV
#
#

# load dependencies
library(shiny)
library(plyr)
library(dplyr)
library(DT)
library(httr)
library(jsonlite)
library(lubridate)
library(shinybusy)

# Variables
users <- list()
pageData <- list()
count <- 0
pag_count <- 1
pagination <- 1
customFields <- list(
  "First-name_Given-name",
  "Last-name_Family-name",
  "educator",
  "academic_staff",
  "non-profit",
  "institution_name",
  "government"
)
readRenviron(".Renviron")
date_from <- "2023-09-01T00:00:00Z"
date_to <- Sys.time() # Today's date
currentDate <- Sys.Date()
firstDay <- floor_date(Sys.Date() - months(1), unit = "month")
lastDay <- currentDate - days(day(currentDate))
from <- as.POSIXct(date_from, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
to <- as.POSIXct(date_to, "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Api Enrollments
url <- "https://course.market/api/v1/enrollments?listing_id=######&per_page=100&page=%d"
url2 <- "https://course.market/api/v1/enrollments?listing_id=%d&per_page=100&page=%d"

# Api Users
userurl <- "https://course.market/api/v1/user_registrations/%d"

# Api Key Need to move this to an Env Var
key <- Sys.getenv("SECRET_KEY")

# Function to get Canvas data through api call
GetTAData <- function(query, dateF, dateT) {
  from <- as.POSIXct(dateF, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  to <- as.POSIXct(dateT, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  # While loop if enrollment results are not empty
  while (TRUE) {
    # Build the api Query
    response <- GET(sprintf(url2, query, pagination), add_headers(Authorization = paste("Bearer ", key)))
    list_res <- httr::content(response)
    # cleans up the list
    list_res <- list_res[["enrollments"]]
    # Check if response is empty
    if (length(list_res) == 0) {
      break # if empty break out of while loop
    }
    pagination <- pagination + 1 # increase the pagination count for the next api call

    # loop through result and filter based on dates given
    for (i in list_res)
    {
      # store the enrolled_at date
      response_date <- i[["enrolled_at"]]
      # if Enrolled date is > (after) date_from and before today
      if (response_date > from && response_date < to) {
        # Add the entry to the list
        pageData <- append(pageData, list(i))
      }
    }
  }

  # The enrollments api only grabs course information about enrollments. The user id is the only information in a api enrollment call
  # So we need to grab student data from our last api call and update our list with student information

  # Grabs the user data for the enrollments
  # Loop through our list of enrollment information
  for (i in pageData)
  {
    count <- count + 1
    userId <- i[["user"]][["id"]] # store the user id
    res_user <- GET(sprintf(userurl, userId), add_headers(Authorization = paste("Bearer ", key))) # construct the api request
    cont <- content(res_user, as = "parsed", type = "application/json")


    for (field in customFields)
    {
      if (length(cont[["user_registration"]][["custom_fields"]][[field]]) == 0) {
        # Add the field with the value "NA"
        cont[["user_registration"]][["custom_fields"]][[field]] <- "NA"
      }
    }

    # Store the user content our users list
    users <- append(users, list(cont))
    # Join the user registration information in the pagedata list
    pageData[[count]][["user"]] <- users[[count]][["user_registration"]]
  }
  # Set the lists to a DataFrame
  df <- ldply(pageData, data.frame)

  # Convert character to Date
  df$enrolled_at <- as.Date(df$enrolled_at, format = "%Y-%m-%d")
  return(df)
}

# df <- GetTAData(######, date_from, date_to)  # Canvas Course1
# df2 <- GetTAData(######) # Canvas - Course2
# df3 <- GetTAData(######) # Canvas - Course3
# df4 <- GetTAData(######) # Canvas - Course4
# df5 <- GetTAData(######) # Canvas - Course5
# df6 <- GetTAData(######) # Canvas - Course6
# df7 <- GetTAData(######) # Canvas - Course7
# df8 <- GetTAData(######) # Canvas - Course8
# df9 <- GetTAData(######) # Canvas - Course9

# This returns the correct dataset based on the inputselection
col_order <- c("id", "user.canvas_user_id", "user.catalog.id",
               "user.catalog.name", "user.name","user.email",
               "user.custom_fields.First.name_Given.name",
               "user.custom_fields.Last.name_Family.name",
               "user.custom_fields.institution_name",
               "user.custom_fields.institution_role",
               "user.custom_fields.student_number",
               "user.custom_fields.zip_code",
               "user.custom_fields.student",
               #"user.custom_fields.faculty",
               "user.custom_fields.industry",
               "user.custom_fields.educator",
               #"user.custom_fields.academic_staff",
               "user.custom_fields.non.profit",
               "user.custom_fields.government",
               "user.created_at","user.updated_at",
               "listing.id","listing.title","listing.type",
               "listing.canvas_course_id","enrolled_at","status")

ui <- fluidPage(
  tags$style(
    HTML(
      "table tr {line-height:100%;}
    table.dataTable tbody th, table.dataTable tbody td {padding: 8px 10px}"
    )
  ),
  add_busy_spinner(spin = "fading-circle"),
  titlePanel("Canvas DataTable"),
  sidebarLayout(
    sidebarPanel(
      # Input: Choose dataset ----
      selectInput("dataset", "Choose a dataset:",
        choices = c("Canvas Course1",
                    "Canvas - Course2",
                    "Canvas - Computer Software Engineering",
                    "Canvas - Course3",
                    "Canvas - Course4",
                    "Canvas - Course11",
                    "Canvas - Course5",
                    "Canvas - Course6",
                    "Canvas - Course7",
                    "Canvas - Course8",
                    "Canvas - Course9",
                    "Canvas - Course10",
                    "Canvas - Course11")
      ),
      dateRangeInput("dateRange", h3("Date range"), firstDay, lastDay, min = "2023-09-01"),

      # Button
      downloadButton("downloadData", "Download CSV"),
      actionButton("updateData", "Update")
    ),
    mainPanel(
      # Table busy indicator

      dataTableOutput("my_table")
    )
  )
)
server <- function(input, output, session) {

  # Function for printing to csv. Need to return to this 
  datasetInput <- eventReactive(input$dataset, {
    show_modal_spinner()
    if (input$dataset == "Canvas Course1") {
      df <- GetTAData(106588, input$dateRange[1], input$dateRange[2]) # Canvas Course1
    } else if (input$dataset == "Canvas - Course2") {
      df <- GetTAData(140528, input$dateRange[1], input$dateRange[2]) # Canvas - Course2
    } else if (input$dataset == "Canvas - Course3") {
      df <- GetTAData(142450, input$dateRange[1], input$dateRange[2]) # Canvas - Course3
    } else if (input$dataset == "Canvas - Course4") {
      df <- GetTAData(140533, input$dateRange[1], input$dateRange[2]) # Canvas - Course4
    } else if (input$dataset == "Canvas - Course5") {
      df <- GetTAData(140529, input$dateRange[1], input$dateRange[2]) # Canvas - Course5
    } else if (input$dataset == "Canvas - Course6") {
      df <- GetTAData(140534, input$dateRange[1], input$dateRange[2]) # Canvas - Course6
    } else if (input$dataset == "Canvas - Course7") {
      df <- GetTAData(140531, input$dateRange[1], input$dateRange[2]) # Canvas - Course7
    } else if (input$dataset == "Canvas - Course8") {
      df <- GetTAData(139963, input$dateRange[1], input$dateRange[2]) # Canvas - Course8
    } else if (input$dataset == "Canvas - Course9") {
      df <- GetTAData(140532, input$dateRange[1], input$dateRange[2]) # Canvas - Course9
    } else if (input$dataset == "Canvas - Computer Software Engineering") {
      df <- GetTAData(152667, input$dateRange[1], input$dateRange[2]) # Canvas - Course10
    } else if (input$dataset == "Canvas - Course10") {
      df <- GetTAData(152666, input$dateRange[1], input$dateRange[2]) # Canvas - Course11
    } else if (input$dataset == "Canvas - Course11") {
      df <- GetTAData(152665, input$dateRange[1], input$dateRange[2]) # Canvas - Course12
    } else if (input$dataset == "Canvas - Course12") {
      df <- GetTAData(152663, input$dateRange[1], input$dateRange[2]) # Canvas - Course13
    }
    
    # If no data is found display a notification
    if (nrow(df) == 0) {
      shiny::showNotification("No data", type = "error")
      dataset <- NULL
      validate(
        need(!is.null(dataset), "No data")
      )
    } else {
      TA_Data <- df
      TA_Data <- TA_Data[as.Date(TA_Data$enrolled_at) >= input$dateRange[1] & as.Date(TA_Data$enrolled_at) <= input$dateRange[2], ]
      datasetOrder <- TA_Data
      ordered_columns <- intersect(col_order, names(datasetOrder))
      dataset <- datasetOrder[,ordered_columns]
    }
    remove_modal_spinner()
    return(dataset)
  })
  
  
  # Setup a reactive expression listener for both inputs
  toListen <- reactive({
    list(input$dataset,input$updateData)
  })
  
    # initialize the observeEvent with the reactive expression
  observeEvent(toListen(), {
    show_modal_spinner()
    if (input$dataset == "Canvas Course1") {
      df <- GetTAData(106588, input$dateRange[1], input$dateRange[2]) # Canvas Course1
    } else if (input$dataset == "Canvas - Course2") {
      df <- GetTAData(140528, input$dateRange[1], input$dateRange[2]) # Canvas - Course2
    } else if (input$dataset == "Canvas - Course3") {
      df <- GetTAData(142450, input$dateRange[1], input$dateRange[2]) # Canvas - Course3
    } else if (input$dataset == "Canvas - Course4") {
      df <- GetTAData(140533, input$dateRange[1], input$dateRange[2]) # Canvas - Course4
    } else if (input$dataset == "Canvas - Course5") {
      df <- GetTAData(140529, input$dateRange[1], input$dateRange[2]) # Canvas - Course5
    } else if (input$dataset == "Canvas - Course6") {
      df <- GetTAData(140534, input$dateRange[1], input$dateRange[2]) # Canvas - Course6
    } else if (input$dataset == "Canvas - Course7") {
      df <- GetTAData(140531, input$dateRange[1], input$dateRange[2]) # Canvas - Course7
    } else if (input$dataset == "Canvas - Course8") {
      df <- GetTAData(139963, input$dateRange[1], input$dateRange[2]) # Canvas - Course8
    } else if (input$dataset == "Canvas - Course9") {
      df <- GetTAData(140532, input$dateRange[1], input$dateRange[2]) # Canvas - Course9
    } else if (input$dataset == "Canvas - Computer Software Engineering") {
      df <- GetTAData(152667, input$dateRange[1], input$dateRange[2]) # Canvas - Course10
    } else if (input$dataset == "Canvas - Course10") {
      df <- GetTAData(152666, input$dateRange[1], input$dateRange[2]) # Canvas - Course11
    } else if (input$dataset == "Canvas - Course11") {
      df <- GetTAData(152665, input$dateRange[1], input$dateRange[2]) # Canvas - Course12
    } else if (input$dataset == "Canvas - Course11") {
      df <- GetTAData(152663, input$dateRange[1], input$dateRange[2]) # Canvas - Course13
    }

    # If no data is found display a notification
    if (nrow(df) == 0) {
      shiny::showNotification("No data", type = "error")
      dataset <- NULL
      validate(
        need(!is.null(dataset), "No data")
      )
    } else {
      TA_Data <- df
      TA_Data <- TA_Data[as.Date(TA_Data$enrolled_at) >= input$dateRange[1] & as.Date(TA_Data$enrolled_at) <= input$dateRange[2], ]
      dataset <- TA_Data
    }

    output$my_table <- renderDataTable({
      ordered_columns <- intersect(col_order, names(dataset))
      DT::datatable(dataset[, ordered_columns, drop = FALSE])
    })
    remove_modal_spinner()
  })

  # Filter data based on selections
  output$my_table <- renderDataTable({
    datasetInput()
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
}
# Run the application
shinyApp(ui = ui, server = server)