library(shiny)
library(DT)
library(ggplot2)
library(Amelia)

stuMat <- read.table(file = "files/student-mat.csv", header = TRUE, sep =";")
stuPor <- read.table(file = "files/student-por.csv", header = TRUE, sep =";")

shinyUI(fluidPage(
  
  titlePanel("Student Performance Analysis"),
         
        sidebarLayout(
         # sidebarPanel(
          navlistPanel(
            tabPanel("About",
                   h3("The particular datasets StudentMath and StudentPor chosen was from Paulo Cortez and Alice Silva’s work, “Using Data Mining To Predict Secondary School Student Performance”. Generally these datasets described the poorly performed Portuguese students on Mathematics and unfortunately their own native language, Portuguese. There are various reasons to be considered in affecting the student’s performance such as travelling time to school, parents’ cohabitation status.")   
            ),
            "Data Quality",
            tabPanel("Missing value observed - studentMath",
                     plotOutput("miplot1")
            ),
            tabPanel("Missing value observed - studentPor",
                     plotOutput("miplot2")
            ),
            tabPanel("Outlier - StudentMath",
                     plotOutput("boxplot1"),
                     h4("The outliers occur in this plot is a natural outlier that within the requested range.")
            ),
            tabPanel("Outlier - StudentPor",
                     plotOutput("boxplot2"),
                     h4("The outliers occur in this plot is a natural outlier that within the requested range.")
            ), 
            "Table Summary",
            tabPanel("Summary for StudentMath table",
               verbatimTextOutput("summaryStuMat")
            ),
             tabPanel("Summary for StudentPor table",
                       verbatimTextOutput("summaryStuPor")
          ),
          tabPanel("StudentMath table structure",
                   verbatimTextOutput("strucMath")
          ),
          tabPanel("StudentPor table structure",
                   verbatimTextOutput("strucPor")
          ),
          "Plot" , 
          tabPanel("Sex vs G3 rates in proportion - StudentMath",
                   plotOutput("plot1")
          ),
          tabPanel("Sex vs G3 rates in proportion - StudentPor",
                   plotOutput("plot2")
          ),  
          tabPanel("Scatterplot G3 vs G1 + G2 - StudentMath",
                   plotOutput("scat1")
          ),
          tabPanel("Scatterplot G3 vs G1 + G2 - StudentPor",
                   plotOutput("scat2")
          ),
          
          "k-means clustering",
          tabPanel("StudentMath k-means clustering - before",
                    plotOutput('bcluster1'), h4("higher is a variable for student wants to take higher education - binary: yes or no")
          ),
          tabPanel("StudentMath k-means clustering - after",
                   
                   plotOutput('acluster1')
          ),
          tabPanel("StudentPor k-means clustering - before",
                   plotOutput('bcluster2'), h4("higher is a variable for student wants to take higher education - binary: yes or no")
          ),
          tabPanel("StudentPor k-means clustering - after",
                   
                   plotOutput('acluster2')
          )
          ),
        
          mainPanel(
            tabsetPanel(
              id = 'dataset',
              tabPanel('StudentMath', 
                       # Create a new Row in the UI for selectInputs
                       fluidRow(
                         column(4,
                                selectInput("school",
                                            "School:",
                                            c("All",
                                              unique(as.character(stuMat$school))))
                         ),
                         column(4,
                                selectInput("paid",
                                            "Extra Paid Classes:",
                                            c("All",
                                              unique(as.character(stuMat$paid))))
                         ),
                         column(4,
                                selectInput("G3",
                                            "Final Grade:",
                                            c("All",
                                              unique(as.numeric(stuMat$G3))))
                         )
                       ),
                       # Create a new row for the table.
                       fluidRow(
                         DT::dataTableOutput("studentMathtable")
                       )
                       
                       ),
              tabPanel('StudentPor',  
                       # Create a new Row in the UI for selectInputs
                       fluidRow(
                         column(4,
                                selectInput("school1",
                                            "School:",
                                            c("All",
                                              unique(as.character(stuPor$school))))
                         ),
                         column(4,
                                selectInput("paid1",
                                            "Extra Paid Classes:",
                                            c("All",
                                              unique(as.character(stuPor$paid))))
                         ),
                         column(4,
                                selectInput("G31",
                                            "Final Grade:",
                                            c("All",
                                              unique(as.numeric(stuPor$G3))))
                         )
                       ),
                       # Create a new row for the table.
                       fluidRow(
                         DT::dataTableOutput("studentPortable")
                       )
                       
              )
               
            )
          )
               
      )                 
                         
)
)


