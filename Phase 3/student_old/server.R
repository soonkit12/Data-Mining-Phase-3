library(shiny)
library(DT)
library(ggplot2)
library(Amelia)



stuMat <- read.table(file = "files/student-mat.csv", header = TRUE, sep =";")
stuPor <- read.table(file = "files/student-por.csv", header = TRUE, sep =";")

server <- function(input, output) {
  # Filter data based on selections - stuMath
  output$studentMathtable <- DT::renderDataTable(DT::datatable({
    data <- stuMat
    if (input$school != "All") {
      data <- data[data$school == input$school,]
    }
    if (input$G3 != "All") {
      data <- data[data$G3 == input$G3,]
    }
    if (input$paid != "All") {
      data <- data[data$paid == input$paid,]
    }
    data
  }))
  
  # Filter data based on selections - stuPor
  output$studentPortable <- DT::renderDataTable(DT::datatable({
    data1 <- stuPor
    if (input$school1 != "All") {
      data1 <- data1[data1$school == input$school1,]
    }
    if (input$G31 != "All") {
      data1 <- data1[data1$G3 == input$G31,]
    }
    if (input$paid1 != "All") {
      data1 <- data1[data1$paid == input$paid1,]
    }
    data1
  }))
  
  #summary for StuMat
  output$summaryStuMat <- renderPrint({
    summary(stuMat)
  })
  
  
  #summary for StuPor
  output$summaryStuPor <- renderPrint({
    summary(stuPor)
  })
  
  #structure for StuMat
  output$strucMath <- renderPrint({
    str(stuMat)
  })
  
  
  #structure for StuPor
  output$strucPor <- renderPrint({
    str(stuPor)
  })
  
  #barplot for sex vs G3 rates proportion - studentMath
  output$plot1 <- renderPlot({
    barplot(counts, main="Sex vs Final Grade (G3) rates in proportion for StudentMath", xlab="G3", col=c("darkblue","red"), legend = rownames(counts), beside=TRUE)
  })
  
  #barplot for sex vs G3 rates proportion - studentPor
  output$plot2 <- renderPlot({
    barplot(counts2, main="Sex VS Final Grade (G3) rates in proportion for StudentPor", xlab="G3", col=c("darkblue","red"), legend = rownames(counts2), beside=TRUE)
  })
  
  #Scatterplot correlation G3 vs G1 + G2
  output$scat1 <- renderPlot({
    pairs(~G3+G1+G2,data = stuMat,col=c("red","blue","green"), main = "Correlationship for G3 with G1 and G2 - StudentMath")
  })
  
  output$scat2 <- renderPlot({
    pairs(~G3+G1+G2,data = stuPor,col=c("red","blue","green"), main = "Correlationship for G3 with G1 and G2 - StudentPor")
  })
  
  #missing value 
  output$miplot1 <- renderPlot({
    missmap(stuMat, main = "Missing values vs observed for studentMath")
  })
  
  output$miplot2 <- renderPlot({
    missmap(stuPor, main = "Missing values vs observed for studentPor")
  })
  
  #outlier 
  output$boxplot1 <- renderPlot({
     boxplot(stuMat, main = "Boxplot for StudentMath ")
  })
  
  output$boxplot2 <- renderPlot({
    boxplot(stuPor, main = "Boxplot for StudentPor ")
  })
  
  
  #k-means clustering before cluster based on higher variable for student wants to take higher education (binary: yes or no)  
  output$bcluster1 <- renderPlot({
    ggplot(stuMat, aes(G1, G2, color = higher)) + geom_point() + ggtitle("Random cluster for G1 vs G2 on higher variable")
  })
  
  #after
  output$acluster1 <- renderPlot({
    # set the seed to ensure reproducibility
    set.seed(20)
    # R will try 20 different random starting assignments and then select the one with the lowest within cluster variation.
    stuMatCluster <- kmeans(stuMat[, 31:32], 2, nstart = 20)
   
    #compare the 2 clusters with the higher (yes or no)
    table(stuMatCluster$cluster, stuMat$higher)
    stuMatCluster$cluster <- as.factor(stuMatCluster$cluster)
    ggplot(stuMat, aes(G1, G2,  color = stuMatCluster$cluster)) + geom_point() + ggtitle("2 cluster for  G1 vs G2 on higher variable")
    
  })
  #studentPor k-means - before
  output$bcluster2 <- renderPlot({
    ggplot(stuPor, aes(G1, G2, color = higher)) + geom_point() + ggtitle("Random cluster for G1 vs G2 on higher variable")
  })
  #after
  output$acluster2 <- renderPlot({
    # set the seed to ensure reproducibility
    set.seed(20)
    stuPorCluster <- kmeans(stuPor[, 31:32], 2, nstart = 20)
    stuPorCluster
    table(stuPorCluster$cluster, stuPor$higher)
    stuPorCluster$cluster <- as.factor(stuPorCluster$cluster)
    ggplot(stuPor, aes(G1, G2,  color = stuPorCluster$cluster)) + geom_point() + ggtitle("2 cluster for  G1 vs G2 on higher variable")
    
  })
  
  
  
}

