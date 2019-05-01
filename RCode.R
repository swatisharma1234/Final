# Code for the final R project
# Adding kn clustering for data

# Downloading data from GitHub folder and saving it in the working directory as "data.csv"
download.file("https://raw.githubusercontent.com/swatisharma1234/Midterm/master/shinydata.txt", destfile = "data.csv", mode = "wb")
# loading the saved data file and assigning it a new name
Data <- read.csv("data.csv")
View(Data)

server <- function(input, output) {
  mydata
  # Plot 3 Clustering cases of malnutrition based on variable selected by user
  selectedData <- reactive({
    qn.data[, c(input$xcol, input$ycol)]
  })
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E74C3C", "#5499C7", "#A2D9CE", "#4A235A",
              "#FF7F00", "#2C3E50", "#CACFD2", "#F7DC6F", "#17202A"))
    par(mar = c(5.1, 4.1, 2, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3, main="Clusters displayed by color")
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
  })
  # Plot 1 relationship between the child and parent's age, colored by location
  # Filtering can be done by Malnutrition type and gender of child
  output$plot_treatment<-renderPlot(
    
    ggplot(data=mydata %>% dplyr::filter(Moderate.Severe==input$Malnutrition_Type,sex==input$Gender),
           aes(Parent_AgeG, Age_M)) + 
      geom_point(aes(colour=Location),size=4)+labs(title="Relationship between Age of Child and Age of Parent by Location", x="Age of Parent (years)", y="Age of Child (Months)")+theme_light(base_size = 12)+
      theme(plot.title = element_text(color="#1A5276", face="bold", size=16, hjust=0)) 
    
  )
  # Plot 2 distribution of children in different age groups
  # Filtering can be done by Malnutrition type and gender of child
  output$plot_gender<- renderPlot(ggplot(data=mydata %>% dplyr::filter(Parent==input$Parent,ParentEducation_M==input$Education, Location==input$Location),
                                         aes(x=reorder(AgeGroup_M,Age_M))) + geom_bar(colour="white",fill="#78A4C0")+
                                    geom_text(stat='count',aes(label=..count..),vjust=-1)+xlab("Age Group of Child")+ylab("Number of Children")+ggtitle("Distribution by Age Groups")+theme_minimal(base_size =12)+
                                    theme(plot.title = element_text(color="#1A5276", face="bold", size=16, hjust=0)))
}
