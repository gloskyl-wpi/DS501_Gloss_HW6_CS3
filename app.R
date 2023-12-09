# Libraries
library(shiny)
library(shinyWidgets)

# Read in data
urlWL = "https://www.kaggle.com/datasets/yuxinc/summer-olympics-weightlifting-records-2000-to-2020"
csvPath = "weightlifting_records_2000_to_2020.csv"
dfWL = read.csv(csvPath)
# Scrub data
# Remove first column since it's just indexes
dfWL = subset(dfWL, select = -1)
# Rename each column to something more reasonable
colnames(dfWL) = c("athlete", "bodyweight", "snatch", "clean_and_jerk", "total", "ranking", "url", "title", "year", "gender")
# Get rid of anything with a bodyweight of 0
dfWL = dfWL[dfWL$bodyweight > 20,]
# Get rid of bombs
dfWL = dfWL[dfWL$snatch > 1,]
dfWL = dfWL[dfWL$clean_and_jerk > 1,]
# Make a column for weight classes by digging the information from the title column
dfWL$weightclass = dfWL$bodyweight
for (i in 1:length(dfWL$title))
{
  indexes = which(strsplit(dfWL$title[i], "")[[1]] == " ")
  dfWL$weightclass[i] = substring(dfWL$title[i], indexes[length(indexes) - 1], indexes[length(indexes)])
}


# Get all filter options
genders = rownames(table(dfWL$gender))
years = rownames(table(dfWL$year))
weightclasses = rownames(table(dfWL$weightclass))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Olympic Weightlifting Correlations"),
  
  sidebarLayout(
    sidebarPanel(
     
       # Input: Radio buttons for displaying writeup
      radioButtons(inputId = "writeupSelect",
                   label = "Writeup Select",
                   choices = c("Introduction", "Dataset", "Motivation", "Analysis", "Regression"),
                   selected = "Introduction")
      
    )
    
    ,
    
    mainPanel(
      
      textOutput(outputId = "writeup")
      
    )
    
  )
  
  ,
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Radio buttons for x and y axis data selection on xy plot ----
      radioButtons(inputId = "color",
                   label = "Color Scheme",
                   choices = c("Gender", "Year"),
                   selected = "Year")
      
      ,
      
      # Input: Radio buttons for x and y axis data selection on xy plot ----
      radioButtons(inputId = "x",
                  label = "X Axis Data",
                  choices = c("Bodyweight", "Snatch", "Clean & Jerk", "Total", "Ranking", "Year"),
                  selected = "Year")
      
      ,
      
      radioButtons(inputId = "y",
                   label = "Y Axis Data",
                   choices = c("Bodyweight", "Snatch", "Clean & Jerk", "Total", "Ranking", "Year"),
                   selected = "Total")
      
      ,
      
      multiInput(inputId = "gender",
                 label = "Gender Test",
                 choices = genders,
                 selected = "Men")
      
      ,
      
      multiInput(inputId = "year",
                 label = "Year",
                 choices = years,
                 selected = years)
      
      ,
      
      multiInput(inputId = "weightclass",
                 label = "Weightclass",
                 choices = weightclasses,
                 selected = weightclasses[c(1, 2)])
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
      ,
      
      
      verbatimTextOutput(outputId = "regression")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output)
{
  summaryText = ""
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot = renderPlot({
    
    # Make new dataframe to be filtered
    df_filtered = dfWL
    
    # Filters
    # By gender
    df_filtered = df_filtered[df_filtered$gender %in% input$gender,]
    # By year
    df_filtered = df_filtered[df_filtered$year %in% input$year,]
    # By weight class
    df_filtered = df_filtered[df_filtered$weightclass %in% input$weightclass,]
    
    # Possible x inputs
    if (input$x == "Bodyweight")
    {
      x = df_filtered$bodyweight
      xLabel = "Bodyweight [kg]"
    }
    else if (input$x == "Snatch")
    {
      x = df_filtered$snatch
      xLabel = "Snatch [kg]"
    }
    else if (input$x == "Clean & Jerk")
    {
      x = df_filtered$clean_and_jerk
      xLabel = "Clean & Jerk [kg]"
    }
    else if (input$x == "Total")
    {
      x = df_filtered$total
      xLabel = "Total [kg]"
    }
    else if (input$x == "Ranking")
    {
      x = df_filtered$ranking
      xLabel = "Ranking"
    }
    else
    {
      x = df_filtered$year
      xLabel = "Year"
    }
    
    # Possible y inputs
    if (input$y == "Bodyweight")
    {
      y = df_filtered$bodyweight
      yLabel = "Bodyweight [kg]"
    }
    else if (input$y == "Snatch")
    {
      y = df_filtered$snatch
      yLabel = "Snatch [kg]"
    }
    else if (input$y == "Clean & Jerk")
    {
      y = df_filtered$clean_and_jerk
      yLabel = "Clean & Jerk [kg]"
    }
    else if (input$y == "Total")
    {
      y = df_filtered$total
      yLabel = "Total [kg]"
    }
    else if (input$y == "Ranking")
    {
      y = df_filtered$ranking
      yLabel = "Ranking"
    }
    else
    {
      y = df_filtered$year
      yLabel = "Year"
    }
    
    
    # Color scheme
    # Gender
    if (input$color == "Gender")
    {
      color = df_filtered$gender
      color[color == "Men"] = "blue"
      color[color == "Women"] = "pink"
      legendLabels = genders
      legendColors = c("blue", "pink")
    }
    else if (input$color == "Year")
    {
      color = df_filtered$year
      color[color == "2000"] = "red"
      color[color == "2004"] = "orange"
      color[color == "2008"] = "yellow"
      color[color == "2012"] = "green"
      color[color == "2016"] = "blue"
      color[color == "2020"] = "purple"
      legendLabels = years
      legendColors = c("red", "orange", "yellow", "green", "blue", "purple")
    }
    else
    {
      color = "black"
    }
    
    # Linear regression
    relation = lm(y~x)
    
    # Plotting
    plot(x,
         y,
         col = color,
         xlab = xLabel,
         ylab = yLabel,
         main = "Weightlifting Characteristics",
         abline(relation))
    legend("bottomright",
           legend = legendLabels,
           col = legendColors,
           lty = 1:2,
           cex = 0.8)
    
    # update the summary text to render it in a different output
    stdError = paste("Std error:", toString(summary(relation)[4]))
    sigma = paste("Sigma:", toString(summary(relation)[6]))
    rsqr = paste("R-squared:", toString(summary(relation)[8]))
    adjrsqr = paste("Adj R-squred:", toString(summary(relation)[9]))
    fstat = paste("F statistic:", toString(summary(relation)[10]))
    summaryText <<- paste(stdError, sigma, rsqr, adjrsqr, fstat, sep = "\n")
    
  })
  
  # this renders the text to display the regression information
  output$regression = renderText(
    {
      # this function needs to read an input to actually update the text for the regression information
      something = input$x
      something = input$y
      something = input$gender
      something = input$year
      something = input$weightclass
      
      # actually update the text
      summaryText
    })
  
  output$writeup = renderText(
    {
      if (input$writeupSelect == "Introduction")
      {
        writeupText = "
                      This assignment outlines a dataset representing olympic weightlifters who participated in the Olympics from years 2000 to 2020. 
                      Linear regression is used to analyze this data to show those who are unfamiliar with weightlifting the correlation between a lifter's bodyweight and their snatch, clean and jerk, and total. 
                      Additionally, an overview of linear regression is included.
                      "
      }
      else if (input$writeupSelect == "Dataset")
      {
        writeupText = "
                      The dataset selected for this assignment was 'Summer Olympics Weightlifting records 2000 to 2020' found on Kaggle at 'https://www.kaggle.com/datasets/yuxinc/summer-olympics-weightlifting-records-2000-to-2020'. 
                      This dataset was created using data from Wikipedia and includes olympic weightlifers and their respective countries, bodyweights, snatches, clean and jerks, totals, ranks, and years they competed from the years 2000 to 2020. 
                      While fairly, clean, this dataset included multiple entries with erraneous data such as bodyweights of 0 kg and failed lifts where either the snatch or clean and jerk is 0 kg. 
                      Additionally, it should be noted that weight classes in weightlifting changed in the year 2020 which is important when using this applciation and discussed in the analysis. 
                      "
      }
      else if (input$writeupSelect == "Motivation")
      {
        writeupText = "
                      The motivation behind the selection of this dataset is my own personal interests in weightlifting and competition in general. 
                      A question posed in any ranked competition, especially the Olympics, is: 'Are athletes getting stronger as years pass?'. 
                      Additionally, while most are familiar with weightlifting, most are not aware of the scale at which world class athletes compete. 
                      "
      }
      else if (input$writeupSelect == "Analysis")
      {
        writeupText = "
                      The main question posed and the motivation behind the analysis of this dataset is: 'Are athletes getting stronger as the years pass?'. 
                      After setting the plot filters to include Men from years 2000 to 2020 in the +105kg and +109kg weight classes, it can be seen by the regression line that, on average, lifters are not getting stronger as time goes on. 
                      This can be seen further by switching to other weight classes such as the 69kg weight class for both men and women. 
                      This realization then poses other questions, one being: 'Why are world records still being set regularly?'. 
                      The answer to this question is simply that the Olympic comittee saw a drop in world records and decided to change the weightclasses such that new records could be set. 
                      This is very similar to changes in swimming at the Olympics where many world records were being broken once swimmers were allowed to perform flip turns. 
                      Another interested correltation to be made is the affect of snatch vs clean and jerk on a lifter's total. 
                      Theoretically, clean and jerk is a heavier lift, therefore can have more variability and thus would have a higher influence on a lifter's total. 
                      If the filters are changed to include both men and women for all years for all weight classes, the R-squared factors come to 0.9925 for snatch vs total and 0.9945 for clean and jerk vs total. 
                      While only marginally larger, clean and jerk is shown to have a stronger correlation on total rather than snatch, which aligns with our hypothesis. 
                      "
      }
      else if (input$writeupSelect == "Regression")
      {
        writeupText = "
                      Regression is a measure of the relation between two variables. 
                      While typically linear in the form of y = mx + b, regression can be nonlinear and take form in the shape of any equation. 
                      The main quantifier in determining if there is a relation between two variables is the R-squared value.
                      R-squared shows the strength of the relationship between the two variables with 1 being a perfect relationship and 0 being no relationship. 
                      The R-squared value is calculated by taking the sum of squared regression (SSR), dividing by the total sum of squares (SST), and subtracting that value from 1 as seen in ths equation: 1 - SSR/SST. 
                      The sum of squared regression (SSR) is simply the sum of the square of any data point minus that value along the regression line. 
                      This difference is also known as a residual. 
                      The total sum of squares (SST) is the sum of the distance the data is away from the mean all squared. 
                      "
      }
      writeupText
    }
  )
  
}

shinyApp(ui = ui, server = server)