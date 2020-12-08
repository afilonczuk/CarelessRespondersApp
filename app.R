library(tidyverse)
library(shiny)
library(careless)
library(DT)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(PerFit)
library(likert)

ui <- dashboardPage(

  # App title ----
  dashboardHeader(title="Careless Responders"),
  
  ## define sidebar ----
 dashboardSidebar(
   div(style="overflow-y: scroll;overflow-x: scroll"),
   sidebarMenu(
      menuItem("Uploading Data", tabName = "uploading-data", icon = icon("dashboard")),
      menuItem("Data Output", tabName = "datavisuals", icon = icon("th")),
      menuItem("Explanations", tabName = "explanations", icon = icon("th")),
      menuItem("Descriptive Statistics", tabName = "descriptive-stats", icon = icon("th")),
      menuItem("Preregistration", tabName = "preregistration", icon = icon("list-alt")),
      menuItem("Downloading Data", tabName = "downloading-data", icon = icon("list-alt"))
         )
  ),
  
  ## define body --- main part of the UI
dashboardBody(
  div(style="overflow-x: scroll"),
  
    tabItems(
      tabItem(
        tabName = "uploading-data",
        fluidPage(
      # Sidebar panel for inputs ----
      sidebarPanel(
        #input data as CSV file
        fileInput(inputId = "dat2", 
              label = "Data Set of Responses:", 
              multiple = FALSE, 
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"),
          ),
        checkboxInput("header", "Header", TRUE),
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                 choices = c(Space = " ",
                             Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    
        # Input: Select quotes ----
    radioButtons("quote", "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"'),
    #checkbox to indicate if userids are included
    checkboxInput("userid", "User IDs", FALSE),
    
    #Select if you would like item-level descriptive statistics. Note: only works 
    checkboxInput("descstats", "Item-Level Descriptive Statistics (Note: If your survey was randomized and your uploaded data is not organized by item, these statistics are not recommended.) ", FALSE),
    
    #Input number of levels in the likert scale
    numericInput("nlevels", "Number of Levels/Options in your Likert Scale", 5),
    #Input Lowest value in your scale (i.e. for a scale of length 5, does your scale begin at the level 0 and end at 4, or 1 and end at 5?)
    numericInput("lowlevel", "Lowest value in your scale (i.e. for a scale of length 5, does your scale begin at the level 0 and end at 4, or 1 and end at 5?)", 0),
    
    #Input validity item as the column number
    numericInput("validityitem", "Column Number of your Validity Item (leave NA if not administered)", NA),
    
      #Input the correct answer to the validity item as a number
    numericInput(inputId = "correctanswer",
                 label = "Correct answer to the validity item:",
                 value = 0),
    
    #Select your alpha for person-fit statistics on the normal distribution
    numericInput("alpha", "Alpha: Person-Fit Statistics", 0.01),
    
    numericInput(inputId = "confidence",
                 label = "Alpha: Mahalanobis Distance",
                 value = 0.01)
   
  ),
  
        # Main panel for displaying outputs: your file ----
  mainPanel(  
       p("Upload a CSV file containing a table of responses to run careless statistics on."),
    h2("Uploaded Data:"),
    
    div(style = 'overflow-x: scroll', tableOutput("original"))
 
  )
)),

tabItem(
  tabName = "datavisuals", 
  h2("Data Output"),
  fluidRow(
    # Main panel for displaying outputs ----
    mainPanel( 
      plotOutput("pie"),
      h3("Rates at which each method flags respondents:"),
      plotOutput("methodtable"),
      h3("Distribution of respondents based on how many times they were flagged"),
      plotOutput("respondenttable")
      #h3("Raw Careless Statistics"),
      #tableOutput("flaggedcarelesstable"),
      #h3("1 indicates being flagged by the method:"),
      #tableOutput("bflagged"),
      #h3("Number of times each student was flagged:"),
      #tableOutput("respondentflagrate")
     
    ))
),

  tabItem(
    tabName = "explanations", 
    h2("Explanations of Methods"),
############Here is where Max will explain info##############
    # Main panel for displaying outputs ----
fluidRow(
  mainPanel( 
    column(width = 10,
           div(
               style="overflow-x: scroll"),
           style = "border: 4px double black;",
           h2("Careless Responding"),
           h5("Survey participants are inattentive or do not properly read the item questions")
           ),
    
    column(width = 12,
           p("
             
             
             
             ")
           ),
    column(width = 12,
           div(
             style="overflow-x: scroll"),
           style = "border: 2px double black;",
      h4("Validity Item: "),
      p("An item used to make sure a respondent is paying attention. 
      Althouhg a validity item can take many forms, this application will detect careless responders from an item that asks the respondent to select a specific response 
      (ex. 'Please select option '1' to show that you are paying attention'). 
      A careless responder will have a response that is different from the expected answer ('1' in this case). 
        
        "),
      h4("Intra-individual response variability: "),
      p(" Determines the variability in one's responses and compares it to the distribution of all respondents' variability. Indicates potential careless respondents who respond randomly (high IRV) or select the same response repeatedly (low IRV). 
        
        "),
      h4("Longstring: "),
      p("The highest number of items in which a respondent has selected the same response consecutively. 
      (ex. selecting '1, 1, 1, 1...' etc.). This type of carelessness may also be known as straightlining. 
      This application will consider the distribution of all respondents' longstrings and flag responders if their 
      highest longstring is an outlier in the distribution (greater than 1.5 times the average longstring).
      
        "),
      h4("Average Longstring: "),
      p("A respondent's average number of longstrings (the average length of consecutive responses). 
      This method may be useful if a respondent is straightlining and switches their response multiple times during the test administration 
      (ex. a respondent may choose answer '1' consecutively but begin answering '2' consecutively on the next page of the test).
        
        "),
      h4("Mahalanobis Distance:"),
      p("Highlights response vectors that differ greatly from the average distribution of responses on the chi-square distribution.
        
        "),
      h4("Person Fit:"),
      p("Highlights response vectors that differ greatly from the average distribution of responses on the normal curve. 
      Given a z-score that that is above or below the critical value found using alpha/2 % level, a person will be flagged.
        
        ")
      ))
)
         ),

tabItem(
  tabName = "descriptive-stats",
  h2("Descriptive Statistics"),
  fluidPage(
    h4("Items with the most positive number of responses"),
    plotOutput("descplot1" ),
    h4("Items with the most negative number of responses"),
    plotOutput("descplot2" )
  )
),

###Tab for Preregistration ----
tabItem(
  tabName = "preregistration", 
  h2("Choosing Methods for Preregistration"),
  column(width = 7,
         DT::dataTableOutput("PreReg")
  ),
  h4("View this table in a Google Document to copy and add to your plan"),
  uiOutput("url")
  
),


###Tab for downloading data ----
tabItem(
  tabName = "downloading-data", 
  h2("Download Your Data with Careless Statistics Attached"),
  
      fluidRow(
        div(style="overflow-x: scroll"),
               
        column(width = 5,
               div(style="overflow-x: scroll"),
               
               #box(
      # Input: Choose dataset ----
      selectInput("dataset", "Choose a table to download:",
                  choices = c("Raw Careless Statistics", "Binary Flagged Table", "Item-Level Descriptive Statistics")),
      
      # Button
      downloadButton("downloadData", "Download"),
      div(style = 'overflow-x: scroll', tableOutput("tableout"))
      
      ),
      column(7,
             div(style="overflow-x: scroll"),
             
             h3("Flagged Respondents are highlighted in red"),
             DT::dataTableOutput("rflagged"),
             p("A blank cell indicates an NA result for that method and person.")
      )
    )
      )
    )
)
)

server <- function(input, output){
  
  # Reactive component ----
  out  <- reactive({
    req(input$dat2)
    tble <- dat <- read.csv(input$dat2$datapath,
                    header = input$header,
                    sep = input$sep,
                    quote = input$quote)
    
    
     ncat<-input$nlevels
    #remove the validity item from your dataset before running statistics
    if(is.null(input$validityitem)||is.na(input$validityitem)){
      validityitem <- NULL
    }else{
      tble <- tble[,-input$validityitem]
      validityitem <-dat[,as.numeric(input$validityitem)]
    } 
   
    #remove user ids from data before running statistics
    if(input$userid == TRUE){
      tble <- tble[,-1]
    }
    
    if (input$descstats){
      #Descriptive Stats
      if (is.na(input$nlevels)){
        likertsum <- likert(tble)               
      }else{
        likertsum <- likert(tble, nlevels = input$nlevels)             
      }
      if(nrow(tble)>20){
        likhead<- likertsum$items[, colnames(likertsum$items) %in% (head(summary(likertsum), 10))$Item ]
        liktail<- likertsum$items[, colnames(likertsum$items) %in% (tail(summary(likertsum), 10))$Item ]
        
        descplot1 <- likert:::plot.likert(likert(likhead), type = "bar",  centered = TRUE)
        descplot2 <- likert:::plot.likert(likert(liktail), type = "bar", centered = TRUE )
        descplot <- list(
          "descplot1" = descplot1,
          "descplot2" = descplot2
        )
      }else{
        descplot1 <- likert:::plot.likert(likertsum, type = "bar", centered = TRUE )
        descplot <- list(
          "descplot1" = descplot1)
      }
    }
    
    itemstats <- data.frame(Item = summary(likertsum)$Item, 
                            Mean = summary(likertsum)$mean,
                            Standard.Deviation = summary(likertsum)$sd
    )
    #Function - Create table with the statistics produced by each method
    carelesstable <- function(dat2, confidencelevel = 0.01, validityitem = NULL, correctanswer = 0, ncat, lowlevel){
      #dat2 is the subsetted data
      #validityitem = vector of responses to the validity item from the administration subsetted
      #correctanswer = the correct answer to the validity item
      #factors	= a vector of integers specifying the length of each factor in the dataset
      if (is.null(validityitem) ){
        val <- matrix(NA, nrow = nrow(dat2), ncol = 1)
      }else {
        val <- matrix(0, nrow = nrow(dat2), ncol = 1)
        validityitem <- as.matrix(validityitem, ncol = 1)
        #the val column will report 1 for an incorrect (careless) response, 0 for correct response, and NA for NA
        for (i in 1:length(validityitem)){
          if (is.na(validityitem[i])){
            val[i] <- NA
          } else
            if (validityitem[i] != correctanswer){
              val[i] <- 1
            }
        }
      }
      #IRV 
      dat2.irv <- irv(dat2)
      #Longstring
      dat2.longstring <- longstring(dat2, avg = TRUE)
      
      #Mahalanobis Distance (D) and flag potential outlier
      m<-mahalanobis(dat2, colMeans(as.matrix(dat2),na.rm = T),cov(as.matrix(dat2), use="complete.obs"))
      p <- pchisq(sqrt(m), df=ncol(dat2)-1, lower.tail=FALSE)
      mahad<- matrix(data = 0, nrow = nrow(dat2))
      for (i in 1:length(p)){
        if (!is.na(p[i]) && p[i] < 1-confidencelevel){
          mahad[i]<- 1
        }
      }
      
      pf<- PerFit::lzpoly(dat2-lowlevel, Ncat = ncat)
      
      if(is.null(validityitem)){
        out3 <- cbind( dat2.irv, as.integer(dat2.longstring$longstr), dat2.longstring$avgstr, as.integer(mahad), pf$PFscores)
        colnames(out3) <- cbind( "IRV", "Longest String", "Average Longstring", "Mahalanobis D", "Person Fit")
        }else{
          out3 <- cbind(as.integer(val), dat2.irv, as.integer(dat2.longstring$longstr), dat2.longstring$avgstr, as.integer(mahad), pf$PFscores)
          colnames(out3) <- cbind("Validity Item", "IRV", "Longest String", "Average Longstring", "Mahalanobis D", "Person Fit")
        }
       
      return(out3)
    }
   int <- carelesstable(tble, confidencelevel = input$confidence, validityitem, input$correctanswer, ncat, input$lowlevel)
  
   #Function - Create table coded as flagged (1) or not flagged (0)
   flagged.table <- function(dat.table, validityitem, alpha){
     out <- matrix(0, nrow = nrow(dat.table), ncol = ncol(dat.table))
    if(is.null(validityitem)||is.na(input$validityitem)){
       for (i in 1:nrow(dat.table)){
         if(is.na(dat.table[i,1]) ){
           out[i,1]<- NA
         } else
           if (dat.table[i,1] <= boxplot.stats(dat.table[,1])$stats[1] || dat.table[i,1] >= boxplot.stats(dat.table[,1])$stats[5]){
             out[i,1] <-  1
           }
       }
       for (i in 1:nrow(dat.table)){
         for (j in 2:3){
           if(is.na(dat.table[i,j])){
             out[i,j] <- NA
           } else if(dat.table[i,j] >= boxplot.stats(dat.table[,j])$stats[5] ){
             out[i,j] <- 1
           }
         }
       }
       out[,4] <- dat.table[,4]
       for (i in 1:nrow(dat.table)){
         if(is.na(dat.table[i,5])){
           out[i,5] <- NA
         } else if(abs(dat.table[i,5]) >qnorm(1-(alpha/2))){
           out[i,5] <- 1
         }
       }
    }else{
      out[,1] <- dat.table[,1]
      for (i in 1:nrow(dat.table)){
        if(is.na(dat.table[i,2]) ){
          out[i,2]<- NA
        } else
          if (dat.table[i,2] <= boxplot.stats(dat.table[,2])$stats[1] || dat.table[i,2] >= boxplot.stats(dat.table[,2])$stats[5]){
            out[i,2] <-  1
          }
      }
      for (i in 1:nrow(dat.table)){
        for (j in 3:4){
          if(is.na(dat.table[i,j])){
            out[i,j] <- NA
          } else if(dat.table[i,j] >= boxplot.stats(dat.table[,j])$stats[5] ){
            out[i,j] <- 1
          }
        }
      }
      out[,5] <- dat.table[,5]
      for (i in 1:nrow(dat.table)){
        if(is.na(dat.table[i,6])){
          out[i,6] <- NA
        } else if( abs(dat.table[i,6]) >qnorm(1-(alpha/2))){
          out[i,6] <- 1
        }
      }
    }
     colnames(out) <- colnames(dat.table)
     return(out)
   }
   bflagged<- flagged.table(int, validityitem, input$alpha)
 
   methodflagrate<- matrix(colSums(bflagged, na.rm = TRUE)/nrow(bflagged), nrow = 1,
                           dimnames = list(c("Percentage"), c(colnames(bflagged))) )
   
   #Number of times each student was flagged
   respondentflagrate <- as.data.frame(rowSums(bflagged, na.rm=T))
   colnames(respondentflagrate)<-"Flag Rate"
   #CREATE PIE CHART
   respondenttable <- t(table(respondentflagrate))
   
   pie <- matrix(data = 0, nrow = 1, ncol = ncol(bflagged)+1, 
                 dimnames = list(c("Number"), c("Not Flagged", if(!is.null(validityitem)){"Validity Item"}, "IRV", "Longstring", "Mahalanobis D","Person Fit",  "More than 1 method")))
   if(is.null(validityitem)){
     for(i in 1:nrow(respondentflagrate)){
       
       if(is.na(respondentflagrate[i,1])||is.na(bflagged[i,1])||is.na(bflagged[i,2])||is.na(bflagged[i,3])||is.na(bflagged[i,4])||is.na(bflagged[i,5]))
       {}else
         if(respondentflagrate[i,1]==0){
           pie[,1] <- pie[,1] +1
         }else
           if (bflagged[i,1] ==1 && bflagged[i,2] == 0 && bflagged[i,3] ==0 && bflagged[i,4] ==0 && bflagged[i,5] ==0){
            pie[,2] <- pie[,2] +1
           }else
             if (bflagged[i,1] ==0 && (bflagged[i,2] ==1 || bflagged[i,3] ==1) && bflagged[i,4] ==0&& bflagged[i,5] ==0){
               pie[,3] <- pie[,3] +1
             }else
               if (bflagged[i,1] ==0 && bflagged[i,2] ==0 && bflagged[i,3] ==0 && bflagged[i,4] ==1&& bflagged[i,5] ==0){
                 pie[,4] <- pie[,4] +1
               }else
                 if (bflagged[i,1] ==0 && bflagged[i,2] ==0 && bflagged[i,3] ==0 && bflagged[i,4] ==0 && bflagged[i,5] ==1){
                   pie[,5] <- pie[,5] +1
               }else {
                 pie[,6] <- pie[,6] + 1
               }
     }
   }else{
     for(i in 1:nrow(respondentflagrate)){
       
       if(is.na(respondentflagrate[i,1])||is.na(bflagged[i,1])||is.na(bflagged[i,2])||is.na(bflagged[i,3])||is.na(bflagged[i,4])||is.na(bflagged[i,5])||is.na(bflagged[i,6]))
       {
       }else
         if(respondentflagrate[i,1]==0){
           pie[,1] <- pie[,1] +1
         }else
           if (bflagged[i,1] ==1 && bflagged[i,2] == 0 && bflagged[i,3] ==0 && bflagged[i,4] ==0 
               && bflagged[i,5] ==0 && bflagged[i,6] ==0){
             pie[,2] <- pie[,2] +1
           }else
             if (bflagged[i,1] ==0 && bflagged[i,2] ==1 && bflagged[i,3] ==0 && bflagged[i,4] ==0  
                 && bflagged[i,5] ==0 && bflagged[i,6] ==0){
               pie[,3] <- pie[,3] +1
             }else
               if (bflagged[i,1] ==0 && bflagged[i,2] ==0 && (bflagged[i,3] ==1 || bflagged[i,4] ==1) 
                   && bflagged[i,5] ==0 && bflagged[i,6] ==0){
                 pie[,4] <- pie[,4] +1
               }else
                 if (bflagged[i,1] ==0 && bflagged[i,2] ==0 && bflagged[i,3] ==0 && bflagged[i,4] ==0 
                     && bflagged[i,5] ==1 && bflagged[i,6] ==0){
                   pie[,5] <- pie[,5] +1
                 }else
                   if (bflagged[i,1] ==0 && bflagged[i,2] ==0 && bflagged[i,3] ==0 && bflagged[i,4] ==0 
                       && bflagged[i,5] ==0 && bflagged[i,6] ==1){
                     pie[,6] <- pie[,6] +1
                   }else {
                   pie[,7] <- pie[,7] + 1
                 }
     }
   }
 
   
   piedata <- data.frame(
     Number = pie[1,],
     name = c(colnames(pie))
   )
    
   # Compute the position of labels
   piedata <- piedata %>% 
     arrange(desc(name)) %>%
     mutate(prop = Number / sum(piedata$Number) *100) %>%
     mutate(ypos = cumsum(prop)- 0.5*prop )%>%
    mutate(labels = paste0(name, " ", round(prop, 1), "%"))
   
   lbl = c("Not Flagged", if(!is.null(validityitem)){"Validity Item"}, "IRV", "Longstring", "Mahalanobis D", "Person Fit", "More than 1 method")
   
   # Basic piechart
  piechart <-  ggplot(piedata, aes(x="", y=prop, fill=labels)) +
     geom_bar(stat="identity", width=1, color="white") +
     coord_polar("y", start=0) +
     theme_void() + 
     theme(legend.position="top") +
     scale_fill_brewer(palette="Set1")
   
   methodtable <- colSums(bflagged, na.rm = TRUE)
  
   #add user ids back on
   if(input$userid == TRUE){
     bflagged <- cbind(dat[,1], bflagged)
     int <- cbind(dat[,1], int)
     respondentflagrate <- cbind(dat[,1], respondentflagrate)
     colnames(int) <- colnames(bflagged) <- c("User ID", colnames(int[,2:ncol(int)]))
     colnames(respondentflagrate) <- c("User ID", "Flag Rate")
     
     #Function - Create table with the statistics produced by each method; highlighted in red if flagged as careless
     redflag.table <- function(dat.table, validityitem, alpha){
       
       if(!is.null(validityitem)){ DT::datatable(dat.table, rownames = FALSE,options = list(scrollX=TRUE)) %>%
           formatStyle(columns = "Validity Item", 
                       background = styleEqual(c(1), c("red"))) %>%
           formatStyle(columns = "IRV",           #This is for data with User Ids
                       background = styleInterval(c(boxplot.stats(dat.table[,3])$stats[1], boxplot.stats(dat.table[,3])$stats[5]), c("red", "white", "red"))) %>%
           formatStyle(columns = "Longest String", 
                       background = styleInterval(boxplot.stats(dat.table[,4])$stats[5], c( "white", "red"))) %>%
           formatStyle(columns = "Average Longstring", 
                       background = styleInterval(boxplot.stats(dat.table[,5])$stats[5], c("white", "red"))) %>%
           formatStyle(columns = "Mahalanobis D", 
                       background = styleEqual(1, c("red"))) %>%
           formatStyle(columns = "Person Fit", 
                       background = styleInterval(c(qnorm(alpha/2),qnorm(1-(alpha/2))), c("red", "white", "red")))
       }else{
         DT::datatable(dat.table, rownames = FALSE,options = list(scrollX=TRUE)) %>%
           formatStyle(columns = "IRV",           #This is for data with User Ids
                       background = styleInterval(c(boxplot.stats(dat.table[,2])$stats[1], boxplot.stats(dat.table[,2])$stats[5]), c("red", "white", "red"))) %>%
           formatStyle(columns = "Longest String", 
                       background = styleInterval(boxplot.stats(dat.table[,3])$stats[5], c( "white", "red"))) %>%
           formatStyle(columns = "Average Longstring", 
                       background = styleInterval(boxplot.stats(dat.table[,4])$stats[5], c("white", "red"))) %>%
           formatStyle(columns = "Mahalanobis D", 
                       background = styleEqual(1, c("red"))) %>%
           formatStyle(columns = "Person Fit", 
                       background = styleInterval(c(qnorm(alpha/2),qnorm(1-(alpha/2))), c("red", "white", "red")))
       }
     }
       }else{
     #Function - Create table with the statistics produced by each method; highlighted in red if flagged as careless
     redflag.table <- function(dat.table, validityitem, alpha){
       
       if(!is.null(validityitem)){ DT::datatable(dat.table, rownames = FALSE,options = list(scrollX=TRUE)) %>%
           formatStyle(columns = "Validity Item", 
                       background = styleEqual(c(1), c("red"))) %>%
           formatStyle(columns = "IRV",           #This is for data without User Ids
                       background = styleInterval(c(boxplot.stats(dat.table[,2])$stats[1], boxplot.stats(dat.table[,2])$stats[5]), c("red", "white", "red"))) %>%
           formatStyle(columns = "Longest String", 
                       background = styleInterval(boxplot.stats(dat.table[,3])$stats[5], c( "white", "red"))) %>%
           formatStyle(columns = "Average Longstring", 
                       background = styleInterval(boxplot.stats(dat.table[,4])$stats[5], c("white", "red"))) %>%
           formatStyle(columns = "Mahalanobis D", 
                       background = styleEqual(1, c("red"))) %>%
           formatStyle(columns = "Person Fit", 
                       background = styleInterval(c(qnorm(alpha/2),qnorm(1-(alpha/2))), c("red", "white", "red")))
       }else{
         DT::datatable(dat.table, rownames = FALSE,options = list(scrollX=TRUE)) %>%
           formatStyle(columns = "IRV",           #This is for data without User Ids
                       background = styleInterval(c(boxplot.stats(dat.table[,1])$stats[1], boxplot.stats(dat.table[,1])$stats[5]), c("red", "white", "red"))) %>%
           formatStyle(columns = "Longest String", 
                       background = styleInterval(boxplot.stats(dat.table[,2])$stats[5], c( "white", "red"))) %>%
           formatStyle(columns = "Average Longstring", 
                       background = styleInterval(boxplot.stats(dat.table[,3])$stats[5], c("white", "red"))) %>%
           formatStyle(columns = "Mahalanobis D", 
                       background = styleEqual(1, c("red"))) %>%
           formatStyle(columns = "Person Fit", 
                       background = styleInterval(c(qnorm(alpha/2),qnorm(1-(alpha/2))), c("red", "white", "red")))
       }
     }
     }
    rflagged <- redflag.table(int, validityitem, input$alpha)
    
     return(list("int" = int, 
                "bflagged" = bflagged, 
                "methodflagrate" = methodflagrate,
                "respondentflagrate" = respondentflagrate,
                "rflagged" = rflagged,
                "piechart" = piechart,
                "labels" = labels,
                "methodtable" = methodtable,
                "respondenttable" =  respondenttable, 
                "itemstats" = itemstats,
                "descplot" = descplot
                ))
})
  #Preregistration Table
  prereg <- reactive({
    prereg <- matrix(data = NA, nrow =6, ncol = 4 )
    colnames(prereg) <- c("Random Responses", "Streamlining", "Outlying Response Vectors", "Indices Planned to Use (check all that apply)")
    row.names(prereg) <- c("Validity Item", "IRV", "Longstring", "Average Longstring", "Mahalanobis Distance", "Person-Fit")
    prereg[1,1] <- prereg[2,1] <- prereg[1,2] <- prereg[2,2] <- prereg[3,2] <- prereg[4,2] <- prereg[5,3] <- prereg[6,3] <- "X"
    return("prereg" = prereg)
    })
  output$PreReg <- DT::renderDataTable({
    DT::datatable( prereg(), rownames = T, options = list(scrollX=TRUE, searching = F, ordering= F, paging = F))
  })
  output$url <- renderUI({
    tagList("URL link:", a("Google Docs", target="_blank", href="https://docs.google.com/document/d/1RoOpnapqNofecapZJlodQsNwdAgOd_OhPG3dkN6h1cQ/"))
  })
  
  #output the plot of the descriptive statistics (the first 10)
  output$descplot1 <- renderPlot({
    req(input$dat2)
    out()$descplot$descplot1
  })
  #output the plot of the descriptive statistics (the last 10)

   output$descplot2 <- renderPlot({
    req(input$dat2)
    out()$descplot$descplot2
  })
 
  # Output a html table highlighting those flagged in red ----
  output$flaggedcarelesstable <- renderTable({
    req(input$dat2)
    head(out()$int, 10)
    })

    output$original <- renderTable({
      req(input$dat2)
      head(read.csv(input$dat2$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote), 10)
    })
    output$bflagged <- renderTable({
      req(input$dat2)
      head(out()$bflagged, 10)
    })
    # Output a table with the rate at which each method flags students ----
    output$methodflagrate <- renderTable({
      req(input$dat2)
      out()$methodflagrate
    })
    
    #output a table that counts the number of times each respondent is flagged
    output$respondentflagrate <- renderTable({
      head(out()$respondentflagrate, 10)
    })
    #Output Red Flagged Table
    output$rflagged <- DT::renderDataTable(
      out()$rflagged
      #, options = list(scrollX=TRUE, scrollCollapse=TRUE)
    )
   #Output pie chart
    output$pie <- renderPlot({
      out()$piechart
    })
    output$respondenttable <- renderPlot({
      rtdata <- data.frame(
        NumberOfFlags = c(colnames(out()$respondenttable)),
        NumberOfStudents = c(out()$respondenttable[1,])
      )
      b <- ggplot(rtdata, aes(x = NumberOfFlags, y=NumberOfStudents, label = (NumberOfStudents)))
      b + geom_col(position = 'dodge',colour = "black", fill = "green") +
        theme_linedraw()+
        geom_text(position = position_dodge(width = .9),    # move to center of bars
                  vjust = -0.5,  size = 3) 
    })
    output$methodtable <- renderPlot({
      
     if(input$userid){ 
       data <- data.frame(
        Method = c(colnames(out()$bflagged[,-1])),
        Percentage = colSums(out()$bflagged[,-1], na.rm = TRUE)/nrow(out()$bflagged[,-1]),
        percentages = colSums(out()$bflagged[,-1], na.rm = TRUE)/nrow(out()$bflagged[,-1])
      )
      a <- ggplot(data, aes(x = Method, y=Percentage, label = scales::percent(percentages)))
      a + geom_col(position = 'dodge',colour = "black", fill = "#00abff") +
        theme_linedraw()+
        geom_text(position = position_dodge(width = .9),    # move to center of bars
                  vjust = -0.5,  size = 3) 
     }else{
       data <- data.frame(
         Method = c(colnames(out()$bflagged)),
         Percentage = colSums(out()$bflagged, na.rm = TRUE)/nrow(out()$bflagged),
         percentages = colSums(out()$bflagged, na.rm = TRUE)/nrow(out()$bflagged)
       )
       a <- ggplot(data, aes(x = Method, y=Percentage, label = scales::percent(percentages)))
       a + geom_col(position = 'dodge',colour = "black", fill = "#00abff") +
         theme_linedraw()+
         geom_text(position = position_dodge(width = .9),    # move to center of bars
                   vjust = -0.5,  size = 3) 
     }
    })
    # Reactive value for selected dataset ----
    datasetInput <- reactive({
      switch(input$dataset,
             "Raw Careless Statistics" = out()$int,
             "Binary Flagged Table" = out()$bflagged,
             "Item-Level Descriptive Statistics" = out()$itemstats)
    })
    
    # Table of selected dataset ----
    output$tableout <- renderTable({
      head(datasetInput(), 10)
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

#run app ----
shinyApp(ui, server)

