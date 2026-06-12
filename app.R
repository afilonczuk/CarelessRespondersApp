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
      menuItem("Group-Level Visuals", tabName = "datavisuals", icon = icon("th")),
      menuItem("Explanations", tabName = "explanations", icon = icon("th")),
      menuItem("Descriptive Statistics", tabName = "descriptive-stats", icon = icon("th")),
      menuItem("Preregistration", tabName = "preregistration", icon = icon("list-alt")),
      menuItem("Individual Statistics & Flags", tabName = "downloading-data", icon = icon("list-alt")),
      menuItem("Help", tabName = "help", icon = icon("list-alt"))
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
    checkboxInput("userid", "User IDs (Must be first column)", FALSE),
    
    #Select if you would like item-level descriptive statistics. Note: only works 
    checkboxInput("descstats", "Item-Level Descriptive Statistics (Note: If your survey was randomized and your uploaded data is not organized by item, these statistics are not recommended.) ", FALSE),
    
    #Input number of levels in the likert scale
    numericInput("nlevels", "Number of Levels/Options in your Likert Scale", 5),
    #Input Lowest value in your scale (i.e. for a scale of length 5, does your scale begin at the level 0 and end at 4, or 1 and end at 5?)
    numericInput("lowlevel", "Lowest value in your scale (i.e. for a scale of length 5, does your scale begin at the level 0 and end at 4, or 1 and end at 5?)", 0),
    
    #Input validity item as the column number
    textInput("validityitem", 
              "Column Number(s) of Validity Item(s) (Comma-separated, leave blank if none)", 
              value = ""),
    
    #Input the correct answer to the validity item as a number
    textInput("correctanswer",
              "Correct Answer(s) to Validity Item(s) (Comma-separated, same order as above)",
              value = ""), 
    
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
  h2("Group-Level Visuals"),
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
      A careless responder will have a response that is different from the 'correct' answer ('1' in this case). 
        
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
      h4("lz Statistic:"),
      p("Calculates the lz person-fit statistic for polytomous data (Drasgow, Levine, and Williams, 1985) according to the Graded Response Model.
        A respondent is flagged if their lz statistic is less than the threshold according to the desired signifcance level and the standard normal distribution.
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


###Tab for downloading Individual Statistics & Flagged data ----
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
  ),

tabItem(
  tabName = "help", 
  h2("Help"),
  fluidRow(
    # Description of what app does
    mainPanel( 
      p("The purpose of this application is to allow researchers to upload their 
        data, view potential careless respondents in the data identified by statistical 
        methods, and independently decide which types of careless respondents should be removed."),
      h2("UPLOADING DATA:"),
      p("Under “Data Set of Responses,” a CSV file of likert-type data can be uploaded. 
        Once a file with the raw data is uploaded, a head of the first ten response 
        vectors will appear in this tab."),
      p("Check “Header” if the data has a header (column names)."),
      p("Select the type of separation in your data file (i.e. Are values comma - separated?)"),
      p("Select the quotes used to quote strings."),
      p("Check “User IDs” if the data includes an identification for each respondent 
        at the beginning of each response vector."),
      p("Check “Item-Level Descriptive Statistics” to obtain summaries for each item.
        If selected, a table with the mean and standard deviation for each item can be 
        downloaded in the “Individual Statistics & Flags” tab. In addition, bar plots displaying the 
        distribution of responses for select items can be found under the “Descriptive Statistics” tab."),
      p("Indicate the number of levels in the likert scale. For example, if respondents 
        could choose between five options, input 5. If there were three options (such as
        “Agree”, “Neutral”, and “Disagree”), input 3."),
      p("Indicate the lowest value in the likert scale. If the data includes five levels
        and ranges from zero to four, input 0, or if your data ranges from one to five, input 1."),
      p("If the data includes a validity item, input the number of the column 
        corresponding to the item. If no validity item was administered, leave blank 
        or enter “NA.” See “Method Explanations” tab for examples of a validity item."),
      p("Input the correct answer to the validity item."),
      p("Indicate the alpha that will be used to detect outlying responses in the 
        person-fit method. With a higher alpha value, more respondents will be marked 
        as potentially careless. By decreasing the alpha value, this method will flag 
        only the most aberrant responses. See “Method Explanations” tab for more 
        information on person-fit statistics."),
      p("Indicate the alpha that will be used to detect outlying responses in the 
        Mahalanobis distance method. With a higher alpha value, more respondents will 
        be marked as potentially careless. By decreasing the alpha value, this method 
        will flag only the most aberrant responses.  See “Method Explanations” tab for 
        more information on Mahalanobis distance."),
      h2("GROUP-LEVEL VISUALS:"),
      p("Please note that the output may take a few minutes to load, especially for large datasets."),
      p("The pie chart displays the relative number of responses flagged by each 
        method. Note: respondents flagged by more than one method are grouped 
        into one category, and each of the other categories (i.e. “IRV” and 
        “Longstring”) represent response vectors flagged by only that method."),
      p("'Rates at which each method flags respondents' displays the aggregate 
        number of respondents flagged by each method. Note: This chart does not 
        take into account respondents that may be flagged by two or more methods. 
        Thus, a respondent who is counted under “IRV” may also be counted under 
        “Longest String”. "),
      p("'Distribution of respondents based on how many times they were flagged' 
        displays the number of respondents who were flagged. For example, if 45 
        participants were deemed careless by two methods, the bar for “2” will 
        have a length of 45."),
      h2("EXPLANATIONS OF METHODS:"),
      p("This tab explains the methods used to detect potential careless respondents."),
      h2("DESCRIPTIVE STATISTICS:"),
      p("Please note that the output may take a few minutes to load, especially for large datasets."),
      p("If “Item-Level Descriptive Statistics” is selected in the “Uploading Data” 
        tab, two graphs will appear here. "),
      p("The first graph displays the distribution of responses for the ten items 
        with the highest number of “positive” responses relative to the median value 
        on the scale. For example, a scale ranging from 1-5 will have a median of 3, 
        and the items with the highest number of respondents choosing “4” and/or “5” 
        will be displayed here. "),
      p("The second graph displays the distribution of responses for the ten items 
        with the lowest number of “positive” responses relative to the median value 
        on the scale. For example, a scale ranging from 1-5 will have a median of 3, 
        and the items with the lowest number of respondents choosing “4” and/or “5”
        will be displayed here. "),
      h2("PREREGISTRATION:"),
      p("A chart describing the different types of carelessness detected by each 
        method is displayed here. An “X” indicates that the method will aid in 
        flagging participants who demonstrate the type of carelessness in the 
        column name. "),
      p("To add this table to a preregistration plan, the user can follow the 
        link to a Google Document to copy the table. "),
      h2("INDIVIDUAL STATISTICS & FLAGS:"),
      p("Please note that the output may take a few minutes to load, especially for large datasets."),
      p("Under “Choose a table to download,” users have the option to download 
        three different tables."),
      p("The “Raw Careless Statistics” table displays the raw statistics resulting 
        from each function for each person (ex. the chi-squared value for Mahalanobis
        distance or the length of the longest longstring). This file is helpful if the
        user would prefer to make decisions about which respondents should be “flagged,”
        instead of relying on the application."),
      p("The “Binary Flagged Table” replaces the raw careless statistics with a binary
        code - “1” if the value indicates the respondent may be careless or “0” if the
        value does not indicate careless responding under that method. This table is 
        useful if the user would like to rely on the standards used in the application
        in order to determine which responders are deemed careless."),
      p("The “Item-Level Descriptive Statistics” offers the mean and standard deviation
        for each item (Note: This table can only be downloaded if “Item-Level Descriptive
        Statistics” is selected in the “Uploading Data” tab).  "),
      p("Under “Flagged Respondents are highlighted in red,” an interactive table will 
        display the raw statistics from each method for each respondent, highlighting 
        cells in red when the respondent is found to be careless under that method.")
      
    )
)
)
)
))

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
    vi_raw <- trimws(unlist(strsplit(input$validityitem, ",")))
    ca_raw <- trimws(unlist(strsplit(input$correctanswer, ",")))
    
    if(length(vi_raw) == 0 || all(vi_raw == "")){
      validityitem <- NULL
      correctanswers <- NULL
      vi_names <- NULL
    }else{
      vi_cols <- as.integer(vi_raw)
      # vector of correct answers
      correctanswers <- as.numeric(ca_raw)
      # create names for validity items
      vi_names<- paste0("VI", vi_cols)
      # data frame, one column per validity item
      validityitem <- dat[, vi_cols, drop = FALSE] 
      # remove all validity cols
      tble <- tble[, -vi_cols]
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
      
      itemstats <- data.frame(Item = summary(likertsum)$Item, 
                              Mean = summary(likertsum)$mean,
                              Standard.Deviation = summary(likertsum)$sd)
    }
    
    #Function - Create table with the statistics produced by each method
    carelesstable <- function(dat2, confidencelevel = 0.01, validityitem = NULL, correctanswer = 0, vi_names = NULL, ncat, lowlevel){
      #dat2 is the subsetted data
      #validityitem = vector/df of responses to the validity item from the dataset
      #correctanswer = the correct answer(s) to the validity item
      #factors	= a vector of integers specifying the length of each factor in the dataset
      if(is.null(validityitem)){
        val <- NULL
      }else{
        # One flag column per validity item; respondent flagged if any item wrong
        val_matrix <- matrix(0, nrow = nrow(dat2), ncol = ncol(validityitem))
        for(k in 1:ncol(validityitem)){
          for(i in 1:nrow(validityitem)){
            if(is.na(validityitem[i, k])){
              val_matrix[i, k] <- NA
            }else if(validityitem[i, k] !=correctanswers[k]){
              val_matrix[i, k]<- 1
            }
          }
        }
        # Flag if failed any validity item
        val <- as.integer(apply(val_matrix, 1, function(r){
          if(all(is.na(r))) NA else as.integer(any(r == 1, na.rm = TRUE))
        }))
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
        out3 <- cbind( dat2.irv, as.integer(dat2.longstring$longstr), dat2.longstring$avgstr, m, pf$PFscores)
        colnames(out3) <- cbind( "IRV", "Longest String", "Average Longstring", "Mahalanobis D", "lz")
        }else{
          out3 <- cbind(as.integer(val_matrix), dat2.irv, as.integer(dat2.longstring$longstr), dat2.longstring$avgstr, m, pf$PFscores)
          colnames(out3) <- c(vi_names, "IRV", "Longest String", "Average Longstring", "Mahalanobis D", "lz")
        }
       
      return(list("out3"=out3, "val"=val, "mahad" = mahad))
    }
    
    ct_result <- carelesstable(tble, confidencelevel = input$confidence, validityitem, correctanswers, vi_names, ncat, input$lowlevel)
   int<-ct_result$out3
   val<- ct_result$val
   mahad<- ct_result$mahad
   
   #Function - Create table coded as flagged (1) or not flagged (0)
   flagged.table <- function(dat.table, vi_names, alpha, mahad) {
     out <- matrix(0, nrow = nrow(dat.table), ncol = ncol(dat.table))
     colnames(out) <- colnames(dat.table)
     
     if (!is.null(vi_names)) {
       # one binary column per validity item — already binary, copy directly
       for (nm in vi_names) {
         out[, nm] <- dat.table[, nm]
       }
     }
     
     out[, "IRV"] <- ifelse(
       is.na(dat.table[, "IRV"]), NA,
       ifelse(dat.table[, "IRV"] <= boxplot.stats(dat.table[, "IRV"])$stats[1] |
                dat.table[, "IRV"] >= boxplot.stats(dat.table[, "IRV"])$stats[5], 1, 0)
     )
     out[, "Longest String"] <- ifelse(
       is.na(dat.table[, "Longest String"]), NA,
       ifelse(dat.table[, "Longest String"] >= boxplot.stats(dat.table[, "Longest String"])$stats[5], 1, 0)
     )
     out[, "Average Longstring"] <- ifelse(
       is.na(dat.table[, "Average Longstring"]), NA,
       ifelse(dat.table[, "Average Longstring"] >= boxplot.stats(dat.table[, "Average Longstring"])$stats[5], 1, 0)
     )
     out[, "Mahalanobis D"] <- mahad
     out[, "lz"] <- ifelse(
       is.na(dat.table[, "lz"]), NA,
       ifelse(abs(dat.table[, "lz"]) > qnorm(1 - (alpha / 2)), 1, 0)
     )
     
     return(out)
   }
   bflagged<- flagged.table(int, vi_names, input$alpha, mahad)
 
   methodflagrate<- matrix(colSums(bflagged, na.rm = TRUE)/nrow(bflagged), nrow = 1,
                           dimnames = list(c("Percentage"), c(colnames(bflagged))) )
   
   #Number of times each student was flagged
   respondentflagrate <- as.data.frame(rowSums(bflagged, na.rm=T))
   colnames(respondentflagrate)<-"Flag Rate"
   #CREATE PIE CHART
   respondenttable <- t(table(respondentflagrate))
   
   pie <- matrix(data = 0, nrow = 1, ncol = ncol(bflagged)+1, 
                 dimnames = list(c("Number"), 
                                 c("Not Flagged", if(!is.null(validityitem)){"Validity Item"}, "IRV",
                                   "Longstring", "Mahalanobis D","lz",  "More than 1 method")))
   for (i in 1:nrow(respondentflagrate)){
     any_na <- is.na(respondentflagrate[i,1]) | is.na(val[i]) | 
       is.na(bflagged[i,"IRV"]) | is.na(bflagged[i,"Mahalanobis D"]) | 
       is.na(bflagged[i,"lz"])
     if (any_na) next
     
     vi_flag<- if(!is.null(vi_names)) as.integer(val[i]) else 0
     irv_flag<- bflagged[i,"IRV"]
     ls_flag<- as.integer(bflagged[i,"Longest String"]==1 | bflagged[i,"Average Longstring"]==1)
     mahad_flag <- bflagged[i,"Mahalanobis D"]
     pf_flag<- bflagged[i,"lz"]
     total<- vi_flag + irv_flag + ls_flag + mahad_flag + pf_flag
     
     if(respondentflagrate[i,1] == 0){
       pie[,1] <- pie[,1] + 1
     }else if(total > 1){
       pie[, "More than 1 method"] <- pie[, "More than 1 method"] + 1
     }else if(vi_flag == 1){ pie[, "Validity Item"]<- pie[, "Validity Item"] + 1
     }else if(irv_flag == 1){ pie[, "IRV"]<- pie[, "IRV"]+ 1
     }else if(ls_flag == 1){ pie[, "Longstring"]<- pie[, "Longstring"]+ 1
     }else if(mahad_flag == 1){ pie[, "Mahalanobis D"]<- pie[, "Mahalanobis D"]+ 1
     }else if(pf_flag == 1){ pie[, "lz"]<- pie[, "lz"]+ 1
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
   
   lbl = c("Not Flagged", if(!is.null(validityitem)){"Validity Item"}, "IRV", "Longstring", "Mahalanobis D", "lz", "More than 1 method")
   
   # Basic piechart
  piechart <-  ggplot(piedata, aes(x="", y=prop, fill=labels)) +
     geom_bar(stat="identity", width=1, color="white") +
     coord_polar("y", start=0) +
     theme_void() + 
     theme(legend.position="top") +
     scale_fill_brewer(palette="Set1")
   
   methodtable <- colSums(bflagged, na.rm = TRUE)
   
   #Function - Create table with the statistics produced by each method; highlighted in red if flagged as careless
   redflag.table <- function(dat.table, vi_names, alpha) {
     dt <- DT::datatable(dat.table, rownames = FALSE, options = list(scrollX = TRUE))
     
     if(!is.null(vi_names)){
       for(nm in vi_names){
         dt <- dt %>% formatStyle(columns = nm, background = styleEqual(1, "red"))
       }
     }
     
     irv_stats <- boxplot.stats(dat.table[, "IRV"])$stats
     dt <- dt %>%
       formatStyle("IRV",
                   background = styleInterval(c(irv_stats[1], irv_stats[5]), c("red", "white", "red"))) %>%
       formatStyle("Longest String",
                   background = styleInterval(boxplot.stats(dat.table[,"Longest String"])$stats[5], c("white","red"))) %>%
       formatStyle("Average Longstring",
                   background = styleInterval(boxplot.stats(dat.table[,"Average Longstring"])$stats[5], c("white","red"))) %>%
       formatStyle("Mahalanobis D",
                   background = styleInterval(
                     quantile(dat.table[, "Mahalanobis D"], probs = 1 - alpha, na.rm = TRUE),
                     c("white", "red") )) %>%
       formatStyle("Person Fit",
                   background = styleInterval(c(qnorm(alpha/2), qnorm(1-(alpha/2))), c("red","white","red")))
     
     return(dt)
   }
   
   #add user ids back on
   if(input$userid == TRUE){
     bflagged <- cbind(dat[,1], bflagged)
     int <- cbind(dat[,1], int)
     respondentflagrate <- cbind(dat[,1], respondentflagrate)
     colnames(int) <- colnames(bflagged) <- c("User ID", colnames(int[,2:ncol(int)]))
     colnames(respondentflagrate) <- c("User ID", "Flag Rate")
    }
    rflagged <- redflag.table(int, vi_names, input$alpha)
    
     return(list("int" = int, 
                "bflagged" = bflagged, 
                "methodflagrate" = methodflagrate,
                "respondentflagrate" = respondentflagrate,
                "rflagged" = rflagged,
                "piechart" = piechart,
                "methodtable" = methodtable,
                "respondenttable" =  respondenttable, 
                "itemstats" = if(input$descstats) itemstats else NULL,
                "descplot" = if(input$descstats) descplot else NULL
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
    req(input$dat2, input$descstats)
    out()$descplot$descplot1
  })
  #output the plot of the descriptive statistics (the last 10)

  output$descplot2 <- renderPlot({
    req(input$dat2, input$descstats)
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
      tbl <- if (input$userid) out()$bflagged[, -1] else out()$bflagged
      data <- data.frame(
        Method     = colnames(tbl),
        Percentage = colSums(tbl, na.rm = TRUE) / nrow(tbl),
        percentages = colSums(tbl, na.rm = TRUE) / nrow(tbl)
      )
      ggplot(data, aes(x = Method, y = Percentage, label = scales::percent(percentages))) +
        geom_col(position = "dodge", colour = "black", fill = "#00abff") +
        theme_linedraw() +
        geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3)
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
      filename = function(){
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file){
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
}

#run app ----
shinyApp(ui, server)
