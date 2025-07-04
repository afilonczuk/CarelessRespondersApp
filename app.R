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
     # Main panel for displaying the explanation page ----    
        fluidRow(
          mainPanel( 
            # Overview & Statistical Thresholds Section
            column(width = 12,
                   div(style="overflow-x: scroll"),
                   style = "border: 2px double black;",
                   h2("Careless Responding"),
                   p("Survey participants are inattentive or do not properly read the item questions"),
                   hr(),
                   
                   h3("Overview & Statistical Thresholds"),
                   p(strong("All careless responding detection methods follow this statistical framework:")),
                   p("• Calculate a specific metric for each respondent based on their response pattern"),
                   p("• Aggregate these individual metrics to form a group distribution"),
                   p("• Apply statistical thresholds to identify outliers or unusual patterns"),
                   p("• Flag respondents exceeding thresholds for further review or exclusion"),
                   hr(),
                   
                   h3("Understanding Statistical Thresholds"),
                   h4("Percentile-Based Approaches:"),
                   withMathJax("$$P_{5} \\quad \\text{and} \\quad P_{95}$$"),
                   p("• \\(P_{5}\\): 5th percentile - flags bottom 5% of distribution"),
                   p("• \\(P_{95}\\): 95th percentile - flags top 5% of distribution"),
                   p("• Example: If \\(P_{95}(IRV) = 1.5\\), then respondents with \\(IRV > 1.5\\) are flagged"),
                   hr(),
                   
                   h4("Standard Deviation-Based Approaches:"),
                   withMathJax("$$\\bar{x} \\pm k \\cdot SD$$"),
                   p("• \\(k = 1.5\\): Moderate threshold (flags ~13% outside range)"),
                   p("• \\(k = 2.0\\): Conservative threshold (flags ~5% outside range)"),
                   p("• \\(k = 3.0\\): Very conservative threshold (flags ~0.3% outside range)"),
                   hr(),
                   
                   h4("Significance Levels:"),
                   withMathJax("$$\\alpha \\in \\{0.05, 0.01, 0.001\\}$$"),
                   p("• \\(\\alpha = 0.05\\): Standard level, accepts 5% Type I error rate"),
                   p("• \\(\\alpha = 0.01\\): Conservative level, accepts 1% Type I error rate"),
                   p("• \\(\\alpha = 0.001\\): Very conservative, accepts 0.1% Type I error rate"),
                   p(strong("Note:"), " Lower \\(\\alpha\\) reduces false positives but may miss genuine careless responders. Adjust based on your research context.")
            ),
            
            column(width = 12,
                   p("")),
            
            # Validity Items
            column(width = 12,
                   div(style="overflow-x: scroll"),
                   style = "border: 2px double black;",
                   h3("Validity Items"),
                   p("Validity items are strategically placed questions designed to verify respondent attention and comprehension. They serve as direct checks on whether participants are reading and processing survey content appropriately."),
                   hr(),
                   
                   h4("Types and Implementation"),
                   
                   p(strong("1. Direct Instruction Items")),
                   p("Explicitly instruct respondents to select a specific response."),
                   p(em("Example:"), " 'To demonstrate you are paying attention, please select Strongly Disagree for this item.'"),
                   p("• Expected: Strongly Disagree"),
                   p("• Flag if: Any other response"),
                   hr(),
                   
                   p(strong("2. Arithmetic Check Items")),
                   p("Simple mathematical problems requiring minimal cognitive effort."),
                   p(em("Example:"), " 'What is \\(2 + 3\\)?'"),
                   p("• Options: 1, 3, 5, 7, 9"),
                   p("• Expected: 5"),
                   p("• Flag if: \\(response \\neq 5\\)"),
                   hr(),
                   
                   p(strong("3. Bogus Items")),
                   p("Statements that are objectively false or impossible."),
                   p(em("Examples:")),
                   p("• 'I have visited every planet in the solar system'"),
                   p("• 'I am answering this survey from the year 1850'"),
                   p("• 'I have never used water in my entire life'"),
                   p("Flag if: Agreement (\\(response \\geq 4\\) on 1-5 scale)"),
                   hr(),
                   
                   h4("Implementation Guidelines"),
                   p("• Include 3-5 validity items per 100 survey questions"),
                   p("• Distribute throughout survey (beginning, middle, end)"),
                   p("• Vary types to catch different inattention patterns"),
                   p("• Consider multiple failures: \\(failures \\geq 2\\) → exclude data")
            ),
            
            column(width = 12,
                   p("")),
            
            # Intra-individual Response Variability (IRV)
            column(width = 12,
                   div(style="overflow-x: scroll"),
                   style = "border: 2px double black;",
                   h3("Intra-individual Response Variability (IRV)"),
                   p("IRV measures the standard deviation of an individual's responses across items. It quantifies response consistency - low values suggest uniform responding (potential straight-lining), while high values indicate erratic patterns (potential random responding). Originally introduced by Dunn et al. (2018), it is also known as the Inter-Item Standard Deviation (ISD) in Marjanovic et al. (2015)."),
                   hr(),
                   
                   h4("Mathematical Foundation"),
                   withMathJax("$$IRV = \\sqrt{\\frac{\\sum_{j=1}^{J}(x_j-\\bar{x})^2}{J-1}}$$"),
                   
                   p(strong("Where:")),
                   p("• \\(x_j\\) = response to item \\(j\\)"),
                   p("• \\(\\bar{x} = \\frac{1}{J}\\sum_{j=1}^{J}x_j\\) = mean response across all items"),
                   p("• \\(J\\) = total number of items"),
                   p("• \\(J-1\\) = degrees of freedom"),
                   p("This is the standard deviation of responses across consecutive item responses for an individual."),
                   hr(),
                   
                   h4("Detailed Example"),
                   p(strong("Participant responses to 20 items (1-5 scale):")),
                   p(code("[5, 1, 5, 1, 5, 5, 5, 5, 1, 5, 3, 2, 4, 1, 5, 2, 5, 1, 5, 3]")),
                   
                   p(strong("Calculation:")),
                   p("• \\(\\bar{x} = \\frac{5+1+5+...+3}{20} = \\frac{70}{20} = 3.5\\)"),
                   p("• Deviations: \\((5-3.5), (1-3.5), ... = 1.5, -2.5, 1.5, ...\\)"),
                   p("• Squared deviations: \\(2.25, 6.25, 2.25, ...\\)"),
                   p("• \\(\\sum(x_j-\\bar{x})^2 = 58.0\\)"),
                   p("• \\(\\frac{58.0}{19} = 3.053\\)"),
                   p("• \\(IRV = \\sqrt{3.053} = 1.749\\)"),
                   hr(),
                   
                   h4("Interpretation"),
                   p(strong("Research-based thresholds"), " (Dunn et al., 2018; Marjanovic et al., 2015):"),
                   p("• \\(IRV < 0.5\\): Extremely low variability → likely straight-lining"),
                   p("• \\(0.5 \\leq IRV < 0.9\\): Low variability → possible patterned responding"),
                   p("• \\(0.9 \\leq IRV \\leq 1.5\\): Normal range → engaged responding"),
                   p("• \\(1.5 < IRV \\leq 2.0\\): High variability → potential carelessness"),
                   p("• \\(IRV > 2.0\\): Very high variability → likely random responding"),
                   
                   p(strong("Note:"), " While Dunn et al. (2018) proposed flagging low IRV scores as outliers (reflecting straight-lining), Marjanovic et al. (2015) proposed flagging high IRV scores (reflecting random responding)."),
                   p(strong("In our example:"), " \\(IRV = 1.749\\) exceeds the threshold of 1.5, suggesting potential random responding.")
            ),
            
            column(width = 12,
                   p("")),
            
            # Maximum Longstring
            column(width = 12,
                   div(style="overflow-x: scroll"),
                   style = "border: 2px double black;",
                   h3("Maximum Longstring"),
                   p("Maximum Longstring identifies the longest sequence of consecutive identical responses. It captures respondents who may have stopped paying attention and repeatedly selected the same option. Introduced by Johnson (2005), this index is particularly effective for detecting straight-lining behavior."),
                   hr(),
                   
                   h4("Mathematical Definition"),
                   withMathJax("$$MaxLongstring = \\max\\{L_1, L_2, \\ldots, L_k\\}$$"),
                   
                   p(strong("Where:")),
                   p("• \\(L_i\\) = length of \\(i\\)-th consecutive sequence"),
                   p("• \\(k\\) = total number of consecutive sequences"),
                   p("• \\(\\max\\{\\cdot\\}\\) = maximum function"),
                   hr(),
                   
                   h4("Detailed Example"),
                   p(strong("Response vector (20 items):")),
                   p(code("[3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 3, 2, 4, 2, 3]")),
                   
                   p(strong("Sequence Identification:")),
                   p("• Positions 1-14: Fourteen 3's → \\(L_1 = 14\\)"),
                   p("• Position 15: One 2 → \\(L_2 = 1\\)"),
                   p("• Position 16: One 3 → \\(L_3 = 1\\)"),
                   p("• Position 17: One 2 → \\(L_4 = 1\\)"),
                   p("• Position 18: One 4 → \\(L_5 = 1\\)"),
                   p("• Position 19: One 2 → \\(L_6 = 1\\)"),
                   p("• Position 20: One 3 → \\(L_7 = 1\\)"),
                   
                   p(strong("Result:"), " \\(MaxLongstring = \\max\\{14, 1, 1, 1, 1, 1, 1\\} = 14\\)"),
                   hr(),
                   
                   h4("Interpretation"),
                   p(strong("Thresholds (for 20-item scale):")),
                   p("• \\(MaxLongstring \\leq 4\\): Normal variation"),
                   p("• \\(5 \\leq MaxLongstring \\leq 8\\): Borderline concerning"),
                   p("• \\(9 \\leq MaxLongstring \\leq 14\\): Likely inattentive"),
                   p("• \\(MaxLongstring \\geq 15\\): Strong evidence of carelessness"),
                   
                   p(strong("Common threshold:"), " \\(\\bar{x} + 1.5 \\times SD\\) of the distribution"),
                   p(strong("In our example:"), " \\(MaxLongstring = 14\\) strongly suggests inattentive responding.")
            ),
            
            column(width = 12,
                   p("")),
            
            # Average Longstring
            column(width = 12,
                   div(style="overflow-x: scroll"),
                   style = "border: 2px double black;",
                   h3("Average Longstring"),
                   p("Average Longstring complements Maximum Longstring by considering all consecutive sequences, not just the longest. It identifies respondents with consistently long sequences throughout the survey."),
                   hr(),
                   
                   h4("Mathematical Definition"),
                   withMathJax("$$AvgLongstring = \\frac{1}{m}\\sum_{i=1}^{m} L_i$$"),
                   
                   p(strong("Where:")),
                   p("• \\(L_i\\) = length of \\(i\\)-th consecutive sequence"),
                   p("• \\(m\\) = number of consecutive sequences"),
                   p("• Note: \\(m \\leq J\\) (total items)"),
                   hr(),
                   
                   h4("Detailed Example"),
                   p(strong("Using previous data with sequences:"), " \\(\\{14, 1, 1, 1, 1, 1, 1\\}\\)"),
                   
                   p(strong("Calculation:")),
                   p("• Sum of lengths: \\(\\sum L_i = 14 + 1 + 1 + 1 + 1 + 1 + 1 = 20\\)"),
                   p("• Number of sequences: \\(m = 7\\)"),
                   p("• \\(AvgLongstring = \\frac{20}{7} = 2.86\\)"),
                   hr(),
                   
                   h4("Interpretation"),
                   p(strong("Typical Thresholds:")),
                   p("• \\(AvgLongstring < 2.0\\): Good response variation"),
                   p("• \\(2.0 \\leq AvgLongstring < 3.0\\): Acceptable range"),
                   p("• \\(3.0 \\leq AvgLongstring < 4.0\\): Potential concern"),
                   p("• \\(AvgLongstring \\geq 4.0\\): Likely problematic"),
                   
                   p(strong("Comparison Example:")),
                   p("• Respondent A: One sequence of 10, rest singles → \\(Max = 10\\), \\(Avg = 1.8\\)"),
                   p("• Respondent B: Five sequences of 4 each → \\(Max = 4\\), \\(Avg = 4.0\\)"),
                   p("Respondent B shows more consistent patterning despite lower maximum.")
            ),
            
            column(width = 12,
                   p("")),
            
            # Mahalanobis Distance
            column(width = 12,
                   div(style="overflow-x: scroll"),
                   style = "border: 2px double black;",
                   h3("Mahalanobis Distance"),
                   p("Mahalanobis Distance measures how far a respondent's multivariate response pattern deviates from the typical pattern, accounting for correlations between items. Unlike Euclidean distance, it considers the covariance structure of the data."),
                   hr(),
                   
                   h4("Mathematical Foundation"),
                   withMathJax("$$D^2 = (\\mathbf{x} - \\boldsymbol{\\mu})^T \\boldsymbol{\\Sigma}^{-1} (\\mathbf{x} - \\boldsymbol{\\mu})$$"),
                   
                   p(strong("Where:")),
                   p("• \\(\\mathbf{x}\\) = respondent's response vector"),
                   p("• \\(\\boldsymbol{\\mu}\\) = group mean vector"),
                   p("• \\(\\boldsymbol{\\Sigma}\\) = covariance matrix"),
                   p("• \\(\\boldsymbol{\\Sigma}^{-1}\\) = inverse covariance matrix"),
                   p("• \\(D^2\\) follows \\(\\chi^2\\) distribution with \\(df = p\\) (number of items)"),
                   hr(),
                   
                   h4("Detailed Example"),
                   p(strong("3-item job satisfaction survey (1-5 scale):")),
                   p("• Item 1: 'I enjoy my work'"),
                   p("• Item 2: 'My workload is manageable'"),
                   p("• Item 3: 'I plan to stay with this company'"),
                   
                   p(strong("Group Statistics:")),
                   withMathJax("$$\\boldsymbol{\\mu} = \\begin{bmatrix} 4.5 \\\\ 3.5 \\\\ 2.0 \\end{bmatrix}, \\quad \\boldsymbol{\\Sigma} = \\begin{bmatrix} 1.0 & 0.2 & 0.1 \\\\ 0.2 & 1.0 & 0.3 \\\\ 0.1 & 0.3 & 1.0 \\end{bmatrix}$$"),
                   
                   p(strong("Individual Response:")),
                   withMathJax("$$\\mathbf{x} = \\begin{bmatrix} 5 \\\\ 3 \\\\ 1 \\end{bmatrix}$$"),
                   
                   p(strong("Calculation:")),
                   p("• Deviation: \\(\\mathbf{x} - \\boldsymbol{\\mu} = \\begin{bmatrix} 0.5 \\\\ -0.5 \\\\ -1.0 \\end{bmatrix}\\)"),
                   p("• \\(D^2 = 1.45\\) (after matrix multiplication)"),
                   hr(),
                   
                   h4("Interpretation"),
                   p(strong("Critical Values"), " (\\(\\chi^2\\) distribution):"),
                   p(strong("For \\(df = 3\\)"), " (3 items in our example):"),
                   p("• \\(\\chi^2_{0.05}(3) = 7.815\\)"),
                   p("• \\(\\chi^2_{0.01}(3) = 11.345\\)"),
                   p("• \\(\\chi^2_{0.001}(3) = 16.266\\)"),
                   
                   p(strong("The degrees of freedom equals the number of items. Other common critical values:")),
                   p("• \\(df = 5\\): \\(\\chi^2_{0.05} = 11.070\\), \\(\\chi^2_{0.01} = 15.086\\)"),
                   p("• \\(df = 10\\): \\(\\chi^2_{0.05} = 18.307\\), \\(\\chi^2_{0.01} = 23.209\\)"),
                   p("• \\(df = 20\\): \\(\\chi^2_{0.05} = 31.410\\), \\(\\chi^2_{0.01} = 37.566\\)"),
                   
                   p(strong("In our example:"), " \\(D^2 = 1.45 < 7.815\\), so the response pattern is not statistically unusual at \\(\\alpha = 0.05\\).")
            ),
            
            column(width = 12,
                   p("")),
            
            # Person Fit
            column(width = 12,
                   div(style="overflow-x: scroll"),
                   style = "border: 2px double black;",
                   h3("\\(L_z\\) Statistic: Person-Fit Index"),
                   p("The \\(L_z\\) statistic (Drasgow, Levine, & Williams, 1985) is a standardized log-likelihood statistic that measures whether a respondent's pattern is consistent with their overall ability or trait level. It compares the observed response pattern against what would be expected given the respondent's estimated ability, detecting unusual patterns such as correctly answering difficult items while missing easy ones."),
                   hr(),
                   
                   h4("Mathematical Foundation"),
                   p(strong("For dichotomous (binary) items where \\(u_i \\in \\{0, 1\\}\\), the log-likelihood is:")),
                   withMathJax("$$\\log L(\\theta) = \\sum_{i=1}^n \\left[ u_i \\log P_i(\\theta) + (1 - u_i) \\log(1 - P_i(\\theta)) \\right]$$"),
                   
                   p(strong("The standardized \\(L_z\\) statistic is:")),
                   withMathJax("$$L_z = \\frac{\\log L(\\theta) - \\mathbb{E}[\\log L(\\theta)]}{\\sqrt{\\mathrm{Var}[\\log L(\\theta)]}}$$"),
                   
                   p(strong("Where:")),
                   p("• \\(u_i\\) = observed response to item \\(i\\) (0 = incorrect/disagree, 1 = correct/agree)"),
                   p("• \\(P_i(\\theta)\\) = probability of correct/positive response to item \\(i\\) at ability \\(\\theta\\)"),
                   p("• \\(\\mathbb{E}[\\log L(\\theta)]\\) = expected log-likelihood under the model"),
                   p("• \\(\\mathrm{Var}[\\log L(\\theta)]\\) = variance of log-likelihood"),
                   p("• \\(L_z \\sim N(0,1)\\) asymptotically when true \\(\\theta\\) is known"),
                   
                   p(strong("Note:"), " When estimated \\(\\hat{\\theta}\\) is used instead of true \\(\\theta\\), the distribution deviates from standard normal. Snijders (2001) developed \\(L_z^*\\) to correct for this."),
                   hr(),
                   
                   h4("Detailed Calculation Example"),
                   p(strong("Consider a 5-item test with dichotomous responses:")),
                   p("• Items: Ordered from easy to difficult"),
                   p("• Response pattern: \\(\\mathbf{u} = (1, 0, 1, 1, 0)\\)"),
                   p("• Estimated ability: \\(\\hat{\\theta} = 0.5\\)"),
                   
                   p(strong("Item probabilities at \\(\\hat{\\theta} = 0.5\\):")),
                   p("• \\(P_1(0.5) = 0.9\\) (easy item)"),
                   p("• \\(P_2(0.5) = 0.7\\)"),
                   p("• \\(P_3(0.5) = 0.5\\)"),
                   p("• \\(P_4(0.5) = 0.3\\)"),
                   p("• \\(P_5(0.5) = 0.1\\) (difficult item)"),
                   
                   p(strong("Log-likelihood calculation:")),
                   p("• Item 1: \\(u_1 = 1\\), contributes \\(\\log(0.9) = -0.105\\)"),
                   p("• Item 2: \\(u_2 = 0\\), contributes \\(\\log(1-0.7) = \\log(0.3) = -1.204\\)"),
                   p("• Item 3: \\(u_3 = 1\\), contributes \\(\\log(0.5) = -0.693\\)"),
                   p("• Item 4: \\(u_4 = 1\\), contributes \\(\\log(0.3) = -1.204\\)"),
                   p("• Item 5: \\(u_5 = 0\\), contributes \\(\\log(0.9) = -0.105\\)"),
                   
                   p("\\(\\log L(0.5) = -0.105 - 1.204 - 0.693 - 1.204 - 0.105 = -3.311\\)"),
                   
                   p("If \\(\\mathbb{E}[\\log L(0.5)] = -2.5\\) and \\(\\sqrt{\\mathrm{Var}[\\log L(0.5)]} = 0.8\\):"),
                   p("\\(L_z = \\frac{-3.311 - (-2.5)}{0.8} = \\frac{-0.811}{0.8} = -1.014\\)"),
                   
                   p("This negative value suggests the response pattern is somewhat unexpected given the ability level."),
                   hr(),
                   
                   h4("Interpretation"),
                   p(strong("Statistical Thresholds:")),
                   p("• \\(L_z > -1.0\\): Normal response pattern"),
                   p("• \\(-2.0 < L_z \\leq -1.0\\): Mildly unusual pattern"),
                   p("• \\(-3.0 < L_z \\leq -2.0\\): Moderately unusual pattern (flag for review)"),
                   p("• \\(L_z \\leq -3.0\\): Highly unusual pattern (likely invalid)"),
                   
                   p(strong("Significance-based cutoffs:")),
                   p("• \\(\\alpha = 0.05\\): Flag if \\(L_z < -1.645\\)"),
                   p("• \\(\\alpha = 0.01\\): Flag if \\(L_z < -2.326\\)"),
                   p("• \\(\\alpha = 0.001\\): Flag if \\(L_z < -3.090\\)"),
                   
                   p(strong("Common patterns detected:")),
                   p("• ", strong("Lucky guessing:"), " Correct on hard items, incorrect on easy items"),
                   p("• ", strong("Careless errors:"), " Incorrect on easy items despite high ability"),
                   p("• ", strong("Cheating:"), " Unusually high performance on specific item clusters"),
                   p("• ", strong("Random responding:"), " No relationship between item difficulty and responses"),
                   
                   p(strong("Note:"), " While \\(L_z\\) was developed for Item Response Theory models, similar likelihood-based approaches can be adapted for other psychometric models. The key requirement is the ability to compute \\(P_i(\\theta)\\) - the probability of each response given the respondent's trait level.")
            )
          )
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

