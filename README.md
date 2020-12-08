# CarelessRespondersApp

The purpose of this application is to allow researchers to upload their data, view potential careless respondents in the data identified by statistical methods, and independently decide which types of careless respondents should be removed. The application can be viewed at the link https://careless-statistics.shinyapps.io/carelessstatistics/.


UPLOADING DATA:

Under “Data Set of Responses,” a CSV file of likert-type data can be uploaded. Once a file with the raw data is uploaded, a head of the first ten response vectors will appear in this tab.

Check “Header” if the data has a header (column names).

Select the type of separation in your data file (i.e. Are values comma - separated?)

Select the quotes used to quote strings.

Check “User IDs” if the data includes an identification for each respondent at the beginning of each response vector.

Check “Item-Level Descriptive Statistics” to obtain summaries for each item. If selected, a table with the mean and standard deviation for each item can be downloaded in the “Downloading Data” tab. In addition, bar plots displaying the distribution of responses for select items can be found under the “Descriptive Statistics” tab.

Indicate the number of levels in the likert scale. For example, if respondents could choose between five options, input 5. If there were three options (such as “Agree”, “Neutral”, and “Disagree”), input 3.

Indicate the lowest value in the likert scale. If the data includes five levels and ranges from zero to four, input 0, or if your data ranges from one to five, input 1.

If the data includes a validity item, input the number of the column corresponding to the item. If no validity item was administered, leave blank or enter “NA.” See “Method Explanations” tab for examples of a validity item.

Input the correct answer to the validity item.

Indicate the alpha that will be used to detect outlying responses in the person-fit method. With a higher alpha value, more respondents will be marked as potentially careless. By decreasing the alpha value, this method will flag only the most aberrant responses. See “Method Explanations” tab for more information on person-fit statistics.

Indicate the alpha that will be used to detect outlying responses in the Mahalanobis distance method. With a higher alpha value, more respondents will be marked as potentially careless. By decreasing the alpha value, this method will flag only the most aberrant responses.  See “Method Explanations” tab for more information on Mahalanobis distance.


DATA OUTPUT:

The pie chart displays the relative number of responses flagged by each method. Note: respondents flagged by more than one method are grouped into one category, and each of the other categories (i.e. “IRV” and “Longstring”) represent response vectors flagged by only that method.

“Rates at which each method flags respondents” displays the aggregate number of respondents flagged by each method. Note: This chart does not take into account respondents that may be flagged by two or more methods. Thus, a respondent who is counted under “IRV” may also be counted under “Longest String”. 

“Distribution of respondents based on how many times they were flagged” displays the number of respondents who were flagged. For example, if 45 participants were deemed careless by two methods, the bar for “2” will have a length of 45.


EXPLANATIONS OF METHODS:

This tab explains the methods used to detect potential careless respondents.


DESCRIPTIVE STATISTICS:

If “Item-Level Descriptive Statistics” is selected in the “Uploading Data” tab, two graphs will appear here. 

The first graph displays the distribution of responses for the ten items with the highest number of “positive” responses relative to the median value on the scale. For example, a scale ranging from 1-5 will have a median of 3, and the items with the highest number of respondents choosing “4” and/or “5” will be displayed here. 

The second graph displays the distribution of responses for the ten items with the lowest number of “positive” responses relative to the median value on the scale. For example, a scale ranging from 1-5 will have a median of 3, and the items with the lowest number of respondents choosing “4” and/or “5” will be displayed here. 


PREREGISTRATION:

A chart describing the different types of carelessness detected by each method is displayed here. An “X” indicates that the method will aid in flagging participants who demonstrate the type of carelessness in the column name. 

To add this table to a preregistration plan, the user can follow the link to a Google Document to copy the table. 


DOWNLOADING DATA:

Under “Choose a table to download,” users have the option to download three different tables.

The “Raw Careless Statistics” table displays the raw statistics resulting from each function for each person (ex. the chi-squared value for Mahalanobis distance or the length of the longest longstring). This file is helpful if the user would prefer to make decisions about which respondents should be “flagged,” instead of relying on the application.

The “Binary Flagged Table” replaces the raw careless statistics with a binary code - “1” if the value indicates the respondent may be careless or “0” if the value does not indicate careless responding under that method. This table is useful if the user would like to rely on the standards used in the application in order to determine which responders are deemed careless.

The “Item-Level Descriptive Statistics” offers the mean and standard deviation for each item (Note: This table can only be downloaded if “Item-Level Descriptive Statistics” is selected in the “Uploading Data” tab).  

Under “Flagged Respondents are highlighted in red,” an interactive table will display the raw statistics from each method for each respondent, highlighting cells in red when the respondent is found to be careless under that method. 
