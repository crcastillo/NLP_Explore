# NLP_Explore

### Project Objective
The goal of this project is to explore some of the R packages associated with Natural Language Processing and build some topic models and word clouds utilizing the [State of the Union addresses](https://www.presidency.ucsb.edu/documents/presidential-documents-archive-guidebook/annual-messages-congress-the-state-the-union). We will attempt to determine how the sentiment of SOTU addresses changes over time, whether there is a meaningful difference in sentiment for [Republican vs Democratic parties](https://www.presidentsusa.net/partyofpresidents.html), and if topic modeling can classify recent addresses by political party.

### Required Libraries
* XML
* rvest
* lubridate
* ggplot2
* dplyr
* tidyverse
* tidytext
* tidyr
* glue
* stringr
* magrittr
* stringdist
* topicmodels
* tm
* wordcloud

## Interesting Findings
Net Sentiment (positive words - negative words) doesn't appear to have significant differences between Democrats and Republicans. This may not be a good metric since this doesn't control for differences in counts of words associated with a positive/negative sentiment.

| Party       | Metric                | Value  |
| ----------- | --------------------- | ------ |
| Democrat    | Average Net Sentiment | 156    |
| Republican  | Average Net Sentiment | 173    |
| Democrat    | Observations          | 89     |
| Republican  | Observations          | 88     |
| N/A         | T-Stat                | -0.99  |
| N/A         | P-Value               | 0.3218 |

![Net Sentiment by Party](https://github.com/crcastillo/NLP_Explore/blob/master/Images/Net Sentiment by Party.png)

Percent Sentiment (positive words / (positive words + negative words) ) indicates that Republicans use a significantly higher proportion of positive words (compared to negative words) compared to Democrats. The edge is realtively slight.

| Party       | Metric                | Value  |
| ----------- | --------------------- | ------ |
| Democrat    | Average % Sentiment   | 0.6394 |
| Republican  | Average % Sentiment   | 0.6581 |
| Democrat    | Observations          | 89     |
| Republican  | Observations          | 88     |
| N/A         | T-Stat                | -0.99  |
| N/A         | P-Value               | 0.3218 |

![Percent Sentiment by Party](https://github.com/crcastillo/NLP_Explore/blob/master/Images/Percent Sentiment by Party.png)

The 2-topic model trained since 1981 shows the following classification results:

| Party       | Topic | Topic_Count | Party_Count | Topic_Percent |
| ----------- | ----- | ----------- | ----------- | ------------- |
| Democrat    | 1     | 19          | 16          | 0.842         |
| Republican  | 1     | 19          | 3           | 0.158         |
| Republican  | 2     | 20          | 20          | 1             |

Topic 1 categorizes Democrats pretty well, while Topic 2 categorizes Republican addresses perfectly for an overall accuracy of 92.3% ((16 + 20) / 39).

The following wordclouds were generated for Democrats and Republicans and using the top 50 terms.

![Democrat Wordcloud](https://github.com/crcastillo/NLP_Explore/blob/master/Images/Democrat Wordcloud.png)

![Republican Wordcloud](https://github.com/crcastillo/NLP_Explore/blob/master/Images/Republican Wordcloud.png)