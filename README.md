# Ekstraklasa logistics aspects rated by attendees
Author Jakub Durawa

```{r include=FALSE}
require(dplyr)
require(tidyverse)
require(ggplot2)
require(scales)
```


## **The Research Question**

### Purpose of this research was finding aspects that should be optimized in whole process of organizing football matches in Ekstraklasa. Highest football league in Poland.
### This dataframe was collected by myself using Microsoft Forms for purpose of my Bachelor degree diploma. Out of 118 people who answered my survey 70 of them were on Ekstraklasa match in 2022/2023 season.

## **Data Preparation**

#### As it is really small dataframe, i couldn't go for machine learning in this case, so i decided to put some extra effort to learn new things. I decided to go through whole data cleaning process in R. (Despite it would be much easier for me to do it using Excel and PowerQuery.) Definietly it took me a lot of time as in the end it took over 250 lines of code to make this dataframe workable. 
```{r echo=FALSE}
