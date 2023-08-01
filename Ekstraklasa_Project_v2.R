library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)

#Loading data, i had some issues with csv file but "sep" helped me :)
df_Ekstraklasa <- read.csv('~/personal_storage/Project_JAKDU/Ekstraklasa_clean_df.csv', sep=",")

# Data set: Ekstraklasa's tournament ratings of logistic aspects from my bachelor degree diploma
# This data set was created to find aspects to improve in Ekstraklasa's tournament logistics support system.
# I asked people to rate those aspects, and provide more detailed information about their metrics.

# Ctrl + shift + c = very useful for creating multiline comments :)
# Columns Descriptions

# function that shows all columns in df, remember to update it using latest dataset!!!
colnames(df_Ekstraklasa_2)
#[1] "ID" Primary key that identifies respondent                                                                                            
#[2] "Attended_match_on_stadium" Checks if person attended matches this year, helps to filter out responses.                                                                     
#[3] "Role" - Role that attendant pended on match                                                                                          
#[4] "City_with_stadium" - Checks if there is stadium in home city                                                                        
#[5] "Distance_from_stadium" - Distance from home to stadium in km                                                                       
#[6] "Vehicle" -  Most used vehicle to go on match, Change = people that has to change vehicle                                                                                      
#[7] "Matches_attended" - Amount of matches attended in 2022/2023 season                                                                              
#[8] "Catering_services_quality"  - 1 to 5 rating of catering quality on stadiums                                                                    
#[9] "Medical_Quality"            - 1 to 5 rating of medical quality on stadium (f.e amount of 1st aid kits)                                                                    
#[10] "Buying_tickets_process"    - 1 to 5 rating of buying tickets process                                                                     
#[11] "Evacuation_process"        - 1 to 5 rating of evacuation process on stadiums                                                                     
#[12] "Info_behaviour_in_danger"  - 1 to 5 rating of availability to information how to behave in danger                                                                    
#[13] "Hygiene_Quality"           - 1 to 5 rating of hygiene quality on stadiums                                                                    
#[14] "Arrival_to_stadium_matchday"  - 1 to 5 rating of getting to the stadium on matchday                                                                  
#[15] "Parking_spaces_availability"  - 1 to 5 rating of parking spaces availability                                                                  
#[16] "Facilities_for_disabled"      - 1 to 5 rating of Facilities for disabled                                                                  
#[17] "Sense_of_security"            - 1 to 5 rating of attendees sense of security                                                                  
#[18] "Police_work_Quality"          - 1 to 5 rating of police work quality
#[19] "Medical_people_amount"        - 1 to 5 rating of amount of medical people (f.e amount of medical personas for 1 attendee)
#[20] "Game_schedule_Quality"        - 1 to 5 rating of matches schedule (Date, hour etc.)                                                                 
#[21] "Entering_and_leaving_process" - 1 to 5 rating of quality of entering and leaving stadium process                                                                 
#[22] "Stadium_staff_work_Quality"   - 1 to 5 rating of stadium staf work quality                                                                
#[23] "Things_to_improve"            - Ideas about what should be improved                                                                   
#[24] "Rating_Ekstraklasa"           - 1 to 5 rating of Ekstraklasa enterprise                                                                 
#[25] "Age"                          - Age of respondent                                                                  
#[26] "Sex"                          - Sex of respondent                                                                  
#[27] "Education"                    - Education of respondent                                                                  
#[28] "Professional_status"          - Professional status of respondent                                                                  
#[29] "Opinion_Ekstraklasa"          - Its related to Rating_Ekstraklasa column results 1,2 = Bad, 3 = Medium, 4,5 = Good opinion about Ekstraklasa

#Removing useless column
df_Ekstraklasa_2 <- df_Ekstraklasa %>% select(-c(X))

df_Ekstraklasa_2[df_Ekstraklasa_2=="Sportowiec (Członek jednej z drużyn / Sędzia)"] <- "Sportsman"
df_Ekstraklasa_2[df_Ekstraklasa_2=="Men"] <- "Man"
df_Ekstraklasa_2[df_Ekstraklasa_2=="Women"] <- "Woman"

#i have created df with quality ratings and metric info
Quality_Ekstraklasa_with_metric_df <- subset(df_Ekstraklasa_2, select = c("ID", "Catering_services_quality","Medical_Quality","Buying_tickets_process", "Evacuation_process","Info_behaviour_in_danger","Hygiene_Quality","Arrival_to_stadium_matchday","Parking_spaces_availability","Facilities_for_disabled","Sense_of_security", "Police_work_Quality","Medical_people_amount",
                                                                          "Game_schedule_Quality",                                                                          
                                                                          "Entering_and_leaving_process",                                                                   
                                                                          "Stadium_staff_work_Quality", "Rating_Ekstraklasa", "Opinion_Ekstraklasa", "Role", "Age", "Sex", "Professional_status", "Education", "Vehicle", "Distance_from_stadium"))

Quality_Ekstraklasa_with_Sex_df <- subset(df_Ekstraklasa_2, select = c("ID", "Catering_services_quality","Medical_Quality","Buying_tickets_process", "Evacuation_process","Info_behaviour_in_danger","Hygiene_Quality","Arrival_to_stadium_matchday","Parking_spaces_availability","Facilities_for_disabled","Sense_of_security", "Police_work_Quality","Medical_people_amount",
                                                                          "Game_schedule_Quality",                                                                          
                                                                          "Entering_and_leaving_process",                                                                   
                                                                          "Stadium_staff_work_Quality", "Rating_Ekstraklasa", "Sex"))


#I have created new data frame to create charts comparing results based on sex
Quality_Ekstraklasa_Woman <- Quality_Ekstraklasa_with_Sex_df %>% 
  filter(!str_detect(Sex, 'Man'))

Quality_Ekstraklasa_Man <- Quality_Ekstraklasa_with_Sex_df %>% 
  filter(!str_detect(Sex, 'Woman'))

Quality_Ekstraklasa_with_metric_df$Means <-apply(Quality_Ekstraklasa_with_metric_df,1,mean)
Quality_Ekstraklasa_with_metric_df

Quality_Ekstraklasa_means_df <- (colMeans(Quality_Ekstraklasa_df))

# Bar graph, time on x-axis, color fill grouped by sex -- use position_dodge()


  
 #Stacked chart which shows roles + Sex of attendees                          
ggplot(data=Quality_Ekstraklasa_with_metric_df, aes(x=Sex)) +
  geom_bar(aes(fill = Role), colour="black") 

ggplot(data = Quality_Ekstraklasa_with_metric_df, aes(x = Sex)) +
  geom_bar(aes(fill = Role), colour = "black") +
  geom_text(
    aes(label = stat(count), group = Role),
    stat = "count",
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
    )


#stacked chart that shows age + Sex of attendees
ggplot(data = Quality_Ekstraklasa_with_metric_df, aes(x = Age)) + 
  geom_bar(aes(fill = Sex), colour="black") +
  geom_text(data = . %>% group_by(Sex, Age) %>% 
              summarize(n = n()) %>% group_by(Age) %>%
              summarize(Sex = Sex, n = n, perc = n / sum(n)),
            aes(y = n, label = paste(scales::comma(n), 
                                     
                                     sep = '\n'), group = Sex), 
            position = position_stack(vjust = 0.5)) +
  geom_text(stat = "count", aes(label = scales::comma(after_stat(count))),
            nudge_y = 0.9, fontface = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        axis.text.y.left = element_blank(),
        axis.title.y.left = element_blank()) 

# Reorder the levels of Age
Quality_Ekstraklasa_with_metric_df$Age <- factor(Quality_Ekstraklasa_with_metric_df$Age, levels = c("Under 18", "18-25", "26-35", "36-45"))

# Plot
ggplot(data = Quality_Ekstraklasa_with_metric_df, aes(x = Age)) +
  geom_bar(aes(fill = Sex), colour = "black") +
  geom_text(data = . %>% group_by(Sex, Age) %>% 
              summarize(n = n()) %>% group_by(Age) %>%
              summarize(Sex = Sex, n = n, perc = n / sum(n)),
            aes(y = n, label = paste(scales::comma(n), sep = '\n'), group = Sex), 
            position = position_stack(vjust = 0.8)) +
  geom_text(stat = "count", aes(label = scales::comma(after_stat(count))),
            nudge_y = 2, fontface = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0),
    axis.text.y.left = element_blank(),
    axis.title.y.left = element_blank()
  )


 # Reorder the levels of Distance_from_stadium
 Quality_Ekstraklasa_with_metric_df$Distance_from_stadium <- factor(Quality_Ekstraklasa_with_metric_df$Distance_from_stadium,
                                                                    levels = c("0-10", "10-50", "51-100", "100+"),
                                                                    ordered = TRUE)
 
 # Calculate counts
 counts <- Quality_Ekstraklasa_with_metric_df %>%
   group_by(Vehicle, Distance_from_stadium) %>%
   summarize(n = n())
 
 # Plot
 ggplot(data = counts, aes(x = Distance_from_stadium, y = n, fill = Vehicle)) +
   geom_bar(stat = "identity", position = "dodge", colour = "black") +
   geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black") +
   theme_minimal() +
   theme(
     axis.text.x = element_text(angle = 90, hjust = 0),
     axis.text.y.left = element_blank(),
     axis.title.y.left = element_blank(),
     plot.margin = margin(20, 30, 20, 20, "pt")
   ) +
   coord_cartesian(clip = "off")
 
 
 
 
 Rating_Ekstraklasa_means <- df_Ekstraklasa_2 %>%
   summarize(mean = mean(Rating_Ekstraklasa))
 
 #I have created rounded mean for Ekstraklasa rating
 
 Rounded_Rating_Ekstraklasa_means <-  round(Rating_Ekstraklasa_means, 2)
 #INT: From what can we see there Ekstraklasa has rather medium opinion along football fans in Poland as its mean is closer to 3 than to 4.
 
 
 #Histogram plots of Ekstraklasa Rating
 
 ggplot(df_Ekstraklasa_2, aes(x=Rating_Ekstraklasa)) +
   labs(y= "Amount of votes", x = "Rating of Ekstraklasa SA from 1 to 5") +
   geom_histogram(binwidth=1, colour="blue", fill="white") +
   geom_vline(data = Rounded_Rating_Ekstraklasa_means, aes(xintercept=mean), 
              color="red", linetype="longdash", size=1) +
   geom_text(data = (df_Ekstraklasa_2 %>% 
                       group_by(Rating_Ekstraklasa) %>%
                       summarize(count = n())), 
             aes(x = Rating_Ekstraklasa, y = count, label = count),
             vjust = -0.5, color = "black", size = 3) +
   geom_text(data = Rounded_Rating_Ekstraklasa_means, aes(x = mean, y = 0, label = round(mean, 2)),
             vjust = -1, hjust = -5, color = "red", size = 5, angle = 90)  # Adding mean value as text
 
 
 
 #In this code, the geom_text() layer uses the df_Ekstraklasa_2 data frame to calculate the count of votes for each rating level. The group_by() and summarize() functions are used to group the data by Rating_Ekstraklasa and calculate the count using the n() function. Then, geom_text() adds the count labels to the chart, using x = Rating_Ekstraklasa for the x-coordinate, y = count for the y-coordinate, and label = count to display the count value.
 
# Adjust the vjust, color, and size parameters according to your preference for the position, color, and size of the count labels.
 

 Summarized_info_Quality_Ekstraklasa_with_metric_df <-Quality_Ekstraklasa_with_metric_df %>%
   group_by(Age, Sex, Professional_status) %>%
   summarise(
     Rating_Ekstraklasa = mean(Rating_Ekstraklasa),
     Catering = mean(Catering_services_quality),
     Medical = mean(Medical_Quality),
     Buying_Tickets = mean(Buying_tickets_process),
     Evacuation_process = mean(Evacuation_process),
     Info_behaviour_in_danger = mean(Info_behaviour_in_danger),
     Hygiene = mean(Hygiene_Quality),
     Arrival_to_stadium_matchday = mean(Arrival_to_stadium_matchday),
     Parking_spaces_availability = mean(Parking_spaces_availability),
     Facilities_for_disabled = mean(Facilities_for_disabled),
     Sense_of_security = mean(Sense_of_security),
     Police_work_Quality = mean(Police_work_Quality),
     Medical_people_amount = mean(Medical_people_amount),
     Game_schedule_Quality = mean(Game_schedule_Quality),
     Entering_and_leaving_process = mean(Entering_and_leaving_process),
     Stadium_staff_work_Quality = mean(Stadium_staff_work_Quality),
   ) 
 
 Quality_Ekstraklasa_df <- subset(df_Ekstraklasa_2, select = c("Catering_services_quality","Medical_Quality","Buying_tickets_process", "Evacuation_process","Info_behaviour_in_danger","Hygiene_Quality","Arrival_to_stadium_matchday","Parking_spaces_availability","Facilities_for_disabled","Sense_of_security", "Police_work_Quality","Medical_people_amount",
                                                               "Game_schedule_Quality",                                                                          
                                                               "Entering_and_leaving_process",                                                                   
                                                               "Stadium_staff_work_Quality", "Rating_Ekstraklasa"))
 
 
 Quality_Ekstraklasa_without_metric_df <- subset(df_Ekstraklasa_2, select = c("Catering_services_quality","Medical_Quality","Buying_tickets_process", "Evacuation_process","Info_behaviour_in_danger","Hygiene_Quality","Arrival_to_stadium_matchday","Parking_spaces_availability","Facilities_for_disabled","Sense_of_security", "Police_work_Quality","Medical_people_amount",
                                                                           "Game_schedule_Quality",                                                                          
                                                                           "Entering_and_leaving_process",                                                                   
                                                                           "Stadium_staff_work_Quality"))

 
#
# Oblicz średnie wartości dla każdej kolumny i zaokrąglij do 2 miejsc po przecinku
average_scores_Quality_Ekstraklasa <- round(sapply(Quality_Ekstraklasa_without_metric_df[, ], mean), 2)

# Tworzenie ramki danych dla wykresu
data_Quality_Ekstraklasa_plot <- data.frame(aspect = names(average_scores_Quality_Ekstraklasa), average_score = average_scores_Quality_Ekstraklasa)

# Sortowanie malejąco według średniej wartości
data_Quality_Ekstraklasa_plot <- data_Quality_Ekstraklasa_plot[order(-data_Quality_Ekstraklasa_plot$average_score), ]


# Wykres słupkowy poziomy z różnymi kolorami
ggplot(data_Quality_Ekstraklasa_plot, aes(x = reorder(aspect, -average_score), y = average_score, fill = aspect)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = average_score), vjust = 0.5, hjust = 1.5, color = "black", size = 3, fontface = "bold") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)), # Add margin to the top of the x-axis label
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10))) +
  labs(y = "Average rating", x = "Logistics aspect") +
  guides(fill = "none") +
  coord_flip() +
  scale_x_discrete(expand = c(0.2, 0)) # Zwiększenie szerokości obszar

Quality_Ekstraklasa_Man_without_Sex_df <- subset(Quality_Ekstraklasa_Man, select = c( "Catering_services_quality","Medical_Quality","Buying_tickets_process", "Evacuation_process","Info_behaviour_in_danger","Hygiene_Quality","Arrival_to_stadium_matchday","Parking_spaces_availability","Facilities_for_disabled","Sense_of_security", "Police_work_Quality","Medical_people_amount",
                                                                       "Game_schedule_Quality",                                                                          
                                                                       "Entering_and_leaving_process",                                                                   
                                                                       "Stadium_staff_work_Quality"))

#Now creating charts to compare Man vs Woman ratings

#Man chart with ratings 


# Oblicz średnie wartości dla każdej kolumny i zaokrąglij do 2 miejsc po przecinku
average_scores_Quality_Ekstraklasa_Man <- round(sapply(Quality_Ekstraklasa_Man_without_Sex_df[, ], mean), 2)

# Tworzenie ramki danych dla wykresu
data_Quality_Ekstraklasa_Man_plot <- data.frame(aspect = names(average_scores_Quality_Ekstraklasa_Man), average_score = average_scores_Quality_Ekstraklasa_Man)

# Sortowanie malejąco według średniej wartości
data_Quality_Ekstraklasa_Man_plot <- data_Quality_Ekstraklasa_Man_plot[order(-data_Quality_Ekstraklasa_Man_plot$average_score), ]


# Wykres słupkowy poziomy z różnymi kolorami
ggplot(data_Quality_Ekstraklasa_Man_plot, aes(x = reorder(aspect, -average_score), y = average_score, fill = aspect)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = average_score), vjust = 0.5, hjust = 1.5, color = "black", size = 3, fontface = "bold") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)), # Add margin to the top of the x-axis label
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10))) +
  labs(y = "Average rating given by Men", x = "Logistics aspect") +
  guides(fill = "none") +
  coord_flip() +
  scale_x_discrete(expand = c(0.2, 0)) # Zwiększenie szerokości obszar


Quality_Ekstraklasa_Woman_without_Sex_df <- subset(Quality_Ekstraklasa_Woman, select = c( "Catering_services_quality","Medical_Quality","Buying_tickets_process", "Evacuation_process","Info_behaviour_in_danger","Hygiene_Quality","Arrival_to_stadium_matchday","Parking_spaces_availability","Facilities_for_disabled","Sense_of_security", "Police_work_Quality","Medical_people_amount",
                                                                                      "Game_schedule_Quality",                                                                          
                                                                                      "Entering_and_leaving_process",                                                                   
                                                                                      "Stadium_staff_work_Quality"))

#Now creating chart Woman ratings

# Oblicz średnie wartości dla każdej kolumny i zaokrąglij do 2 miejsc po przecinku
average_scores_Quality_Ekstraklasa_Woman <- round(sapply(Quality_Ekstraklasa_Woman_without_Sex_df[, ], mean), 2)

# Tworzenie ramki danych dla wykresu
data_Quality_Ekstraklasa_Woman_plot <- data.frame(aspect = names(average_scores_Quality_Ekstraklasa_Woman), average_score = average_scores_Quality_Ekstraklasa_Woman)

# Sortowanie malejąco według średniej wartości
data_Quality_Ekstraklasa_Woman_plot <- data_Quality_Ekstraklasa_Woman_plot[order(-data_Quality_Ekstraklasa_Woman_plot$average_score), ]


# Wykres słupkowy poziomy z różnymi kolorami
ggplot(data_Quality_Ekstraklasa_Woman_plot, aes(x = reorder(aspect, -average_score), y = average_score, fill = aspect)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = average_score), vjust = 0.5, hjust = 1.5, color = "black", size = 3, fontface = "bold") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)), # Add margin to the top of the x-axis label
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10))) +
  labs(y = "Average rating given by Women", x = "Logistics aspect") +
  guides(fill = "none") +
  coord_flip() +
  scale_x_discrete(expand = c(0.2, 0)) # Zwiększenie szerokości obszar

