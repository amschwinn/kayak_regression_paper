library("xlsx")
library("car")
library("bstats")

#Load in Excel Data
kayak_dataset<-read.xlsx("C:/Users/Austin/Documents/Fall 2013 Classes/Applied Stats/kayak data set.xlsx",1)

#Take Columns in dataset and Make into Variables
gender		<-kayak_dataset[,3]
number_in_boat	<-kayak_dataset[,4]
heat_number		<-kayak_dataset[,5]
semi_final		<-kayak_dataset[,6]
event_meters	<-kayak_dataset[,7]
heat_place		<-kayak_dataset[,8]
semi_final_place	<-kayak_dataset[,9]
final_place		<-kayak_dataset[,10]

summary(gender)
summary(number_in_boat)
summary(heat_number)
summary(semi_final)
summary(event_meters)
summary(heat_place)
summary(semi_final_place)
summary(final_place)

sd(gender)
sd(number_in_boat)
sd(heat_number)
sd(semi_final)
sd(event_meters)
sd(heat_place)
sd(semi_final_place)
sd(final_place)





#Run Regression 1
lm(final_place ~ semi_final_place + heat_place + semi_final + heat_number)

#Run Summary 1
place_results	<-lm(final_place ~ semi_final_place + heat_place + semi_final + heat_number)
place_summary	<-summary(place_results)
place_summary


#Calculate F 
fstat_place		<-place_summary$fstatistic
fcrit_place		<-qf(.95,4,67)


#Hypothesis Test
hypoth_rej=0; if(fstat_place>fcrit_place){hypoth_rej=1}
hypoth_rej

#Statistically Signifcant

######################################################################################################

#Run Regression 2
lm(final_place ~ semi_final_place + heat_place + gender)

#Run Summary 2
gender_results	<-lm(final_place ~ semi_final_place + heat_place + gender)
gender_summary	<-summary(gender_results)
gender_summary

#Calculate F 
fstat_gender	<-gender_summary$fstatistic
fcrit_gender	<-qf(.95,3,68)


#Hypothesis Test
hypoth_rej=0; if(fstat_gender>fcrit_gender){hypoth_rej=1}
hypoth_rej

#Statistically Signifcant


######################################################################################################

#Run Regression 3
lm(final_place ~ semi_final_place + heat_place + number_in_boat)

#Run Summary 3
boat_size_results	<-lm(final_place ~ semi_final_place + heat_place + number_in_boat)
boat_size_summary	<-summary(boat_size_results)
boat_size_summary

#Calculate F 
fstat_boat_size	<-boat_size_summary$fstatistic
fcrit_boat_size	<-qf(.95,3,68)


#Hypothesis Test
hypoth_rej=0; if(fstat_boat_size>fcrit_boat_size){hypoth_rej=1}
hypoth_rej

#Statistically Significant?

######################################################################################################

#Run Regression 4
lm(final_place ~semi_final_place + heat_place + event_meters)

#Run Summary 4
meters_results	<-lm(final_place ~semi_final_place + heat_place + event_meters)
meters_summary	<-summary(meters_results)
meters_summary

#Calculate F 
fstat_meters	<-meters_summary$fstatistic
fcrit_meters	<-qf(.95,3,68)


#Hypothesis Test
hypoth_rej=0; if(fstat_meters>fcrit_meters){hypoth_rej=1}
hypoth_rej

#Statistically Significant
