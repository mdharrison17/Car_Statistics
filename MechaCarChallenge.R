orig_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F) #load csv into table
names(orig_table)[names(orig_table) == "vehicle weight"] <- "vehicleweight" #rename column
names(orig_table)[names(orig_table) == "vehicle length"] <- "vehiclelength" #rename column
names(orig_table)[names(orig_table) == "spoiler angle"] <- "spoilerangle" #rename column
names(orig_table)[names(orig_table) == "ground clearance"] <- "groundclearance" #rename column

used_matrix <- as.matrix(orig_table[,c("vehiclelength","vehicleweight","spoilerangle", "groundclearance", "mpg")]) #convert data frame into numeric matrix
cor(used_matrix) #calculate the correlation between fields in the mpg table

lm(mpg ~ vehiclelength + vehicleweight + spoilerangle + groundclearance,data=orig_table) #generate multiple linear regression model
summary(lm(mpg ~ vehiclelength + vehicleweight + spoilerangle + groundclearance,data=orig_table)) #generate summary statistics

##########################################
suspension_coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
library(tidyverse) ##add tidyverse library
summarize_psi <- suspension_coil %>% summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), Variance_PSI=var(PSI), Standard_Deviation=sd(PSI) ) # create summary table

###########################################
sample_table <- suspension_coil %>% sample_n(50) #randomly sample 50 data points
t.test((sample_table$PSI),mu=mean(suspension_coil$PSI)) #compare sample versus population means
