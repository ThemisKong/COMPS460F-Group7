# Load the data
my_data <- read.csv("C:/Users/user/Desktop/year4_project/COMPS460FProject/Mental_Health_and_Social_Media_Balance_Dataset.csv")

# Data inspection
head(my_data)
str(my_data)
summary(my_data)
dim(my_data)
names(my_data) 

missing_summary <- colSums(is.na(my_data))
print(missing_summary)

