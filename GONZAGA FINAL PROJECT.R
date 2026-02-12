set.seed(12345)

student_data <- data.frame(
  student_ID = 1:5000,
  Gender = sample(c("Male", "Female"), 5000, replace = TRUE),
  Semester = sample(c("Semester 1", "Semester 2"), 5000, replace = TRUE),
  Attendance = sample(50:100, 5000, replace = TRUE),
  Assignment_score = sample(10:50, 5000, replace = TRUE),
  Exam_score = sample(20:60, 5000, replace = TRUE),
  login_times = round(rpois(5000, lambda = 60))
)
#Display of student data
student_data

# Total score

student_data$total_score <- 
  student_data$Assignment_score + student_data$Exam_score

# Final Result pass or fail

student_data$final_Result <-ifelse(
  student_data$Attendance >= 60 & student_data$Exam_score >=40,
  "Pass",
  "Fail"
)
#Display change
student_data

#view rows
head(student_data)

#check strings
str(student_data)

#confirm size
nrow(student_data)

#Save the file
write.csv(student_data,"student_data_project.csv",row.names =FALSE)

#Install packages 

install.packages(c("data.table","dply","ggplot2"))
library(data.table)
library(dply)
library(ggplot2)

#data processing

install.packages("data.table")
library(data.table)
student_data <-fread("student_data_project.csv")

#view rows
head(student_data)

#check strings
str(student_data)

#confirm size
nrow(student_data)

#Data cleaning
student_data <-na.omit(student_data)

#total score
student_data[,total_score :=Assignment_score+Exam_score]

student_data

#descriptive analysis(Pass & failed)

table(student_data$final_Result)

#Average performance for Exam & Assignment & Gender 
mean(student_data$Exam_score)
mean(student_data$Assignment_score)

student_data[,.(Avg_Exam = mean(Exam_score)),by = Gender]

#correlation
cor(student_data$Attendance,student_data$Exam_score)
cor(student_data$Attendance,student_data$Assignment_score)
cor(student_data$Attendance,student_data$login_times)
cor(student_data$Attendance,student_data$total_score)

#visualization 

install.packages("ggplot2")
library(ggplot2)

#exam_score distribution
ggplot(student_data,aes(x = Exam_score)) +
  geom_histogram (binwidth = 5,
                 fill = "blue" +
  labs(title ="Exam score Distribution")
  
#Attendance vs performance 
ggplot(student_data,aes(x = Attendance,y = Exam_score))+
    geom_point(alpha =0.2) +
    labs(title = "Attendance vs Exam Performance")
  
#Pass vs fail chart 
  ggplot(student_data,aes(x = final_Result)) +
    geom_bar()+
    labs(title = "Pass vs Fail")

#Predict the future using current data

student_data$Result_Num <-ifelse(student_data$final_Result =="Pass",1,0)

#Split train & test data
set.seed(12345)

index <- sample(1:nrow(student_data),0.7*nrow(student_data))
train_data <- student_data[index,]
test_data <- student_data[-index,]

#Predictive model(regression)
model <-glm(
  Result_Num ~Attendance + Assignment_score + Exam_score,
  data = train_data,
  family = "binomial"
  )
summary(model)
#Make prediction
predicted_prob <- predict(model,test_data,type = "response")

#convert proberlilities

predicted_result <- ifelse(predicted_prob >=0.5,1,0)

















