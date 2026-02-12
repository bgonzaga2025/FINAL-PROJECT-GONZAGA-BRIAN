set.seed(123)

student_data <- data.frame(
  student_ID = 1:10000,
  Gender = sample(c("Male","Female"),10000,replace = TRUE),
  Attendance = sample(50:100,10000,replace = TRUE),
  Assigment_score=sample(10:40,10000,replace = TRUE),
  Exam_Score = sample(20:60,10000,replace = TRUE)
  )

student_data$final_Result <-ifelse(
  student_data$Attendance >= 75 & student_data$Exam_Score >=40,
  "Pass",
  "Fail"
)
write.csv(student_data,"student_data.csv",row.names =FALSE)

student_data

install.packages(c("data.table","dply","ggplot2"))



install.packages("data.table")
library(data.table)

student_data <- fread("student_data.csv")

head(student_data)
student_data
str(student_data)
nrow(student_data)

student_data<-na.omit(student_data)

student_data[,Total_score :=Assigment_score+Exam_Score]

table(student_data$final_Result)

mean(student_data$Exam_Score)

student_data[,.(avg_Exam = mean(Exam_Score)),by = Gender]

cor(student_data$Attendance,student_data$Exam_Score)

ggplot(student_data,aes(X = Exam_Score)) +
  geom_histogram(binwidth = 5) +
  labs(title ="Exam Score Distribution")

install.packages("ggplot2")
library(ggplot2) 

ggplot(student_data,aes(x = Exam_Score)) +
  geom_histogram(binwidth = 5) +
  labs(title ="Exam Score Distribution")

ggplot(student_data,aes(x = Attendance,y = Exam_Score))+
  geom_point(alpha =0.3) +
  labs(title = "Attendance vs Exam Performance")

ggplot(student_data,aes(x = final_Result)) +
  geom_bar()+
  labs(title = "Pass vs Fail Distribution")

student_data$Result_Num <-ifelse(student_data$final_Result =="Pass",1,0)

set.seed(123)

index <- sample(1:nrow(student_data),0.7*nrow(student_data))
train_data <- student_data[index,]
test_data <- student_data[-index,]

model <-glm(
  Result_Num ~ Attendance + Assigment_score + Exam_Score,
  data = train_data,
  family = "binomial"
  )

summary("model")

summary(model)

predicted_prob <- predict(model,test_data,type = "response")

predicted_results <- ifelse(predicted_prob >= 0.5,1,0)

table(
  actual = test_data$Result_Num,
  Predicted = predicted_results
)
accurancy <- mean(predicted_results ==test_data$Result_Num)

accurancy

ggplot(test_data,aes(x = Attendance,fill = factor(predicted_results))) +
geom_histogram(binwidth = 5,position = "dodge") +
  labs(title = "predicted Pass/Fail by Attendance",fill ="Prediction")





















