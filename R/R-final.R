R Intro - Final Exercise


library(DBI)
library(dplyr)
library(ggplot2)
library(sqldf)


con <- dbConnect(odbc::odbc(), "COLLEGE", timeout = 10)
classrooms<- dbGetQuery(con,'select*from "COLLEGE"."dbo"."Classrooms"')
courses<- dbGetQuery(con,'select*from "COLLEGE"."dbo"."Courses"')
departments<- dbGetQuery(con,'select*from "COLLEGE"."dbo"."Departments"')
students<- dbGetQuery(con,'select*from "COLLEGE"."dbo"."Students"')
teachers<- dbGetQuery(con,'select*from "COLLEGE"."dbo"."Teachers"')



### Try with the diferent driver strings to see what works for you

con <- dbConnect(odbc::odbc(), "COLLEGE", timeout = 10)


### Get the students table
students = dbQuery(conn, "SELECT * FROM Students")

classrooms<- dbGetQuery(con,'select*from "COLLEGE"."dbo"."Classrooms"')
courses<- dbGetQuery(con,'select*from "COLLEGE"."dbo"."Courses"')
departments<- dbGetQuery(con,'select*from "COLLEGE"."dbo"."Departments"')
students<- dbGetQuery(con,'select*from "COLLEGE"."dbo"."Students"')
teachers<- dbGetQuery(con,'select*from "COLLEGE"."dbo"."Teachers"')


classcourse<- merge(classrooms,courses, all.x=TRUE)
classcoursesdep=left_join(classcourse,departments, by = c("DepartmentID"="DepartmentId"))
classcoursesdep


dbDisconnect(con)

 
Questions
Q1. Count the number of students on each department

Q1ANS<- classcoursesdep  %>%
  group_by(DepartmentID)  %>%
  summarise(n_distinct(StudentId))
Q1ANS



Q2. How many students have each course of the English department and the total number of students in the department?

Q2ANS<-classcoursesdep  %>%  
  filter(DepartmentName == "English") %>% 
  group_by(CourseName) %>% 
  summarise(n_distinct(StudentId))
Q2ANS
 

  

Q3. How many small (<22 students) and large (22+ students) classrooms are needed for the Science department?

Q3<-classcoursesdep  %>%  
  filter(DepartmentName == "Science") %>% 
  group_by(CourseName) %>% 
  summarise(n_distinct(StudentId))
Q31<-data.frame(Q3)
Q31$Class_Size_Cat= "Small"
Q31$Class_Size_Cat[which(Q31$n_distinct.StudentId.>22)] <- " Big"
Q31ANS<-Q31%>%
      group_by(Class_Size_Cat) %>% 
      summarise(Count=n())
Q31ANS
  

Q4. A feminist student claims that there are more male than female in the College. Justify if the argument is correct

Q4ANS<-students%>% 
    group_by(Gender) %>% 
    summarise(Count=n_distinct(StudentId))
Q4ANS


Q5. For which courses the percentage of male/female students is over 70%?

classcoursestu=left_join(classcourse,students, by = c("StudentId"))
classcoursestu$F_CAT=0
classcoursestu$F_CAT[classcoursestu$Gender=="F"] <-1
classcoursestu
Q5ANS<-classcoursestu%>%
    group_by(CourseName) %>% 
    summarise(Count=n(),Sum=sum(F_CAT),F_AVE=(sum(F_CAT)/n()*100))%>%
    filter(F_AVE > 70)
Q5ANS
  


Q6. For each department, how many students passed with a grades over 80?


Q6Total<- classcoursesdep%>% 
     group_by(DepartmentName) %>% 
     summarise(Total=n_distinct(StudentId))
Q6Total<-data.frame(Q6Total)

Q6GradeOver80<-classcoursesdep%>% 
               filter(degree>80)%>%
               group_by(DepartmentName) %>% 
               summarise(GradeOver80=n_distinct(StudentId))
Q6GradeOver80<-data.frame(Q6GradeOver80)

Q6ANS<-merge(Q6GradeOver80, Q6Total, by = "DepartmentName")%>%
       mutate(PercentOver80=GradeOver80/Total*100)
Q6ANS



Q7. For each department, how many students passed with a grades under 60?

Q7ANS<- classcoursesdep%>% 
        filter(degree<60)%>%
        group_by(DepartmentName) %>% 
        summarise(GradeOver80=n_distinct(StudentId))
Q7ANS


  


Q8. Rate the teachers by their average students grades (in descending order).

classcourse=left_join(classrooms, courses, by = c("CourseId"))
classcourseteach=left_join(classcourse, teachers, by = c("TeacherId"))
classcourseteach$TeachFullName <- paste(classcourseteach$FirstName,classcourseteach$LastName)

Q8ANS<- classcourseteach %>%
        group_by(TeachFullName) %>% 
        summarise(AveStuGrade=mean(degree))%>%
        arrange(desc(AveStuGrade))
Q8ANS
        


Q9. Create a dataframe showing the courses, departments they are associated with, the teacher in each course, and the number of students enrolled in the course (for each course, department and teacher show the names).
###    Dep-Course-Teacher-#students
classcourse=left_join(classrooms, courses, by = c("CourseId"))
classcoursesep=left_join(classcourse,departments, by = c("DepartmentID"="DepartmentId"))
classcoursedepteach=left_join(classcoursesep,teachers,by = c("TeacherId"))
classcoursedepteach$TeachFullName <- paste(classcoursedepteach$FirstName,classcoursedepteach$LastName)
Q9ANS<- classcoursedepteach%>%
        group_by(DepartmentName,CourseName,TeachFullName) %>% 
        summarise(Count=n_distinct(StudentId))
Q9Table<-data.frame(Q9ANS)
Q9Table




Q10. Create a dataframe showing the students, the number of courses they take, the average of the grades per class, and their overall average (for each student show the student name).

library(tidyr)
###    Student ID-Student NAME - # COURSE - AVE.grade by Dep.

#######   יצירת טבלה מרכזית לעבודה ########

students<- dbGetQuery(con,'select*from "COLLEGE"."dbo"."Students"')
students$Stu_name <- paste(students$FirstName,students$LastName)
students
classcoursesdepstu=left_join(classcoursesep, students, by = c("StudentId"))
classcoursesdepstu

#######    יצירת טבלה על מנת לקבל לכל סטודנט ציון ממוצע כולל וחיבורה עם המרכזית ########

resultbein <- classcoursesep %>%
              group_by(StudentId) %>% 
              summarise(AverageDegree=mean(degree))
resultbein <- data.frame(resultbein)
  
classcoursesdepstu1= left_join(classcoursesdepstu, resultbein, by = c("StudentId"))
classcoursesdepstu1


#######    יצירת טבלה על מנת לקבל לכל סטודנט כמה קורסים לוקח וחיבורה עם המרכזית ########

classuni<- classrooms %>%
           group_by(StudentId)%>%
           summarise(CoursesCount=n_distinct(CourseId))
classuni <- data.frame(classuni)

classcoursesdepstu1=left_join(classcoursesdepstu1, classuni, by = c("StudentId"))
classcoursesdepstu1


#########לכל סוטנט, מחלקה, קורסים, ציון ממוצע, ציון ממוצע כללי וכמות קורסים שלוקח ######################

Q10<- classcoursesdepstu1%>%
      group_by(StudentId,Stu_name,DepartmentName,CourseName,degree,AverageDegree,CoursesCount)%>%
      summarise(Count=n_distinct(StudentId))
Q10

############### יצירת פיבוט ######################
Q10ANS<- Q10 %>%                     
         group_by(StudentId,Stu_name,CoursesCount,AverageDegree,DepartmentName) %>%     
         summarise(degree = mean(degree)) %>% 
         ungroup() %>%                
         spread(DepartmentName, degree)   
Q10ANS<- data.frame(Q10ANS) 
Q10ANS


