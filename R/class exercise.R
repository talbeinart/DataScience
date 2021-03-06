
###### 1 ######
# להשתמש טבלת נתונים בשם 
# iris 
# ולחשב עבור כל אחד מארבעה העמודות הראשונות את הממוצע, הערך המינימום והמקסימום

df<- iris
summary(df)
sapply(df[1:4],min)



###### 2 ######
# להשתמש טבלת נתונים בשם 
# mtcars 
# ולבצע את 33החישובים הבאים
df1<-mtcars
summary(df1)

# mpg השורש של העמודה 
SQRT<-sqrt(df1$mpg)

# disp  של העמודה log
LOG<-log10(df1$disp)

#   בחזקת שלוש wt
a <- df1$wt^3

###### 3 ######
#  "+" ליצור שרשור של הערכים הבאים מחוברים עם הסימן	

s1 <- c("age", "gender", "height", "weight")
s1 <- paste(s1,collapse="+")
s1
class(s1)
str(s1)


###### 4 ######
#  חשבו את הממוצע של המטריצה הבאה לפי עמודה, שורה ושל המטריצה כולה

m1 <- matrix(c(4,7,-8,3,0,-2,1,-5,12,-3,6,9), ncol=4)
m1
mean(m1)
rowMeans(m1)
colMeans(M1)
###### 5 ######
#  (Z-ל A-מ) לכתוב לולאה שמדפיסה את האותיות באנגלית בסדר הפוך
LETTERS
for (i in seq(length(LETTERS),1,-1)){
  print(LETTERS[i])}
###### 6 ######
# לכתוב לולאה שמדפיס מספרים שלמים בין 1 ל-10 באופן אקראי ולעצור כאשר המספר 8 מופיע בפעם הראשונה.
# תעשו זאת בשתי שיטות: פעם אחת עם לולאה
#   for
# ופעם אחרת עם לולאה 
#   while. 
# ניתן להשתמש בפונקציה
#   sample 
# או בפונקציה
#   runif 
# כדי לייצר את המספרים האקראיים.


###### 7 ######
# בהינתן שני הווקטורים הבאים, בעזרת לולאה תחברו את המילים שבהם למשפט אחת כך שכל פעם מדלגים בין הווקטורים.

a <- c("well", "you", "merged", "vectors", "one") 
b <- c("done", "have", "two", "into", "phrase")


###### 7 ######
# השתמשו בטבלת 
# iris 
# כדי ליצור את הגרפים הבאים

hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.width)
#  היסטוגרמה עבור ארבעה המשתנים הראשונים


# Species גרף עוגה עם המשתנה 

pie(table(iris$Species))
# גרף של
# Petal.Length
# מול
# Petal.Width 
# ולצבוע לפי
# Species
plot(iris$Petal.Length~iris$Petal.Width,col=iris$Species)
scatter.smooth(iris$Petal.Length~iris$Petal.Width,col=iris$Species)
#  ליצור
# boxplot 
# של 
# Sepal.Length 
# לפי 
# Species

boxplot(iris$Sepal.Length~iris$Species)
