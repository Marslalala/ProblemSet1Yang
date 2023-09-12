###Problem 1


## Part a

#  Import the data as data.frame
wine_data <- read.csv('wine.data', header = FALSE)

#  Column names setting
Col_name <- c('Class', 'Alcohol', 'Malic acid', 'Ash', 'Alcalinity of ash',
              'Magnesium', 'Total phenols', 'Flavanoids', 'Nonflavanoid phenols',
              'Proanthocyanins', 'Color intensity', 'Hue', 'OD280/OD315 of diluted wines',
              'Proline')
colnames(wine_data) <- Col_name
names(wine_data)


## Part b

#  Number of wines check in each of 3 class
#  Class distribution in "wine.names": Class 1: 59; Class 2: 71; Class 3: 48
length(which(wine_data$Class == 1))
length(which(wine_data$Class == 2))
length(which(wine_data$Class == 3))


## Part c

#  Q1
max_alc <- max(wine_data$Alcohol)
#   Check the uniqueness of the wine with the highest alcohol content
length(which(wine_data$Alcohol == max_alc))
wine_data[wine_data$Alcohol == max_alc, ][, 1]
#  The wine with the highest alcohol content belongs to class 1

#  Q2
min_alc <- min(wine_data$Alcohol)
#  Check the uniqueness of the wine with the lowest alcohol content
length(which(wine_data$Alcohol == min_alc))
wine_data[wine_data$Alcohol == min_alc, ][, 1]
#  The wine with the lowest alcohol content belongs to class 2

#  Q3
Beer_mag <- 114
length(which(wine_data$Magnesium > Beer_mag))
#  There are 26 wines with level of magnesium higher than that

#  Q4
#  Separate all 3 classes
class_1 <- subset(wine_data, wine_data$Class == 1)
class_2 <- subset(wine_data, wine_data$Class == 2)
class_3 <- subset(wine_data, wine_data$Class == 3)
#  Then get the values
length(which(class_1$Magnesium > Beer_mag))
length(which(class_2$Magnesium > Beer_mag))
length(which(class_3$Magnesium > Beer_mag))
#  There are 15 wines in class 1 with level of magnesium higher than German beers, 
#  6 for class 2 and 5 for class 3


## Part d

#  Calculate the overall average
ov_avg <- c(mean(wine_data$Alcohol), mean(wine_data$`Malic acid`), 
            mean(wine_data$Ash), mean(wine_data$`Alcalinity of ash`),
            mean(wine_data$Magnesium), mean(wine_data$`Total phenols`),
            mean(wine_data$Flavanoids), mean(wine_data$`Nonflavanoid phenols`),
            mean(wine_data$Proanthocyanins), mean(wine_data$`Color intensity`),
            mean(wine_data$Hue), mean(wine_data$`OD280/OD315 of diluted wines`),
            mean(wine_data$Proline))
#  Calculate the average for each class
c1_avg <- c(mean(class_1$Alcohol), mean(class_1$`Malic acid`), 
            mean(class_1$Ash), mean(class_1$`Alcalinity of ash`),
            mean(class_1$Magnesium), mean(class_1$`Total phenols`),
            mean(class_1$Flavanoids), mean(class_1$`Nonflavanoid phenols`),
            mean(class_1$Proanthocyanins), mean(class_1$`Color intensity`),
            mean(class_1$Hue), mean(class_1$`OD280/OD315 of diluted wines`),
            mean(class_1$Proline))

c2_avg <- c(mean(class_2$Alcohol), mean(class_2$`Malic acid`), 
            mean(class_2$Ash), mean(class_2$`Alcalinity of ash`),
            mean(class_2$Magnesium), mean(class_2$`Total phenols`),
            mean(class_2$Flavanoids), mean(class_2$`Nonflavanoid phenols`),
            mean(class_2$Proanthocyanins), mean(class_2$`Color intensity`),
            mean(class_2$Hue), mean(class_2$`OD280/OD315 of diluted wines`),
            mean(class_2$Proline))

c3_avg <- c(mean(class_3$Alcohol), mean(class_3$`Malic acid`), 
            mean(class_3$Ash), mean(class_3$`Alcalinity of ash`),
            mean(class_3$Magnesium), mean(class_3$`Total phenols`),
            mean(class_3$Flavanoids), mean(class_3$`Nonflavanoid phenols`),
            mean(class_3$Proanthocyanins), mean(class_3$`Color intensity`),
            mean(class_3$Hue), mean(class_3$`OD280/OD315 of diluted wines`),
            mean(class_3$Proline))
#  Then create a table
avg_tab <- t(data.frame(ov_avg, c1_avg, c2_avg, c3_avg))
#  Set the rows and columns names to make the table clear
row.names(avg_tab) <- c('Overall Average', 'Average of Class 1', 
                        'Average of Class 2', 'Average of Class 3')
colnames(avg_tab) <- Col_name[-1]
#  View the table
View(avg_tab)

## Part e

#  Carry out a t-test between class 1 & 2
t.test(class_1$Ash, class_2$Ash)
#  The p-value is 2.124e-05, which means the result is statistically significant.
#  Therefore, the null hypothesis, the level of Ash differs between two classes, should
#  be rejected.

#  Carry out a t-test between class 1 & 3
t.test(class_1$Ash, class_3$Ash)
#  The p-value is 0.643, which means the result is statistically insignificant.
#  Therefore, the null hypothesis, the level of Ash differs between two classes, is true

#  Carry out a t-test between class 2 & 3
t.test(class_2$Ash, class_3$Ash)
#  The p-value is 5.627e-05, which means the result is statistically significant.
#  Therefore, the null hypothesis, the level of Ash differs between two class, should
#  be rejected.