getwd()
setwd("/Users/jingjiezhao/Downloads")

# install RColorBrewer package
install.packages("RColorBrewer")
library(RColorBrewer)

# Import data
campus_re <- read.csv("campus_recruitment.csv")
head(campus_re)
tail(campus_re)

# Summarize data
summary(campus_re)
str(campus_re)

# Replace column names
colnames(campus_re)
my_column_name <- c("serial_number", "gender", "ten_sec_edu_per", "ten_board_of_edu",
                    "twelve_sec_edu_per", "twelve_board_of_edu", "specialize_sec_edu",
                    "degree_percent", "degree_type_field", "work_ex", "entrence_test_per",
                    "MBA_specialize", "MBA_per", "hiring_status", "salary")
colnames(campus_re) <- my_column_name
colnames(campus_re)

# Backup
campus_re_backup <- campus_re
campus_re_backup2 <- campus_re # 做分数分类用

# Pick NA rows (HAVE MISSING VALUES)
summary(is.na(campus_re_backup))

# Visualizing null values (给missing value画图)
install.packages("naniar")
library(naniar)
vis_miss(campus_re_backup) 
### 解释:
# We can see that there is only Salary column has Missing Values.
# The Salary has null values because the students who aren't placed will not have any Salary values.
# We can replace the null values with 0.


# Imputing MISSING VALUE - campus_re_backup (as = 0)
campus_re_backup[is.na(campus_re_backup)] <- 0
# Check it again (NA)
summary(is.na(campus_re_backup)) # NO MISSING VALUE!!
# Imputing MISSING VALUE - campus_re_backup2 (as = 0)
campus_re_backup2[is.na(campus_re_backup2)] <- 0
# Check it again (NA)
summary(is.na(campus_re_backup2)) # NO MISSING VALUE!!


######################################################################################

# Data Visualization
# Univariate Exploration
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
library(ggplot2)
library(plyr)
library(dbplyr)
library(dplyr)
library(ggpubr)

# 1. GENDER
# Check how many levels in "gender"
levels(campus_re_backup$gender)
# Count the frequency of each level in "gender"
sum(data.frame(campus_re_backup$gender) == "F")
sum(data.frame(campus_re_backup$gender) == "M")
# Percentage of each level
female_per <- (sum(data.frame(campus_re_backup$gender) == "F") /
                 length(campus_re_backup$gender)) * 100
female_per # 35%
male_per <- (sum(data.frame(campus_re_backup$gender) == "M") / 
               length(campus_re_backup$gender)) * 100
male_per #65%


# 2. 10th secondary Board of education 
# Check how many levels in "ten_board_of_edu"
levels(campus_re_backup$ten_board_of_edu)
# Count the frequency of each level in "ten_board_of_edu"
sum(data.frame(campus_re_backup$ten_board_of_edu) == "Central")
sum(data.frame(campus_re_backup$ten_board_of_edu) == "Others")
# Percentage of each level
central_10_per <- (sum(data.frame(campus_re_backup$ten_board_of_edu) == "Central") /
                     length(campus_re_backup$ten_board_of_edu)) * 100
central_10_per # 54%
others_10_per <- (sum(data.frame(campus_re_backup$ten_board_of_edu) == "Others") / 
                    length(campus_re_backup$ten_board_of_edu)) * 100
others_10_per # 46%


# 3. 12th secondary Board of education
# Check how many levels in "twelve_board_of_edu"
levels(campus_re_backup$twelve_board_of_edu)
# Count the frequency of each level in "twelve_board_of_edu"
sum(data.frame(campus_re_backup$twelve_board_of_edu) == "Central")
sum(data.frame(campus_re_backup$twelve_board_of_edu) == "Others")
# Percentage of each level
central_12_per <- (sum(data.frame(campus_re_backup$twelve_board_of_edu) == "Central") / 
                     length(campus_re_backup$twelve_board_of_edu)) * 100
central_12_per # 39%
others_12_per <- (sum(data.frame(campus_re_backup$twelve_board_of_edu) == "Others") / 
                    length(campus_re_backup$twelve_board_of_edu)) * 100
others_12_per # 61%


# 4. Higher Secondary Education - Specialization
# Check how many levels in "specialize_sec_edu"
levels(campus_re_backup$specialize_sec_edu)
# Count the frequency of each level in "specilize_sec_edu"
sum(data.frame(campus_re_backup$specialize_sec_edu) == "Arts")
sum(data.frame(campus_re_backup$specialize_sec_edu) == "Commerce")
sum(data.frame(campus_re_backup$specialize_sec_edu) == "Science")
# Percentage of each level
arts_per <- (sum(data.frame(campus_re_backup$specialize_sec_edu) == "Arts") / 
               length(campus_re_backup$specialize_sec_edu)) * 100
arts_per # 5%
commerce_per <- (sum(data.frame(campus_re_backup$specialize_sec_edu) == "Commerce") / 
                   length(campus_re_backup$specialize_sec_edu)) * 100
commerce_per # 53%
science_per <- (sum(data.frame(campus_re_backup$specialize_sec_edu) == "Science") / 
                  length(campus_re_backup$specialize_sec_edu)) * 100
science_per # 42%


# 5. Degree Type (Field)
# Check how many levels in "degree_type_field"
levels(campus_re_backup$degree_type_field)
# Count the frequency of each level in "degree_type(field)"
sum(data.frame(campus_re_backup$degree_type_field) == "Comm&Mgmt")
sum(data.frame(campus_re_backup$degree_type_field) == "Others")
sum(data.frame(campus_re_backup$degree_type_field) == "Sci&Tech")
# Percentage of each level
c_m_per <- (sum(data.frame(campus_re_backup$degree_type_field) == "Comm&Mgmt") / 
              length(campus_re_backup$degree_type_field)) * 100
c_m_per # 67%
s_t_per <- (sum(data.frame(campus_re_backup$degree_type_field) == "Sci&Tech") / 
              length(campus_re_backup$degree_type_field)) * 100
s_t_per # 27%
others_per <- (sum(data.frame(campus_re_backup$degree_type_field) == "Others") / 
                 length(campus_re_backup$degree_type_field)) * 100
others_per # 5%


# 6. Work Experience
# Check how many levels in "work_ex"
levels(campus_re_backup$work_ex)
# Count the frequency of each level in "work_ex"
sum(data.frame(campus_re_backup$work_ex) == "No")
sum(data.frame(campus_re_backup$work_ex) == "Yes")
# Percentage of each level
have_workex_per <- (sum(data.frame(campus_re_backup$work_ex) == "Yes")/ 
                      length(campus_re_backup$work_ex)) * 100
have_workex_per # 34%
no_workex_per <- (sum(data.frame(campus_re_backup$work_ex) == "No") / 
                    length(campus_re_backup$work_ex)) * 100
no_workex_per # 66%


# 7. MBA Specialization
# Check how many levels in "MBA_specialize"
levels(campus_re_backup$MBA_specialize)
# Count the frequency of each level in "MBA_specialize"
sum(data.frame(campus_re_backup$MBA_specialize) == "Mkt&Fin")
sum(data.frame(campus_re_backup$MBA_specialize) == "Mkt&HR")
# Percentage of each level
MF_per <- (sum(data.frame(campus_re_backup$MBA_specialize) == "Mkt&Fin") / 
             length(campus_re_backup$MBA_specialize)) * 100
MF_per # 56%
MH_per <- (sum(data.frame(campus_re_backup$MBA_specialize) == "Mkt&HR") / 
             length(campus_re_backup$MBA_specialize)) * 100
MH_per # 44%


# 8. Hiring Status
# Check how many levels in "hiring_status"
levels(campus_re_backup$hiring_status)
# Count the frequency of each level in "hiring_status"
sum(data.frame(campus_re_backup$hiring_status) == "Not Placed")
sum(data.frame(campus_re_backup$hiring_status) == "Placed")
# Percentage of each level
placed_per <- (sum(data.frame(campus_re_backup$hiring_status) == "Placed") / 
                 length(campus_re_backup$hiring_status)) * 100
placed_per # 69%
not_placed_per <- (sum(data.frame(campus_re_backup$hiring_status) == "Not Placed") / 
                     length(campus_re_backup$hiring_status)) * 100
not_placed_per # 31%


# 9. 10th sec edu per
# using "campus_re_backup2"
# Sectioning the 10th education percentage into 3 groups
# Note:
# Group1: 0% - 60%
# Group2: 61% - 80%
# Group3: 81% - 100%
campus_re_backup2[campus_re_backup2['ten_sec_edu_per']<=60,'ten_group']= "Group3"
campus_re_backup2[(campus_re_backup2['ten_sec_edu_per']>60) &
                    (campus_re_backup2['ten_sec_edu_per']<81),'ten_group']= "Group2"
campus_re_backup2[(campus_re_backup2['ten_sec_edu_per']>80) &
                    (campus_re_backup2['ten_sec_edu_per']<101),'ten_group']= "Group1"
# Convert chr to factor
class(campus_re_backup2$ten_group)
campus_re_backup2$ten_group <- as.factor(campus_re_backup2$ten_group)
str(campus_re_backup2)
# Check how many levels in "ten_group"
levels(campus_re_backup2$ten_group)
# Count the frequency of each level in "ten_group"
sum(data.frame(campus_re_backup2$ten_group) == "Group1")
sum(data.frame(campus_re_backup2$ten_group) == "Group2")
sum(data.frame(campus_re_backup2$ten_group) == "Group3")
# Percentage of each level
g1_per <- (sum(data.frame(campus_re_backup2$ten_group) == "Group1") / 
                 length(campus_re_backup2$ten_group)) * 100
g1_per # 14%
g2_per <- (sum(data.frame(campus_re_backup2$ten_group) == "Group2") / 
                     length(campus_re_backup2$ten_group)) * 100
g2_per # 62%
g3_per <- (sum(data.frame(campus_re_backup2$ten_group) == "Group3") / 
             length(campus_re_backup2$ten_group)) * 100
g3_per # 24%


# 10. 12th sec edu per
# using "campus_re_backup2"
# Sectioning the 12th education percentage into 3 groups
# Note:
# Group1: 0% - 60%
# Group2: 61% - 80%
# Group3: 81% - 100%
campus_re_backup2[campus_re_backup2['twelve_sec_edu_per']<=60,'twelve_group']= "Group3"
campus_re_backup2[(campus_re_backup2['twelve_sec_edu_per']>60) &
                    (campus_re_backup2['twelve_sec_edu_per']<81),'twelve_group']= "Group2"
campus_re_backup2[(campus_re_backup2['twelve_sec_edu_per']>80) &
                    (campus_re_backup2['twelve_sec_edu_per']<101),'twelve_group']= "Group1"
# Convert chr to factor
class(campus_re_backup2$twelve_group)
campus_re_backup2$twelve_group <- as.factor(campus_re_backup2$twelve_group)
str(campus_re_backup2)
# Check how many levels in "ten_group"
levels(campus_re_backup2$twelve_group)
# Count the frequency of each level in "ten_group"
sum(data.frame(campus_re_backup2$twelve_group) == "Group1")
sum(data.frame(campus_re_backup2$twelve_group) == "Group2")
sum(data.frame(campus_re_backup2$twelve_group) == "Group3")
# Percentage of each level
g1_12_per <- (sum(data.frame(campus_re_backup2$twelve_group) == "Group1") / 
             length(campus_re_backup2$twelve_group)) * 100
g1_12_per # 8%
g2_12_per <- (sum(data.frame(campus_re_backup2$twelve_group) == "Group2") / 
             length(campus_re_backup2$twelve_group)) * 100
g2_12_per # 68%
g3_12_per <- (sum(data.frame(campus_re_backup2$twelve_group) == "Group3") / 
             length(campus_re_backup2$twelve_group)) * 100
g3_12_per # 24%


# 11. Entrence test percentage
# using "campus_re_backup2"
# Sectioning the 12th education percentage into 3 groups
# Note:
# Group1: 0% - 60%
# Group2: 61% - 80%
# Group3: 81% - 100%
campus_re_backup2[campus_re_backup2['entrence_test_per']<=60,'entrence']= "Group3"
campus_re_backup2[(campus_re_backup2['entrence_test_per']>60) &
                    (campus_re_backup2['entrence_test_per']<81),'entrence']= "Group2"
campus_re_backup2[(campus_re_backup2['entrence_test_per']>80) &
                    (campus_re_backup2['entrence_test_per']<101),'entrence']= "Group1"
# Convert chr to factor
class(campus_re_backup2$entrence)
campus_re_backup2$entrence <- as.factor(campus_re_backup2$entrence)
str(campus_re_backup2)
# Check how many levels in "ten_group"
levels(campus_re_backup2$entrence)
# Count the frequency of each level in "ten_group"
sum(data.frame(campus_re_backup2$entrence) == "Group1")
sum(data.frame(campus_re_backup2$entrence) == "Group2")
sum(data.frame(campus_re_backup2$entrence) == "Group3")
# Percentage of each level
entr_1_per <- (sum(data.frame(campus_re_backup2$entrence) == "Group1") / 
                length(campus_re_backup2$entrence)) * 100
entr_1_per # 28%
entr_2_per <- (sum(data.frame(campus_re_backup2$entrence) == "Group2") / 
                length(campus_re_backup2$entrence)) * 100
entr_2_per # 44%
entr_3_per <- (sum(data.frame(campus_re_backup2$entrence) == "Group3") / 
                length(campus_re_backup2$entrence)) * 100
entr_3_per # 27%


# 12. salary
# using "campus_re_backup2"
# Sectioning the 12th education percentage into 8 groups
# Note:
# Group0: 0 - 199999
# Group1: 200000 - 299999
# Group2: 300000 - 399999
# Group3: 400000 - 499999
# Group4: 500000 - 599999
# Group5: 600000 - 699999
# Group6: 700000 - 799999
# Group7: 800000 - 899999
# Group8: 900000 - 999999
campus_re_backup2[campus_re_backup2['salary'] < 200000,'salary_group']= "G0"
campus_re_backup2[(campus_re_backup2['salary'] >= 200000) &
                    (campus_re_backup2['salary'] < 300000),'salary_group']= "G1"
campus_re_backup2[(campus_re_backup2['salary'] >= 300000) &
                    (campus_re_backup2['salary'] < 400000),'salary_group']= "G2"
campus_re_backup2[(campus_re_backup2['salary'] >= 400000) &
                    (campus_re_backup2['salary'] < 500000),'salary_group']= "G3"
campus_re_backup2[(campus_re_backup2['salary'] >= 500000) &
                    (campus_re_backup2['salary'] < 600000),'salary_group']= "G4"
campus_re_backup2[(campus_re_backup2['salary'] >= 600000) &
                    (campus_re_backup2['salary'] < 700000),'salary_group']= "G5"
campus_re_backup2[(campus_re_backup2['salary'] >= 700000) &
                    (campus_re_backup2['salary'] < 800000),'salary_group']= "G6"
campus_re_backup2[(campus_re_backup2['salary'] >= 800000) &
                    (campus_re_backup2['salary'] < 900000),'salary_group']= "G7"
campus_re_backup2[(campus_re_backup2['salary'] >= 900000) &
                    (campus_re_backup2['salary'] < 1000000),'salary_group']= "G8"
# Convert chr to factor
class(campus_re_backup2$salary_group)
campus_re_backup2$salary_group <- as.factor(campus_re_backup2$salary_group)
str(campus_re_backup2)
# Check how many levels in "ten_group"
levels(campus_re_backup2$salary_group)
# Count the frequency of each level in "ten_group"
sum(data.frame(campus_re_backup2$salary_group) == "G0")
sum(data.frame(campus_re_backup2$salary_group) == "G1")
sum(data.frame(campus_re_backup2$salary_group) == "G2")
sum(data.frame(campus_re_backup2$salary_group) == "G3")
sum(data.frame(campus_re_backup2$salary_group) == "G4")
sum(data.frame(campus_re_backup2$salary_group) == "G5")
sum(data.frame(campus_re_backup2$salary_group) == "G6")
sum(data.frame(campus_re_backup2$salary_group) == "G7")
sum(data.frame(campus_re_backup2$salary_group) == "G8")
# Percentage of each level
sala_0_per <- (sum(data.frame(campus_re_backup2$salary_group) == "G0") / 
                 length(campus_re_backup2$salary_group)) * 100
sala_0_per # 31%
sala_1_per <- (sum(data.frame(campus_re_backup2$salary_group) == "G1") / 
                 length(campus_re_backup2$salary_group)) * 100
sala_1_per # 46%
sala_2_per <- (sum(data.frame(campus_re_backup2$salary_group) == "G2") / 
                 length(campus_re_backup2$salary_group)) * 100
sala_2_per # 17%
sala_3_per <- (sum(data.frame(campus_re_backup2$salary_group) == "G3") / 
                 length(campus_re_backup2$salary_group)) * 100
sala_3_per # 4%
sala_4_per <- (sum(data.frame(campus_re_backup2$salary_group) == "G4") / 
                 length(campus_re_backup2$salary_group)) * 100
sala_4_per # 1%
sala_5_per <- (sum(data.frame(campus_re_backup2$salary_group) == "G5") / 
                 length(campus_re_backup2$salary_group)) * 100
sala_5_per # 1%
sala_6_per <- (sum(data.frame(campus_re_backup2$salary_group) == "G6") / 
                 length(campus_re_backup2$salary_group)) * 100
sala_6_per # 0%
sala_7_per <- (sum(data.frame(campus_re_backup2$salary_group) == "G7") / 
                 length(campus_re_backup2$salary_group)) * 100
sala_7_per # 0%
sala_8_per <- (sum(data.frame(campus_re_backup2$salary_group) == "G8") / 
                 length(campus_re_backup2$salary_group)) * 100
sala_8_per # 0%



### Univariate Histogram (Graph 1 ~ Graph 12)
# Barchart
graph1 <- ggplot(data = campus_re_backup, aes(x = gender)) +
  geom_bar(fill = "lightblue") +
  labs(y="Count", x = "Gender") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  ggtitle("Comparison of Males and Females \n") +
  theme(plot.title = element_text(size = 10, face = "bold"))
  

graph2 <- ggplot(data = campus_re_backup, aes(x = ten_board_of_edu)) +
  geom_bar(fill = "lightblue") +
  labs(y="Count", x = "Groups") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  ggtitle("10th Grade Board of Education \n") +
  theme(plot.title = element_text(size = 10, face = "bold"))

graph3 <- ggplot(data = campus_re_backup, aes(x = twelve_board_of_edu)) +
  geom_bar(fill = "lightblue") +
  labs(y="Count", x = "Groups") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  ggtitle("12th Grade Board of Education \n") +
  theme(plot.title = element_text(size = 10, face = "bold"))

graph4 <- ggplot(data = campus_re_backup, aes(x = specialize_sec_edu)) +
  geom_bar(fill = "lightblue") +
  labs(y="Count", x = "Faculty") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  ggtitle("Higher Secondary Education - Specialization \n") +
  theme(plot.title = element_text(size = 10, face = "bold"))

graph5 <- ggplot(data = campus_re_backup, aes(x = degree_type_field)) +
  geom_bar(fill = "lightblue") +
  labs(y="Count", x = "Degree") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  ggtitle("Higher Secondary Education Degree Type \n") +
  theme(plot.title = element_text(size = 10, face = "bold"))

graph6 <- ggplot(data = campus_re_backup, aes(x = work_ex)) +
  geom_bar(fill = "lightblue") +
  labs(y="Count", x = "Work Experience") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  ggtitle("Work Experiences \n") +
  theme(plot.title = element_text(size = 10, face = "bold"))

graph7 <- ggplot(data = campus_re_backup, aes(x = MBA_specialize)) +
  geom_bar(fill = "lightblue") +
  labs(y="Count", x = "MBA Specialization") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  ggtitle("MBA Specialization \n") +
  theme(plot.title = element_text(size = 10, face = "bold"))

graph8 <- ggplot(data = campus_re_backup, aes(x = hiring_status)) +
  geom_bar(fill = "lightblue") +
  labs(y="Count", x = "Status") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  ggtitle("Hiring Status \n") +
  theme(plot.title = element_text(size = 10, face = "bold"))

graph9 <- ggplot(data = campus_re_backup2, aes(x = ten_group)) +
  geom_bar(fill = "lightblue") +
  labs(y="Count", x = "Percentage Groups") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  ggtitle("Grade 10 Percentage Groups \n") +
  theme(plot.title = element_text(size = 10, face = "bold"))

graph10 <- ggplot(data = campus_re_backup2, aes(x = twelve_group)) +
  geom_bar(fill = "lightblue") +
  labs(y="Count", x = "Percentage Groups") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  ggtitle("Grade 12 Percentage Groups \n") +
  theme(plot.title = element_text(size = 10, face = "bold"))

graph11 <- ggplot(data = campus_re_backup2, aes(x = entrence)) +
  geom_bar(fill = "lightblue") +
  labs(y="Count", x = "Percentage Groups") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  ggtitle("Entrence Test Percentage \n") +
  theme(plot.title = element_text(size = 10, face = "bold"))

graph12 <- ggplot(data = campus_re_backup2, aes(x = salary_group)) +
  geom_bar(fill = "lightblue") +
  labs(y="Count", x = "Salary Groups") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  ggtitle("Salary Distribution \n") +
  theme(plot.title = element_text(size = 10, face = "bold"))



ggarrange(graph1, graph2, graph3, graph4, graph5, graph6, 
          graph7, graph8, graph9, graph10, graph11, graph12, ncol = 3, nrow = 4)



##### 结论:
### Conclusion1:
# We have about 140 male (65%) and 75 female students (35%) in our sample.
### Conclusion2:
# We have about 116 students (54%) from Central Board and 99 students (46%) from Other Board.
### Conclusion3:
# We have about 84 students (39%) from Central Board and 131 students (61%) from Other Board.
### Conclusion4:
# We have about 11 students (5%) from Arts,
# 113 students (53%) from Commerce and
# 91 students (42%) from Science.
### Conclusion5:
# We have 145 students (67%) from Commerce&Management,
# 70 students (32%) from Science&Technology and others.
### Conclusion6:
# We can see that more students have no work experience before graduation. 
# This is an interesting phenomenon. 
# We can set up a model to see
# whether work experience affects the results of campus recruitment.
### Conclusion7:
# There is NOT much difference in the number of our students specializing between Mkt&Fin and Mkt&HR. 
# In comparison, slightly more students specizlize in Mkt&Fin.
### Conclusion8:
# Majority of students were eventually hired by the company
### Conclusion9:
# Majority of grade 10 students belong to Group2: 61% - 80% 
### Conclusion10:
# Approximately half of the students who belong to Group1 in grade 10 rose to Group2. 
# The proportion of students whose grades belong to the third group (Group3) has not changed much.
### Conclusion11:
# The results of the entrance examination show that 
# there is little difference in the number of students in each group. 
# In comparison, 44% of the students belong to the second group, 
# which is slightly more compare to Group1 (28%) and Group3 (27%).
### Conclusion12:
# Most companies limit the salary package for campus recruitment from 200,000 to 400,000. 
# We can see that salary in this range account for 63% of the entire sample.
# But it does not rule out that the company will offer high wages. 
# One student in the sample received a salary of up to 940,000.

######################################################################

#Bivariate Exploration
data("iris")
library(tidyr)
install.packages("GGally")
library(GGally)

# Check data type
str(campus_re_backup)

# 1. Restrict the dataset (NUM only)
campus_re_backup_bivariate <- campus_re_backup[,
                                               c("ten_sec_edu_per",
                                                 "twelve_sec_edu_per",
                                                 "degree_percent",
                                                 "entrence_test_per",
                                                 "MBA_per",
                                                 "salary",
                                                 "hiring_status")]

# Plot Histogram
# 无颜色
a1<- ggpairs(campus_re_backup_bivariate,
             lower = list(continous = "smooth"),
             diag = list(continous = "barDiag"), 
             axisLabels = "show")
a1

# 有颜色
a2 <- ggscatmat(campus_re_backup_bivariate, 
                columns = c("ten_sec_edu_per",
                            "twelve_sec_edu_per",
                            "degree_percent",
                            "entrence_test_per",
                            "MBA_per",
                            "salary"), 
                color = "hiring_status")
a2
### 全部都是significantly corelation, 
# 看来independent variable很少或者没有
# 还可以描述一下hiring_status状态下的区别


# 2. gender & hiring Status
# Bar plot
graph13 <- campus_re_backup %>% 
  group_by(gender, hiring_status) %>% 
  tally() %>% 
  complete(hiring_status, fill = list(n = 0)) %>% 
  mutate(counting = n)

graph13 <- ggplot(graph13, aes(hiring_status, counting, fill = gender)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x ="Status", y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  ggtitle("Hiring Status with Gender \n") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette="Paired")
# Hiring percentage
male_hire_per <- (100 /140) * 100
male_hire_per #71.4%
female_hire_per <- ((148 - 100) / 75) * 100
female_hire_per #64%
### 解释:
# In the campus recruitment, male acceptance rate  
# is slightly higher than the fema


# 3. ten_sec_edu_per & hiring status
# Box Plot
graph14 <- campus_re_backup %>%
  ggplot(aes(y = ten_sec_edu_per, fill = ten_sec_edu_per)) +
  geom_boxplot(aes(fill = hiring_status)) +
  labs(x ="Status", y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  ggtitle("Hiring Status with G10 Percentage \n") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette = "Paired") #OKIE
### 解释:
# Students admitted in campus recruitment 
# generally have better performance in tenth grade education


# 4. twevle_sec_edu_per & hiring status
# Box Plot
graph15 <- campus_re_backup %>%
  ggplot(aes(y = twelve_sec_edu_per, fill = twelve_sec_edu_per)) +
  geom_boxplot(aes(fill = hiring_status)) +
  labs(x ="Status", y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  ggtitle("Hiring Status with G12 Percentage \n") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette = "Paired") #OKIE
### 解释:
# Students admitted in campus recruitment 
# also have better performance in twelveth grade education


# 5. ten_board_of_edu & hiring status
graph16 <- campus_re_backup %>% 
  group_by(ten_board_of_edu, hiring_status) %>% 
  tally() %>% 
  complete(hiring_status, fill = list(n = 0)) %>% 
  mutate(counting = n)

graph16 <- ggplot(graph16, aes(hiring_status, counting, fill = ten_board_of_edu)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x ="Status", y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  ggtitle("Hiring Status with BoE(G10) \n") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette="Paired")
### Conclusion:
# More students who study in seconardy education are from Central board


# 6. twelve_board_of_edu & hring status
# Bar plot
graph17 <- campus_re_backup %>% 
  group_by(twelve_board_of_edu, hiring_status) %>% 
  tally() %>% 
  complete(hiring_status, fill = list(n = 0)) %>% 
  mutate(counting = n)

graph17 <- ggplot(graph17, aes(hiring_status, counting, fill = twelve_board_of_edu)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x ="Status", y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  ggtitle("Hiring Status with BoE(G12) \n") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette="Paired") # OKIE
### 解释:
# More students who study in higher seconardy education are from Others board 


# 6. twelve_board_of_edu $ twelve_sed_edu_per
graph18 <- campus_re_backup %>%
  ggplot(aes(y = twelve_sec_edu_per, fill = twelve_sec_edu_per)) +
  geom_boxplot(aes(fill = twelve_board_of_edu)) +
  labs(x ="Status", y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  ggtitle("Grade Percentage with BoE \n") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette = "Paired") #OKIE
### Conclusion:
# In our sample, it is obvious that students studying in Central Boards have higher score
# compare to those who studying in Other Boards of Secondary Education.


# 7. specialize_sec_edu & hiring status
graph19 <- campus_re_backup %>% 
  group_by(specialize_sec_edu, hiring_status) %>% 
  tally() %>% 
  complete(hiring_status, fill = list(n = 0)) %>% 
  mutate(counting = n)

graph19 <- ggplot(graph19, aes(hiring_status, counting, fill = specialize_sec_edu)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x ="Status", y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  ggtitle("Hiring Status with G12 Specialization \n") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette = "Paired")
### Conclusion:
# More students who graduated from the commerce major have found jobs, 
# while only a few of the students majoring in arts are employed.


# 8. degree percentage & hiring status
graph20 <- campus_re_backup %>%
  ggplot(aes(y = degree_percent, fill = degree_percent)) +
  geom_boxplot(aes(fill = hiring_status)) +
  labs(x ="Status", y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  ggtitle("Hiring Status with Degree Percentage \n") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette = "Paired") #OKIE
### Conclusion: 
# The average degree percentage of the accepted students was nearly 6% higher
# than those students who were not hired by the company.


# 9. degree type field & hiring status
graph21 <- campus_re_backup %>% 
  group_by(degree_type_field, hiring_status) %>% 
  tally() %>% 
  complete(hiring_status, fill = list(n = 0)) %>% 
  mutate(counting = n)

graph21 <- ggplot(graph21, aes(hiring_status, counting, fill = degree_type_field)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x ="Status", y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  ggtitle("Hiring Status with Degree Type \n") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette = "Paired")

# 10. work_ex & hiring status
graph22 <- campus_re_backup %>% 
  group_by(work_ex, hiring_status) %>% 
  tally() %>% 
  complete(hiring_status, fill = list(n = 0)) %>% 
  mutate(counting = n)

graph22 <- ggplot(graph22, aes(hiring_status, counting, fill = work_ex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x ="Status", y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  ggtitle("Hiring Status with Work Experience \n") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette = "Paired")
### Conclusion:
# Although more students do not have work experience, 
# most of them are still employed by the company


# 11. entrence_test_per & hiring status
graph23 <- campus_re_backup %>%
  ggplot(aes(y = entrence_test_per, fill = entrence_test_per)) +
  geom_boxplot(aes(fill = hiring_status)) +
  labs(x ="Status", y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  ggtitle("Hiring Status with Entrence Test Persentage \n") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette = "Paired") #OKIE
### Conclusion:
# Although the hired students and the unemployed students have the same minimum scores, 
# the hired group has a higher average score on the entrance exam.


# 12. MBA specialize & hiring status
graph24 <- campus_re_backup %>% 
  group_by(MBA_specialize, hiring_status) %>% 
  tally() %>% 
  complete(hiring_status, fill = list(n = 0)) %>% 
  mutate(counting = n)

graph24 <- ggplot(graph24, aes(hiring_status, counting, fill = MBA_specialize)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x ="Status", y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  ggtitle("Hiring Status with MBA Specialization \n") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette = "Paired")

ggarrange(graph13, graph14, graph15,
          graph16, graph17, graph18,
          graph19, graph20, graph21,
          graph22, graph23, graph24, ncol = 3, nrow = 4)

##############################################################################

# Multivariate Exploration

# G10 grade, G12 grade, degree percent & salary (3D)
install.packages("plotly")
library(plotly)
# Point colors
marker <- list(color = ~salary, 
               colorscale = c('#FFE1A1', '#683531'), 
               showscale = TRUE)
# Create the plot
plot1 <- plot_ly(campus_re_backup, 
             x = ~ten_sec_edu_per, 
             y = ~twelve_sec_edu_per, 
             z = ~degree_percent, marker = marker) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'G10 Percentage'),
                      yaxis = list(title = 'G12 Percentage'),
                      zaxis = list(title = 'Degree Percent')),
         annotations = list(
           x = 1.15,
           y = 1.05,
           text = 'Salary \n',
           showarrow = FALSE
         ))
plot1

?%>%
  
### 解释:
# 发现被授予更高工资的学生，往往在十年级&十二年级&毕业成绩中具有优异表现
# 不排除有成绩普通但是被授予高工资的情况(eg: x = 60.8, y = 68.4, z = 64.6)
# 更具有普遍性&说服力的一堆数据出现在(x = 79, y = 76, z = 85)
# 这里也潜在说明xyz与hiring_status的关系
# 成绩高被雇佣的几率大

###########################################################################


# why placed
# why NOT placed


############################################################################


## Model Fitting 1
## WHY PLACED (lm)
# Trying to find "fitted model"
# model1

str(campus_re_backup)
campus_re_backup[campus_re_backup['hiring_status'] == "Placed",'hire'] = "1"
str(campus_re_backup)
# Convert chr to num
campus_re_backup$hire <- as.numeric(campus_re_backup$hire)
# Only keep "Placed" status
campus_re_backup <- campus_re_backup[complete.cases(campus_re_backup), ]
str(campus_re_backup)
# model1
model1 <- lm(hire ~ serial_number +
               gender +
               ten_sec_edu_per +
               ten_board_of_edu +
               twelve_sec_edu_per +
               twelve_board_of_edu +
               specialize_sec_edu +
               degree_percent +
               degree_type_field +
               work_ex +
               entrence_test_per +
               MBA_specialize +
               MBA_per +
               salary, data = campus_re_backup)
summary(model1)
## significant:
# degree_type_field
# twelve_sec_edu_per
# intercept

# plot
plot(model1, which = c(1, 2))


# Remove "MBA_per" (p = 0.90023)
# model2
model2 <- lm(hire ~ serial_number +
               gender +
               ten_sec_edu_per +
               ten_board_of_edu +
               twelve_sec_edu_per +
               twelve_board_of_edu +
               specialize_sec_edu +
               degree_percent +
               degree_type_field +
               work_ex +
               entrence_test_per +
               MBA_specialize +
               salary, data = campus_re_backup)
summary(model2)
## Significant:
# degree_type_field
# twelve_sec_edu_per
# intercept


# Remove "specialize_sec_edu" (p = 0.77851)
# model3
model3 <- lm(hire ~ serial_number +
               gender +
               ten_sec_edu_per +
               ten_board_of_edu +
               twelve_sec_edu_per +
               twelve_board_of_edu +
               degree_percent +
               degree_type_field +
               work_ex +
               entrence_test_per +
               MBA_specialize +
               salary, data = campus_re_backup)
summary(model3)
## Significant:
# degree_type_field
# twelve_sec_edu_per
# intercept


# Remove "twelve_board_of_edu" (p = 0.911418)
# model4
model4 <- lm(hire ~ serial_number +
               gender +
               ten_sec_edu_per +
               ten_board_of_edu +
               twelve_sec_edu_per +
               degree_percent +
               degree_type_field +
               work_ex +
               entrence_test_per +
               MBA_specialize +
               salary, data = campus_re_backup)
summary(model4)
## Significant:
# degree_type_field
# twelve_sec_edu_per
# intercept


# Remove "work_ex" (p = 0.730570)
# model5
model5 <- lm(hire ~ serial_number +
               gender +
               ten_sec_edu_per +
               ten_board_of_edu +
               twelve_sec_edu_per +
               degree_percent +
               degree_type_field +
               entrence_test_per +
               MBA_specialize +
               salary, data = campus_re_backup)
summary(model5)
## Significant:
# degree_type_field
# twelve_sec_edu_per
# intercept


# Remove "gender" (p = 0.641972)
# model6
model6 <- lm(hire ~ serial_number +
               ten_sec_edu_per +
               ten_board_of_edu +
               twelve_sec_edu_per +
               degree_percent +
               degree_type_field +
               entrence_test_per +
               MBA_specialize +
               salary, data = campus_re_backup)
summary(model6)
## Significant:
# degree_type_field
# twelve_sec_edu_per
# intercept
# degree_percent


# Remove "salary" (p = 0.669565)
# model7
model7 <- lm(hire ~ serial_number +
               ten_sec_edu_per +
               ten_board_of_edu +
               twelve_sec_edu_per +
               degree_percent +
               degree_type_field +
               entrence_test_per +
               MBA_specialize, data = campus_re_backup)
summary(model7)
## Significant:
# degree_type_field
# twelve_sec_edu_per
# intercept
# degree_percent


# Remove "MBA_specialize" (p = 0.224075)
# model8
model8 <- lm(hire ~ serial_number +
               ten_sec_edu_per +
               ten_board_of_edu +
               twelve_sec_edu_per +
               degree_percent +
               degree_type_field +
               entrence_test_per, data = campus_re_backup)
summary(model8)
## Significant:
# degree_type_field
# twelve_sec_edu_per
# intercept
# degree_percent


# Remove "ten_sec_edu_per" (p = 0.224693)
# model9
model9 <- lm(hire ~ serial_number +
               ten_board_of_edu +
               twelve_sec_edu_per +
               degree_percent +
               degree_type_field +
               entrence_test_per, data = campus_re_backup)
summary(model9)
## Significant:
# degree_type_field
# twelve_sec_edu_per
# intercept
# degree_percent
# entrence_test_per


# Remove "serial_number" (p = 0.21963)
# model10
model10 <- lm(hire ~ ten_board_of_edu +
                twelve_sec_edu_per +
                degree_percent +
                degree_type_field +
                entrence_test_per, data = campus_re_backup)
summary(model10)
## Significant:
# degree_type_field
# twelve_sec_edu_per
# intercept
# degree_percent
# entrence_test_per


# Remove "ten_board_of_edu" (p = 0.081713)
# model11
model11 <- lm(hire ~ twelve_sec_edu_per +
                degree_percent +
                degree_type_field +
                entrence_test_per, data = campus_re_backup)
summary(model11)
## Significant:
# degree_type_field
# twelve_sec_edu_per
# intercept
# degree_percent


# Remove "entrence_test_per" (p = 0.053881)
# model12
model12 <- lm(hire ~ twelve_sec_edu_per +
                degree_percent +
                degree_type_field, data = campus_re_backup)
summary(model12)
##### ALL SIGNIFICANT #####
# (degree_type_field中, others专业的学生不易被录用)

######################## Try add it back ###################

# fit13 = fit12 + ten_board_of_edu
model13 <- lm(hire ~ twelve_sec_edu_per +
                degree_percent +
                degree_type_field +
                ten_board_of_edu, data = campus_re_backup)
summary(model13)
## p = 0.13969 DROP (NO ten_board_of_edu)


# fit14 = fit12 + serial_number
model14 <- lm(hire ~ twelve_sec_edu_per +
                degree_percent +
                degree_type_field +
                serial_number, data = campus_re_backup)
summary(model14)
## p = 0.15005 DROP (NO serial_number)


# fit15 = fit12 + ten_sec_edu_per
model15 <- lm(hire ~ twelve_sec_edu_per +
                degree_percent +
                degree_type_field +
                ten_sec_edu_per, data = campus_re_backup)
summary(model15)
## p = 0.25463 DROP (NO ten_sec_edu_per)


# fit16 = fit12 + MBA_specialize
model16 <- lm(hire ~ twelve_sec_edu_per +
                degree_percent +
                degree_type_field +
                MBA_specialize, data = campus_re_backup)
summary(model16)
## p = 0.11591 DROP (NO MBA_specialize)


# fit17 = fit12 + salary
model17 <- lm(hire ~ twelve_sec_edu_per +
                degree_percent +
                degree_type_field +
                salary, data = campus_re_backup)
summary(model17)
## p = 0.36321 DROP (NO salary)


# fit18 = fit12 + gender 
model18 <- lm(hire ~ twelve_sec_edu_per +
                degree_percent +
                degree_type_field +
                gender, data = campus_re_backup)
summary(model18)
## p = 0.98047 DROP (NO gender)


# fit19 = fit12 + work_ex
model19 <- lm(hire ~ twelve_sec_edu_per +
                degree_percent +
                degree_type_field +
                work_ex, data = campus_re_backup)
summary(model19)
## p = 0.25554 DROP (NO work_ex)


# fit20 = fit12 + twelve_board_of_edu
model20 <- lm(hire ~ twelve_sec_edu_per +
                degree_percent +
                degree_type_field +
                twelve_board_of_edu, data = campus_re_backup)
summary(model20)
## p = 0.33794 DROP (NO twelve_board_of_edu)


# fit21 = fit12 + specialize_sec_edu
model21 <- lm(hire ~ twelve_sec_edu_per +
                degree_percent +
                degree_type_field +
                specialize_sec_edu, data = campus_re_backup)
summary(model21)
## p(Commerce) = 0.55925 DROP (NO specialize_sec_eduCommerce)
## p(Science) = 0.66068 DROP (NO specialize_sec_eduScience)


# fit22 = fit12 + MBA_per
model22 <- lm(hire ~ twelve_sec_edu_per +
                degree_percent +
                degree_type_field +
                MBA_per, data = campus_re_backup)
summary(model22)
## p = 0.38016 DROP (NO MBA_per)


#############################
######## FINAL MODEL ########
#############################
summary(model12)

# residual plot:
plot(model12, which = c(1, 2))


###############################################################


## Model Fitting 2
## SALARY (lm)
# Trying to find "fitted model"
# fit1
campus_fit1 <- lm(salary ~ serial_number +
                    gender +
                    ten_sec_edu_per +
                    ten_board_of_edu +
                    twelve_sec_edu_per +
                    twelve_board_of_edu +
                    specialize_sec_edu +
                    degree_percent +
                    degree_type_field +
                    work_ex +
                    entrence_test_per +
                    MBA_specialize +
                    MBA_per +
                    hiring_status, data = campus_re_backup2)
summary(campus_fit1)
## significant:
# hiring_statusPlaced
# MBA_per
# degree_type_fieldSci&Tech


# Remove "twelve_sec_edu_per" (p = 0.9334)
# fit2
campus_fit2 <- lm(salary ~ serial_number +
                    gender +
                    ten_sec_edu_per +
                    ten_board_of_edu +
                    twelve_board_of_edu +
                    specialize_sec_edu +
                    degree_percent +
                    degree_type_field +
                    work_ex +
                    entrence_test_per +
                    MBA_specialize +
                    MBA_per +
                    hiring_status,
                  data = campus_re_backup2)
summary(campus_fit2)
## significant:
# hiring_statusPlaced
# MBA_per
# degree_type_fieldSci&Tech


# Remove "ten_board_of_edu" (p = 0.8534)
# fit3
campus_fit3 <- lm(salary ~ serial_number +
                    gender +
                    ten_sec_edu_per +
                    twelve_board_of_edu +
                    specialize_sec_edu +
                    degree_percent +
                    degree_type_field +
                    work_ex +
                    entrence_test_per +
                    MBA_specialize +
                    MBA_per +
                    hiring_status,
                  data = campus_re_backup2)
summary(campus_fit3)
## significant:
# hiring_statusPlaced
# MBA_per
# degree_type_fieldSci&Tech


# Remove "serial_number" (p = 0.8079)
# fit4
campus_fit4 <- lm(salary ~ gender +
                    ten_sec_edu_per +
                    twelve_board_of_edu +
                    specialize_sec_edu +
                    degree_percent +
                    degree_type_field +
                    work_ex +
                    entrence_test_per +
                    MBA_specialize +
                    MBA_per +
                    hiring_status,
                  data = campus_re_backup2)
summary(campus_fit4)
## significant:
# hiring_statusPlaced
# MBA_per
# degree_type_fieldSci&Tech


# Remove "twelve_board_of_edu" (p = 0.5799)
# fit5
campus_fit5 <- lm(salary ~ gender +
                    ten_sec_edu_per +
                    specialize_sec_edu +
                    degree_percent +
                    degree_type_field +
                    work_ex +
                    entrence_test_per +
                    MBA_specialize +
                    MBA_per +
                    hiring_status,
                  data = campus_re_backup2)
summary(campus_fit5)
## significant:
# hiring_statusPlaced
# MBA_per
# degree_type_fieldSci&Tech


# Remove "specialize_sec_edu" (p = 0.4361)
# fit6
campus_fit6 <- lm(salary ~ gender +
                    ten_sec_edu_per +
                    degree_percent +
                    degree_type_field +
                    work_ex +
                    entrence_test_per +
                    MBA_specialize +
                    MBA_per +
                    hiring_status,
                  data = campus_re_backup2)
summary(campus_fit6)
## significant:
# hiring_statusPlaced
# MBA_per


# Remove "degree_type_field" (p = 0.8535)
# fit7
campus_fit7 <- lm(salary ~ gender +
                    ten_sec_edu_per +
                    degree_percent +
                    work_ex +
                    entrence_test_per +
                    MBA_specialize +
                    MBA_per +
                    hiring_status,
                  data = campus_re_backup2)
summary(campus_fit7)
## significant:
# hiring_statusPlaced
# MBA_per
# gender


# Remove "ten_sec_edu_per" (p = 0.6641)
# fit8
campus_fit8 <- lm(salary ~ gender +
                    degree_percent +
                    work_ex +
                    entrence_test_per +
                    MBA_specialize +
                    MBA_per +
                    hiring_status,
                  data = campus_re_backup2)
summary(campus_fit8)
## significant:
# hiring_statusPlaced
# MBA_per
# gender
# intercept


# Remove "MBA_specialize" (p = 0.3143)
# fit9
campus_fit9 <- lm(salary ~ gender +
                    degree_percent +
                    work_ex +
                    entrence_test_per +
                    MBA_per +
                    hiring_status,
                  data = campus_re_backup2)
summary(campus_fit9)
## significant:
# hiring_statusPlaced
# MBA_per
# gender
# intercept


# Remove "degree_percent" (p = 0.2994)
# fit10
campus_fit10 <- lm(salary ~ gender +
                    work_ex +
                    entrence_test_per +
                    MBA_per +
                    hiring_status,
                  data = campus_re_backup2)
summary(campus_fit10)
## significant:
# hiring_statusPlaced
# MBA_per
# gender
# intercept


# Remove "work_ex" (p = 0.21086)
# fit11
campus_fit11 <- lm(salary ~ gender +
                     entrence_test_per +
                     MBA_per +
                     hiring_status,
                   data = campus_re_backup2)
summary(campus_fit11)
## significant:
# hiring_statusPlaced
# MBA_per
# gender
# intercept


#Remove "entrence_test_per" (p = 0.1369)
# fit12
campus_fit12 <- lm(salary ~ gender +
                     MBA_per +
                     hiring_status,
                   data = campus_re_backup2)
summary(campus_fit12)
##### ALL SIGNIFICANT #####

######################## Try add it back ###################

# fit13 = fit12 + work_ex
campus_fit13 <- lm(salary ~ gender +
                     MBA_per +
                     hiring_status +
                     work_ex,
                   data = campus_re_backup2)
summary(campus_fit13)
## p = 0.22826 DROP (NO work_ex)


# fit14 = fit12 + degree_percent
campus_fit14 <- lm(salary ~ gender +
                     MBA_per +
                     hiring_status +
                     degree_percent,
                   data = campus_re_backup2)
summary(campus_fit14)
## p = 0.367 DROP (NO degree_percent)


# fit15 = fit12 + MBA_specialize
campus_fit15 <- lm(salary ~ gender +
                     MBA_per +
                     hiring_status +
                     MBA_specialize,
                   data = campus_re_backup2)
summary(campus_fit15)
## p = 0.19425 DROP (NO MBA_specialize)


# fit16 = fit12 + ten_sec_edu_per
campus_fit16 <- lm(salary ~ gender +
                     MBA_per +
                     hiring_status +
                     ten_sec_edu_per,
                   data = campus_re_backup2)
summary(campus_fit16)
## p = 0.62998 DROP (NO ten_sec_edu_per)


# fit17 = fit12 + degree_type_field
campus_fit17 <- lm(salary ~ gender +
                     MBA_per +
                     hiring_status +
                     degree_type_field,
                   data = campus_re_backup2)
summary(campus_fit17)
## p(Others) = 0.71860
## p(Sci&Tech) = 0.11596 DROP (NO degree_type_field)


# fit18 = fit12 + specialize_sec_edu
campus_fit18 <- lm(salary ~ gender +
                     MBA_per +
                     hiring_status +
                     specialize_sec_edu,
                   data = campus_re_backup2)
summary(campus_fit18)
## p(Commerce) = 0.48079
## p(Science) = 0.43263 DROP (NO specialize_sec_edu)


# fit19 = fit12 + twelve_board_of_edu
campus_fit19 <- lm(salary ~ gender +
                     MBA_per +
                     hiring_status +
                     twelve_board_of_edu,
                   data = campus_re_backup2)
summary(campus_fit19)
## p = 0.57598 DROP (NO twelve_board_of_edu)


# fit20 = fit12 + serial_number
campus_fit20 <- lm(salary ~ gender +
                     MBA_per +
                     hiring_status +
                     serial_number,
                   data = campus_re_backup2)
summary(campus_fit20)
## p = 0.63659 DROP (NO serial_number)


# fit21 = fit12 + ten_board_of_edu
campus_fit21 <- lm(salary ~ gender +
                     MBA_per +
                     hiring_status +
                     ten_board_of_edu,
                   data = campus_re_backup2)
summary(campus_fit21)
## p = 0.82053 DROP (NO ten_board_of_edu)


# fit22 = fit12 + twelve_sec_edu_per
campus_fit22 <- lm(salary ~ gender +
                     MBA_per +
                     hiring_status +
                     twelve_sec_edu_per,
                   data = campus_re_backup2)
summary(campus_fit22)
## p = 0.97686 DROP (NO twelve_sec_edu_per)


#############################
######## FINAL MODEL ########
#############################
summary(campus_fit12)
plot(campus_fit1, c(1, 2))
plot(campus_fit12, c(1, 2))

###########################################################################


## Spatial Modelling
library(gstat)
library(lattice)
library(sp)

# Structure of campus_re_backup
str(campus_re_backup)

# Exploring Stationarity
# Based on model 12
# hire ~ twelve_sec_edu_per + degree_percent + degree_type_field
# NOW,
# A.
# hiring_status & degree_percent, GIVEN degree_type_field
coplot(hiring_status ~ degree_percent | degree_type_field,
             data = campus_re_backup)
# hiring_status & degree_type_field, GIVEN degree_percent
coplot(hiring_status ~ degree_type_field | degree_percent,
             data = campus_re_backup)

# B.
# hiring_status & twelve_sec_edu_per, GIVEN degree_type_field
coplot(hiring_status ~ twelve_sec_edu_per | degree_type_field,
             data = campus_re_backup)
# hiring_status & degree_type_field, GIVEN twelve_sec_edu_per
coplot(hiring_status ~ degree_type_field | twelve_sec_edu_per,
             data = campus_re_backup)

# C.
# hiring_status & twelve_sec_edu_per, GIVEN degree_percent
coplot(hiring_status ~ twelve_sec_edu_per | degree_percent,
             data = campus_re_backup)
# hiring_status & degree_percent, GIVEN twelve_sec_edu_per
coplot(hiring_status ~ degree_percent | twelve_sec_edu_per,
             data = campus_re_backup)


##################################

# Exploring Stationarity
# Based on campus_fit12
# salary ~ gender + MBA_per + hiring_status
# NOW,
# A.
# salary & gender, GIVEN MBA_per
coplot(salary ~ gender | MBA_per,
       data = campus_re_backup)

# B.
# salary & MBA_per, GIVEN gender
coplot(salary ~ MBA_per | gender,
       data = campus_re_backup)

######################################################################

######## Cumulative distribution function of salary ########
cdf_hiring_status <- ecdf(campus_re_backup$salary)
plot(cdf_hiring_status, verticals = T, do.points = F)

## QQ-Plot of salary
qqnorm(campus_re_backup$salary)
qqline(campus_re_backup$salary)

