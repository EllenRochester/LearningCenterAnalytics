########## Load and clean data ########## 

# deleting all objects from the memory
rm(list=ls()); 
# garbage collection - releasing memory when an object is no longer used
gc(); 

# load the data
setwd('/Users/wangzifei/Desktop/Case 2') 
evalsData = read.csv('evals_experiment.csv')
studData = read.csv('students_experiment.csv')

# merge two datasets by studentID
mergedData <- merge(x = evalsData, y = studData, by = "student_id")


# clean the data
summary(mergedData)
colSums(is.na(mergedData))

# check whether every student has both intake and post-program score
library(dplyr)

filteredData <- mergedData %>%
    group_by(student_id) %>%
    filter(n() == 2)


########## First, have a look at the intake score distribution of different district.##########

# Because different level of student may have different improvement under same program.
# However, if we want to run the regression,
# We have to make sure there is randomization with the data.

# Sum the score
library(dplyr)

filteredData <- filteredData %>% mutate(sumScore = score_reading + 
                                                score_writing + score_mathNoCalc +
                                                score_mathCalc)

sumStuScore <- filteredData[,c(1,2,3,9,10)]


# subset the intake score from datasets
intScore <- sumStuScore[sumStuScore$program == "intake",]


# group the student from different district
intMeanByDist <- intScore %>%
    group_by(district) %>%
    summarise(scoreAverage = mean(sumScore))

# make a graph

library(ggplot2)

ggplot(intMeanByDist, aes(x = district, y = scoreAverage, fill = scoreAverage)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Scores by District", x = "District", y = "Score", fill = "scoreAverage")

colnames(mergedData)

# "score_reading"   
# "score_writing"    
# "score_mathNoCalc" 
# "score_mathCalc"   
# "district"        

readingScoreFit = lm(score_reading ~ district, data = filteredData)
writingScoreFit = lm(score_writing ~ district, data = filteredData)
mathNoCalcScoreFit = lm(score_mathNoCalc ~ district, data = filteredData)
mathCalcScoreFit = lm(score_mathCalc ~ district, data = filteredData)

summary(readingScoreFit)
summary(writingScoreFit)
summary(mathNoCalcScoreFit)
summary(mathCalcScoreFit)


# can see from regression, 
# there is statistically significant in score with different district
# Thus, there is no randomization with the data.

########## Second, because there is no randomization, we use diff-diff method ##########
# y = B0 + B1*D1 + B2*D2 + B3*D1D2 + e
# – First Diff (D1): Test (Treatment) vs. Control -- A/B/C/Control
# – Second Diff (D2): Before vs. After Treatment -- Intake/Post-program score

# make intake/post-program dummy variable
filteredData <- filteredData %>%
    mutate(isPostProgram = ifelse(program == unique(program)[2], 1, 0))


# divide district into educational program A/B/C/Current
filteredData <- filteredData %>%
    mutate(eduProgram = case_when(
        district == "St. Paul" ~ "Current",
        district == "Beaverton" ~ "Current",
        district == "Lake Oswego" ~ "A",
        district == "Ridgefield" ~ "A",
        district == "Camas" ~ "B",
        district == "Sherwood" ~ "B",
        district == "Riverdale" ~ "C",
        district == "Wilsonville" ~ "C",
    ))


# see the impr of 4 skills

# imprReading
filteredData <- filteredData %>%
    arrange(student_id, date) %>% 
    group_by(student_id) %>% 
    mutate(imprReading = (score_reading - lag(score_reading)) / lag(score_reading) * 100) 

# imprWriting
filteredData <- filteredData %>%
    arrange(student_id, date) %>% 
    group_by(student_id) %>% 
    mutate(imprWriting = (score_writing - lag(score_writing)) / lag(score_writing) * 100) 

#imprmathNoCalc
filteredData <- filteredData %>%
    arrange(student_id, date) %>% 
    group_by(student_id) %>% 
    mutate(imprMathNoCalc = (score_mathNoCalc - lag(score_mathNoCalc)) / lag(score_mathNoCalc) * 100) 

#imprMathCalc
filteredData <- filteredData %>%
    arrange(student_id, date) %>% 
    group_by(student_id) %>% 
    mutate(imprMathCalc = (score_mathCalc - lag(score_mathCalc)) / lag(score_mathCalc) * 100) 


imprScore <- filteredData[,c(1,3,9,12,13,14,15,16)]

# subset the intake score from datasets
imprScore <- imprScore[imprScore$program == "skills",]



# subset the program and mean of skills improvment
# calculate the average 

imprScoreReading <- imprScore %>%
    group_by(eduProgram) %>%
    summarise(averImprReading = mean(imprReading))

readingImpr = lm(imprReading ~ eduProgram, data = imprScore)
summary(readingImpr)

imprScoreWriting <- imprScore %>%
    group_by(eduProgram) %>%
    summarise(averImprWriting = mean(imprWriting))

writingImpr = lm(imprWriting ~ eduProgram, data = imprScore)
summary(writingImpr)

imprMathNoCalc <- imprScore %>%
    group_by(eduProgram) %>%
    summarise(averImprMathNoCalc = mean(imprMathNoCalc))

mathNoCalcImpr = lm(imprMathNoCalc ~ eduProgram, data = imprScore)
summary(mathNoCalcImpr)

imprMathCalc <- imprScore %>%
    group_by(eduProgram) %>%
    summarise(averImprMathCalc = mean(imprMathCalc))

mathCalcImpr = lm(imprMathCalc ~ eduProgram, data = imprScore)
summary(mathCalcImpr)


write.csv(imprScoreReading, "imprScoreReading.csv", row.names = FALSE)
write.csv(imprScoreWriting, "imprScoreWriting.csv", row.names = FALSE)
write.csv(imprMathNoCalc, "imprMathNoCalc.csv", row.names = FALSE)
write.csv(imprMathCalc, "imprMathCalc.csv", row.names = FALSE)

getwd()

##### drop #####

readingFit = lm(score_reading ~ isPostProgram*eduProgram, data = filteredData)
summary(readingFit)

writingFit = lm(score_writing ~ isPostProgram*eduProgram, data = filteredData)
summary(writingFit)

mathNoCalcFit = lm(score_mathNoCalc ~ isPostProgram*eduProgram, data = filteredData)
summary(mathNoCalcFit)

mathCalcFit = lm(score_mathCalc ~ isPostProgram*eduProgram, data = filteredData)
summary(mathCalcFit)

mathCalcFit = lm(score_mathCalc ~ isPostProgram + eduProgram, data = filteredData)
summary(mathCalcFit)





