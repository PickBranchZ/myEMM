library(emmeans)
data(pigs)

m1 <- lm(conc~source + percent, pigs)
emmeans(m1, 'percent')
emmeans(m1, 'source')
m2 <- lm(conc~source + factor(percent), pigs)
emmeans(m2, 'percent')
emmeans(m2, 'source')

ref_grid(m1)

library('NHANES')
##read data
data(NHANES)
var = c('BMI', 'Poverty', 'Age', 'Gender',  'Race1', 'Work', 'Education', 'BPSysAve', 'DirectChol')
mydata <- subset(NHANES, select = var)
mynames <- c('BMI', 'Income', 'Age', 'Gender',  'Race', 'Work', 'Edu', 'BPSys', 'DirectChol')
names(mydata) <- mynames

##remove pregnant data and Poverty=5 data
mydata = mydata[-which(NHANES$PregnantNow=='Yes'|NHANES$Poverty==5), ]

##transform categorical data
mydata$Work <- as.character(mydata$Work)
for (i in 1:nrow(mydata)){
  if (is.na(mydata$Work[i])==FALSE){
    if (mydata$Work[i] == 'Looking'){
      mydata$Work[i] = 0
    } else if (mydata$Work[i] == 'NotWorking'){
      mydata$Work[i] = 0
    } else if (mydata$Work[i] == 'Working'){
      mydata$Work[i] = 1
    }
  } else {
    mydata$Work[i] = NaN
  }
}
mydata$Work <- as.numeric(mydata$Work)


mydata$NumEdu <- 0
for (i in 1:nrow(mydata)){
  if (is.na(mydata$Edu[i])==FALSE){
    if (as.character(mydata$Edu[i]) == 'High School'){
      mydata$NumEdu[i] = 1
    } else if (as.character(mydata$Edu[i]) == 'Some College'){
      mydata$NumEdu[i] = 2
    } else if (as.character(mydata$Edu[i]) == 'College Grad'){
      mydata$NumEdu[i] = 3
    }
  } else {
    mydata$Work[i] = NaN
  }
}

mydata$Gender = ifelse(mydata$Gender=='female', 1, 0)

tmp=na.omit(mydata)
dim(tmp)

m3 <- lm(BMI~Income+Gender+BPSys+DirectChol, tmp)
summary(m3)

ref_grid(m3)
emmeans(m3, 'Gender')


x=data.frame(matrix(c(2.2873, 0, 121.39, 1.3358, 2.2873, 1, 121.39, 1.3358), byrow = TRUE,  ncol = 4))
names(x) <- c('Income','Gender','BPSys','DirectChol')
predict(m3, x, se.fit = TRUE, interval = 'confidence')

m4 <- lm(BMI~Income+factor(Gender)+factor(Work)+factor(NumEdu), tmp)
summary(m4)
ref_grid(m4)
emmeans(m4, 'Gender')
x=matrix(rep(0, 32),  ncol = 4)
x[, 1]=2.2873
x[, 2]=0
x[, 3]=rep(c(0,1), each=4)
x[, 4]=rep(c(0, 1, 2, 3), 2)
x <- data.frame(x)
names(x) <- c('Income','Gender','Work','NumEdu')
myPre <- predict(m4, x, se.fit = TRUE, interval = 'confidence')

m5 <- lm(BMI~Income+factor(Gender)+factor(Work)+factor(NumEdu)+I(Income^2), tmp)
ref_grid(m5)
emmeans(m5, 'Gender')
