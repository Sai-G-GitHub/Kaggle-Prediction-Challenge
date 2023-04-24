data <- read.csv('HireTrainApr10.csv')

colors<- c('red','blue','cyan','yellow','green')

summary(data)
colnames(data)
nrow(data)
unique(data$Coding)
unique(data$Impression)
unique(data$Major)
unique(data$College)
table(data$Coding, data$Hired)
table(data$Impression, data$Hired)
table(data$Major, data$Hired)
table(data$College, data$Hired)


mosaicplot(data$Coding~data$Hired,xlab='Coding', ylab='Hired', main='Coding vs Hired', col=colors)
mosaicplot(data$Impression~data$Hired,xlab='Impression', ylab='Hired', main='Hired vs Impression',col=colors)
mosaicplot(data$Major~data$Hired,xlab='Major', ylab='Hired', main='Hired vs Major',col=colors)
mosaicplot(data$College~data$Hired,xlab='Hired', ylab='College', main='Hired vs College',col=colors)


myprediction <- data
train<-sample(1:nrow(data))
train[1:5]
Hired2<-data[train, ]
trainset<-Hired2[(nrow(Hired2)-1000):nrow(HiredScrambled), ]
myprediction<-trainset

mypredidction<-trainset
decision<- rep('No', nrow(mypredidction))
decision[myprediction$Coding == "Excellent"] <- 'Yes'
decision[myprediction$Coding == "OK"] <- 'Yes'
decision[myprediction$Coding == "OK" &myprediction$Impression=='Nerdy'&myprediction$College=='BestCollege'] <- 'No'
decision[myprediction$Coding == "OK" &myprediction$Impression=='Shy'&myprediction$College=='BestCollege'] <- 'No'
decision[myprediction$Coding == "OK" &myprediction$Impression=='Shy'&myprediction$College=='RedBrick'] <- 'No'
decision[myprediction$Coding == "Weak" &myprediction$Impression=='Shy'&myprediction$College=='RedBrick'] <- 'Yes'
decision[myprediction$Coding == "Weak" &myprediction$Impression=='Nerdy'&myprediction$College=='RedBrick'] <- 'Yes'
decision[myprediction$Coding == "Excellent" &myprediction$Major=='IT'&myprediction$Impression=='Nerdy'&myprediction$College=='BestCollege'] <- 'No'
decision[myprediction$Coding == "Excellent" &myprediction$Major=='IT'&myprediction$Impression=='Shy'&myprediction$College=='BestCollege'] <- 'No'
myprediction$Hired<-decision
error <- mean(trainset$Hired!= myprediction$Hired)
error


test <- read.csv("test_challenge1.csv")
submission <- read.csv("sample_submission_challenge1.csv")
myprediction<-test
decision <- rep('No', nrow (myprediction))
decision[myprediction$Coding == "Excellent"] <- 'Yes'
decision[myprediction$Coding == "OK"] <- 'Yes'
decision[myprediction$Coding == "OK" &myprediction$Impression=='Nerdy'&myprediction$College=='BestCollege'] <- 'No'
decision[myprediction$Coding == "OK" &myprediction$Impression=='Shy'&myprediction$College=='BestCollege'] <- 'No'
decision[myprediction$Coding == "OK" &myprediction$Impression=='Shy'&myprediction$College=='RedBrick'] <- 'No'
decision[myprediction$Coding == "Weak" &myprediction$Impression=='Shy'&myprediction$College=='RedBrick'] <- 'Yes'
decision[myprediction$Coding == "Weak" &myprediction$Impression=='Nerdy'&myprediction$College=='RedBrick'] <- 'Yes'
decision[myprediction$Coding == "Excellent" &myprediction$Major=='IT'&myprediction$Impression=='Nerdy'&myprediction$College=='BestCollege'] <- 'No'
decision[myprediction$Coding == "Excellent" &myprediction$Major=='IT'&myprediction$Impression=='Shy'&myprediction$College=='BestCollege'] <- 'No'
submission$Prediction <- decision
write.csv(submission, 'submission2.csv', row.names = FALSE)
View(submission)
