setwd("C:/Users/Nathaniel/Desktop/Datathon")
# import jobs and subset to largest economies
jobs <- read.csv("jobs.csv")
names(jobs)
cali <- jobs[jobs$state=="CA",]
write.csv(cali,file="cali.csv")
state3 = jobs[jobs$state %in% c("TX","NY","CA"),]
dim(state3)
# attach real estate by id
zillow <- read.csv("real_estate.csv")[]

zillow3 = zillow[zillow$state %in% c("TX","NY","CA"),]
save(zillow3,state3,file="statezillow.RData")

load("statezillow.RData")
# trim variables - too big
dim(statezillow)

statehome <- merge(x=state3,y=zillow3,by=c("city","state"))
save(statehome,file="statehome.RData")
load("statehome.RData")
dim(statehome)
names(statehome)

statekeep <- statehome[,c(1,2,7,12)]
rm(statehome)
save(statekeep,file="statekeep.RData")
# import and merge education
education <- read.csv("education.csv")
education <- education[education$state %in% c("TX","NY","CA"),1:26]
weights <- c(0,0,0,1:12,12,12,13,13,14,16,18,20,22)
sumedu <- apply(education[,3:26],1,sum)
wted <- apply(education[,3:26],1, function(i) sum(i*weights))
aved <- wted/sumedu

educ <- cbind(education[1:2],aved)

mean(aved[education$state=="CA"])
stateeduc <- merge(statekeep,educ,by=c("city","state"))

save(stateeduc,file="stateeduc")

statehome2 <- merge(x=statehome,y=education,by=c("city","state"),all.x=TRUE)
save(statehome2,file="statehome2.RData")
dim(statehome2)

# iport and attach geographic identifiers
load("statehome2.RData")
# subset statehome2 to fewer timepoints to work
statehome3 <- statehome2[,c(1:14,192:251)]
save(statehome3,file="statehome3.RData")

load("statehome3.RData")
geographic <- read.csv("geographic.csv")
jobedugeo <- merge(x=stateeduc,y=geographic,by=c("city","state"))
save(jobedugeo,file="jobeducgeo.RData")

# add real estate date
zillow <- read.csv("real_estate.csv")

zillow3 = zillow[zillow$state %in% c("TX","NY","CA"),]
zillowtrim = zillow3[c(2,3,182:241)]
names(zillowtrim)
install.packages("reshape")
library(reshape)

names(zillowtrim)
melted <- melt(zillowtrim,id=c("city","state")
)
head(melted)
names(melted)[3] <- "date"
names(melted)[4] <- "homevalue"

jobedugeo$city <- as.character(jobedugeo$cit)

length(table(jobedugeo$city))
174*60
ca3 <- jobedugeo[jobedugeo$state=="CA",]

names(ca3)
head(ca3$created_date)
ca3$created_date <- as.character(ca3$created_date)
ca3$date <- as_date(ca3$created_date)
table(ca3$state)
class(ca3$date)
install.packages("lubridate")
library(lubridate)

names(melted)
head(melted$date)
melted$date2 <- substr(melted$date,2,8)
melted$date3 <- paste(melted$date2,"01",sep=".")

melted$date4 <- ymd(melted$date3)

save(melted,file="melted.RData")
save(ca3,file="ca3.RData")

names(ca3)
ca3$datem <- as.character(ca3$date)
ca3$datem <- substr(ca3$datem,1,7)
ca3$datem2 <- paste(ca3$datem,"01",sep="-")
ca3$datem3 <- as_date(ca3$datem2)



?as_date
all.data <- merge(jobedugeo,melted,by=c("city","state"))
save(melted,file="melted.RData")

load("statehome4.RData")
cali <- statehome4[statehome4$state=="CA",]
texas <- statehome4[statehome4$state=="TX",]
ny <- statehome4[statehome4$state=="NY",]

write.csv(cali,file="cali.csv")
write.csv(texas,file="texas.csv")
write.csv(ny,file="ny.csv")

save(cali,texas,ny,file="state3.RData")


pdf("test.pdf")
plot(statehome4$latitude,statehome4$longitude)
dev.off()

names(ca3)
ca3$date <- ca3$datem3
# merge by date categories

save(ca3,file="ca3.RData")
save(melted,file="melted.RData")

ca3$date <- as.character(ca3$date)
melted$date <- as.character(melted$date4)

caall <- merge(ca3,melted,by=c("city","state","date"))

save(caall,file="california.RData")
caall2 <- caall[,c(1,2,3,5,6,7,8,12)]
write.csv(caall2,file="analysis.csv")

names(caall)

names(ca3)
melted(date)
head(ca3$date)
head(melted$date4)
names(melted)

setwd("C:/Users/Nathaniel/Desktop/Datathon")
jobs <- read.csv("analysis.csv")

jobs$i <- 1   # counting variable

jobn<-aggregate(jobs$i,by=list(jobs$city,jobs$state,jobs$date),sum)
names(jobn) <-setwd("C:/Users/Nathaniel/Desktop/Datathon")
jobs <- read.csv("analysis.csv")

jobs$i <- 1   # counting variable

jobn<-aggregate(jobs$i,by=list(jobs$city,jobs$state,jobs$date),sum)
names(jobn) <- c("city","state","date","njobs")

job <- merge(jobs,jobn,by=c("city","state","date"))
table(job$njobs)

write.csv(job,file="job.csv")



c("city","state","date","njobs")

job <- merge(jobs,jobn,by=c("city","state","date"))
table(job$njobs)

names(job)
hist(job$njobs)
head(jobn)

names(jobs)
glm(homevalue ~ 
      
      head(jobs)  
    
    
    setwd("C:/Users/Nathaniel/Desktop/Datathon")
    job <- read.csv("job.csv")
    
    names(job)
    head(job)
    
    table(job$njobs)
    
    model1 <- glm(njobs~homevalue,data=job)
    
    
    education <- read.csv("education.csv")  
    names(education)
    years <- apply(education[,3:26],1,sum)
    pop <- cbind(education[,1:2],years)
    
    jobpop <- merge(job,pop,by=c("city"))
    jobpop
    names(jobpop)
    jobpop <- jobpop[,c(-2,-5,-11)]
    jobpop <- jobpop[,-10]
    write.csv(jobpop,file="jobpop.csv")
    
    
    #### attempt 2
    jobpop$pop <- jobpop$years
    jobspercapit = jobpop$njobs / jobpop$years
    hundredthousand = jobpop$homevalue/100000
    
    
    names(jobpop)
    model1 = glm( jobspercapit ~ hundredthousand,data=jobpop)
    install.packages("stargazer")
    library(stargazer)
    stargazer(model1,out="out.html")
    
    ?stargazer
    
    names(jobpop)
    model2 = glm( jobspercapit ~ hundredthousand + aved + ,data=jobpop)
    stargazer(model2,out="out2.html")
    
    pdf("map.pdf")
    
    pdf("hist.pdf")
    hist(jobspercapit)
    dev.off()
    
    
    install.packages("dynlm")
    library(dynlm)
    
    dynlm(jobspercapit ~ hundredthousand + hundredthousand[,)
    
    install.packages("xts")
    require(xts)
    ts <- xts(jobpop, order.by=as.POSIXct(jobpop$date))
    
    ts$jobspercapit <- as.numeric(ts$njobs) / as.numeric(ts$years)
    
    model3 <- dynlm(ts$njobs ~ ts$homevalue)
    summary(model3)
    
    names(jobpop)
    attach(jobpop)
    
    
    write.csv(table(category_name),file="table.csv")
    par(mar=c(3,3,3,3))
    pdf("map1.pdf")
    
    plot(jobpop$homevalue,jobpop$njobs)
    
    plot(jobpop$longitude,jobpop$latitude,col=grey(jobpop$aved/25))
    dev.off()
    
    
    
    
    



