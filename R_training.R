library(ggplot2)
data <- read.csv("Table1_margins.csv")
data <- lapply(data, function(x) gsub("=","",x))
data <- data.frame(data)
n <-c('b','se','t','pvalue','ll','ul','df','crit','eform',rep(c('x','b','se','t','pvalue','ll','ul','df','crit','eform'),3))
data <- t(data[,-1])[,c(1:39)]
colnames(data) <- n
rownames(data) <- NULL
data <- data[,c(1,4,5,6,11,14,15,16,21,24,25,26,31,34,35,36)]
male_diabetes <- as.data.frame(data[,c(1:4)])
female_diabetes <- as.data.frame(data[,c(5:8)])
male_heartatk <- as.data.frame(data[,c(9:12)])
female_heartatk <- as.data.frame(data[,c(13:16)])
data <- rbind(male_diabetes, female_diabetes, male_heartatk, female_heartatk) 
data$race <- rep(c("White","Black","Other"),4)
data$type <- rep(c("Diabetes","Heart Attack"),each=6)
data$sex <- rep(rep(c("Male","Female"),each=3),2)
data[,c(1,2,3,4)] <- sapply(data[,c(1,2,3,4)], as.numeric)
data_diabetes <- data[1:6,]
data_heartatk <- data[7:12,]
plot_diabetes <- ggplot(data_diabetes, aes(x=race, y=b,colour=sex,group=sex)) +
  geom_pointrange(aes(x=race, y=b, ymin=ll, ymax=ul),position=position_dodge(0.3),size=1.05)+
  labs(y="Marginal Probabilities", x="Race",title="Risk of Diabetes",colour='', shape='')+theme_bw()
plot_heartatk <- ggplot(data_heartatk, aes(x=race, y=b,colour=sex,group=sex)) +
  geom_pointrange(aes(x=race, y=b, ymin=ll, ymax=ul),position=position_dodge(0.3),size=1.05)+
  labs(y="Marginal Probabilities", x="Race",title="Risk of Heart Attack",colour='', shape='')+theme_bw()
plot(plot_diabetes)
plot(plot_heartatk)
