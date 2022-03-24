#reading in Stata dataset nhanes2: National Health and Nutrition Examination Survey
nhanes2 <- read_dta("https://www.stata-press.com/data/r17/nhanes2.dta")
#setting categorical variables
for(category in c("race","sex","diabetes","heartatk")){
  eval(parse(text=paste0("nhanes2$", category, "<- as.factor(nhanes2$", category, ")")))
}
#assigning covariates
cov1 <- c("race","age","weight","height","sex")
#creating formulas
organize <- function(risk){
  #creating formulas
  Formula <- paste(cov1, collapse="+")
  Formula_risk <- formula(paste(risk,Formula, sep=""))
  #running regression
  model_risk <- glm(Formula_risk,binomial,nhanes2)
  #getting margins
  risk_est <- emmeans(model_risk, specs = "race","sex")
  risk_est <- risk_est %>%
    #getting probabilites
    regrid() %>%
    #renaming cells for comprehensibility
    as_tibble() %>%
    mutate(race=rep(rep(c("White","Black","Other"),1),2)) %>%
    mutate(sex=rep(rep(c("Male","Female"),each=3),1))
  return(risk_est)
}
diab_est <- organize("diabetes~")
heart_est <- organize("heartatk~")
#plotting
plot_diabetes <- diab_est %>%
  ggplot(aes(x=race, y=prob,colour=sex,group=sex)) +
  geom_pointrange(aes(x=race, y=prob, ymin=asymp.LCL, ymax=asymp.UCL),position=position_dodge(0.3),size=1.05)+
  labs(y="Marginal Probabilities", x="Race",title="Risk of Diabetes",colour='', shape='')+theme_bw()
plot_heartatk <- heart_est %>%
  ggplot(aes(x=race, y=prob,colour=sex,group=sex)) +
  geom_pointrange(aes(x=race, y=prob, ymin=asymp.LCL, ymax=asymp.UCL),position=position_dodge(0.3),size=1.05)+
  labs(y="Marginal Probabilities", x="Race",title="Risk of Heart Attack",colour='', shape='')+theme_bw()
plot_diabetes
plot_heartatk