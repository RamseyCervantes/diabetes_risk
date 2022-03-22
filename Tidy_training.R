library(tidyverse)
library(emmeans)
library(ggplot2)
library(readr)
library(haven)
library(purrr)
library(readxl)
temp1 = list.files(pattern="*.dta")
list2env(lapply(setNames(temp1, make.names(gsub("*.dta$","", temp1))), read_dta), envir = .GlobalEnv)
organize <- function(unorg_data){
  org_data <- unorg_data %>%
    pivot_longer(cols=c(-estimates),names_to="race") %>%
    pivot_wider(names_from=c(estimates))
  org_data <- select(org_data, race, b, pvalue, ll, ul)
  return(org_data)
}
male_diabetes <- organize(diabetes_s1)
female_diabetes <- organize(diabetes_s2)
male_heartatk <- organize(heartatk_s1)
female_heartatk <- organize(heartatk_s2)
data <- bind_rows(male_diabetes, female_diabetes, male_heartatk, female_heartatk)
data <- data %>%
  relocate(race, .after=type) %>%
  mutate(type=rep(c("Diabetes","Heart Attack"),each=6)) %>%
  mutate(sex=rep(rep(c("Male","Female"),each=3),2))
data_diabetes <- filter(data, type=="Diabetes")
data_heartatk <- filter(data, type=="Heart Attack")
plot_diabetes <- data_diabetes %>%
  ggplot(aes(x=race, y=b,colour=sex,group=sex)) +
  geom_pointrange(aes(x=race, y=b, ymin=ll, ymax=ul),position=position_dodge(0.3),size=1.05)+
  labs(y="Marginal Probabilities", x="Race",title="Risk of Diabetes",colour='', shape='')+theme_bw()
plot_heartatk <- data_heartatk %>%
  ggplot(aes(x=race, y=b,colour=sex,group=sex)) +
  geom_pointrange(aes(x=race, y=b, ymin=ll, ymax=ul),position=position_dodge(0.3),size=1.05)+
  labs(y="Marginal Probabilities", x="Race",title="Risk of Heart Attack",colour='', shape='')+theme_bw()
model_diabetes_lm <- lm(b~race, data_diabetes)
model_diabetes_glm <- glm(b~race, gaussian, data_diabetes)
model_heartatk_lm <- lm(b~race, data_heartatk)
model_heartatk_glm <- glm(b~race, gaussian, data_heartatk)
diabetes_est <- emmeans(model_diabetes, spec="race")
summary(diabetes_est)
m_tukey <- contrast(diabetes_est, method="pairwise")
summary(m_tukey)
confint(m_tukey)
summary(model_diabetes_lm)
summary(model_diabetes_glm)
plot(plot_diabetes)
plot(plot_heartatk)