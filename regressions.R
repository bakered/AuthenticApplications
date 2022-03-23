
#install.packages("insight")
sessionInfo()
Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "C")

library(arsenal)
library(knitr)
library(survival)
library(coin)
library(miceadds)
library(stargazer)
library(dplyr)
require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)
library(lmtest)
library(sandwich)
library(plm)
library(multiwayvcov)
library(lfe)
library(GGally)
library(ggpubr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tikzDevice)

options("tikzLatex"='C:/Users/fo06mafa/AppData/Local/Programs/MiKTeX 2.9/miktex/bin/x64/pdflatex.exe') 
results = readRDS("C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\Data_amalgamate\\results.RDS")
player_table = readRDS("C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\Data_amalgamate\\player_table.RDS")
type. = "latex"

formula_vars = c(
  "experience_years",
  "education_yt" ,
  "skills_1.2", 
  "interests_1.2",
  "Gender",
  "Order",
  "skill_level", 
  "Teamwork", "Time.Management", "Microsoft.Office", "Administration", "Customer.Service",
  "age", "Female", "ethnicity",
  "highest_education2", "marital_status2","employment_status2", 
  "lib_con", "Twitter", "Instagram", "Facebook", "Snapchat", "LinkedIn", 
  "TikTok", "stayintouch", "toomuchtime", "potentialemployers", 
  "professional", "lostControl", "advertisers", "employers", "workExperienceInc", 
  "workExperienceIncText.x", "educationInc", "educationIncText.x", 
  "health", "healthText.x", "crime", "crimeText.x", "discrimination", 
  "discriminationText.x", "anythingElse", "anythingElseText.x", 
  "teams", "communicate", "administration", "customerService", 
  "ITskills", "experience", "education", "hobbies", "CVcontents", 
  "SMcontents",
  "SM_active", "SM_approval", "privacy_no_problem", "graduate", 
  "TrumpShare", "TrumpSharebin", "mental_health_dayIndex", "TrumpShare2", 
  "TrumpShare3", "state2", "mental_health_dayIndex2", "DMA", "metro", 
  "rural_urban_con", "urban_influence", "economy_type", "stateage", 
  "random", "random2", "random3", "time_startednum"
)
y = c(
  #"CV_score.std"
  "CV_score"
) 
colnames(results)

  
scaletab = function(LM_tab, scale_ind = T, scale = T, index){ #LM_tab = LM_tab[LM_tab$Treated1,]
  if(scale){
    if(scale_ind){
      LM_tab = subset(LM_tab, index)
    }
    for(v in formula_vars){
      if(is.numeric(LM_tab[[v]])){
        LM_tab[[v]] = scale(LM_tab[[v]])[,1]
      } else if(is.factor(LM_tab[[v]])){
        LM_tab[[v]] = factor(LM_tab[[v]], ordered = F)
      }
    }
    if(!scale_ind){
      LM_tab = subset(LM_tab, index)
    }
  }
  if(!scale){
    LM_tab = subset(LM_tab, index)
  }
  return(LM_tab)
}


############################ LINEAR MODELS ############################
formula_vars = c(
  "experience_years",
  "education_yt" ,
  "skills_1.2", 
  "interests_1.2",
  "endorsements",
  "Female",
  "gap",
  "gap_lie",
  "mental_health",
  "no_data",
  "bad"
)
cluster_on = "mturkID" #"state" #
for_table<-gsub("_"," ", formula_vars)

formula =  paste(formula_vars, collapse = "+") %>%  paste("|0|0|", cluster_on) %>% paste(y,"~", ., sep="") %>% as.formula()

LM_tab = scaletab(results, index=rep(T,nrow(results)))
fitmodp = felm(keepX=TRUE, formula = formula, data = LM_tab)
summary(fitmodp)

LM_tab = results
fitmodp2 = felm(keepX=TRUE, formula = formula, data = LM_tab)
summary(fitmodp2)

LM_tab = scaletab(results, index=results$workExperienceInc==1)
fitmod.WI = felm(keepX=TRUE, formula = formula, data = LM_tab)
summary(fitmod.WI)

LM_tab = scaletab(results, index=results$health ==1)
fitmod.H = felm(keepX=TRUE, formula = formula, data = LM_tab)
summary(fitmod.H)

LM_tab = scaletab(results, index= (results$SM_active > quantile(results$SM_active, 0.9)))
fitmod = felm(keepX=TRUE, formula = formula, data = LM_tab)
summary(fitmod)
LM_tab = scaletab(results, index= (results$SMcontents < quantile(results$SMcontents, 0.3)))
fitmod = felm(keepX=TRUE, formula = formula, data = LM_tab)
summary(fitmod)
LM_tab = scaletab(results, index= (results$min_length_viewed < quantile(results$min_length_viewed, 0.3)))
fitmod = felm(keepX=TRUE, formula = formula, data = LM_tab)
summary(fitmod)
LM_tab = scaletab(results, index= (results$workExperienceIncLength > quantile(results$workExperienceIncLength, 0.9)))
fitmod = felm(keepX=TRUE, formula = formula, data = LM_tab)
summary(fitmod)

formula_vars = c(
  "experience_years",
  "education_yt" ,
  "skills_1.2", 
  "interests_1.2",
  "endorsements",
  "Female"
)
cluster_on = "mturkID" #"state" #
for_table<-gsub("_"," ", formula_vars)
formula =  paste(formula_vars, collapse = "+") %>%  paste("|0|0|", cluster_on) %>% paste(y,"~", ., sep="") %>% as.formula()

LM_tab = scaletab(results, index=(results$no_data))
fitmod = felm(keepX=TRUE, formula = formula, data = LM_tab)
summary(fitmod)



################## tables and graphs print ######################

pl = c(#"(Intercept)" = "Intercept", 
  "experience_years" = "CV Experience$_i$", 
  "skills_1.2" = "CV Skills$_i$", 
  "interests_1.2" = "CV Interests$_i$", 
  "education_yt" = "CV Education$_i$", 
  "Female" = "Female$_i$", 
  "Order" = "Order$_i$",
  "skill_level" = "SM Skills$_i$",
  "gapTRUE" = "Gap$_i$",
  "gap_lieTRUE"= "Lie about Gap$_i$"     ,
  "mental_healthTRUE"= "Mantal Health$_i$" ,
  "no_dataTRUE"= "No SM$_i$"      ,
  "badTRUE" = "Bad SM$_i$"
)

modellist = factor(c("Full"))

cov.names = na.omit(pl[match(names(coef(fitmodp))[!grepl("Intercept", names(coef(fitmodp)))], names(pl))])
linear_fitmodp_table  = stargazer(fitmodp,
                                      title="Linear Model", 
                                      type=type.,
                                      align=T,
                                      dep.var.labels=c(gsub("_"," ", y)),
                                      column.labels = c("Linear Model"),
                                      label = "linear_model_paper_table",
                                      omit.stat = c("f",  "ser", "AIC"),
                                      covariate.labels = cov.names,
                                      table.placement = "htb"
                                      #omit.stat = c("f"),
                                      # omit = c("highest_education", "marital_status", "employment_status", "state"),
                                      #omit.labels = gsub("_", " ", c("highest_education", "marital_status", "employment_status", "state"))
)

linear_fitmodp_plot = plot_models(fitmodp,
                                  m.labels = modellist, 
                                  axis.labels = pl,
                                  legend.title = "Treatment") +
  geom_hline(yintercept=0, colour = "red", linetype="dashed") +
  labs(title="Linear Models by Treatment Group", x="Coefficient", y="Estimate") +
  theme_minimal() +
  theme(#text = element_text(size=25),
    #axis.text.x=element_blank(),
    #axis.ticks.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
#linear_fitmodp_plot
#tikz(file = "Paper/linear_fitmodp_plot.tex",  width = tikw, height = tikh)
#linear_model_treat_plot
#dev.off()


############# tableby ################
form_pre = c("experience_years",
             "education_yt" ,
             "skills_1.2", 
             "interests_1.2",
             "Order")
formula = form_pre %>% paste(collapse = "+") %>% paste("treatment", "~", .) %>% as.formula() 
balance_table = tableby(formula, results, control = tableby.control(numeric.test = "kwt"))
tab1 = as.data.frame(summary(balance_table))

formula = "applicant.gender" %>% paste("treatment", "~", .) %>% as.formula() 
balance_table = tableby(formula, subset(results, !no_data), control = tableby.control(numeric.test = "kwt"))
tab2 = as.data.frame(summary(balance_table))
tab2 = cbind(tab2[,1:5], c("", "n/a", ""), tab2[6:8])
colnames(tab2) = colnames(tab1)
rbind(tab1, tab2) %>% kable()              


