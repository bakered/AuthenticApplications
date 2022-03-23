library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(reshape)
library(tikzDevice)
library(plyr)
library(dplyr)
library(gtable)
library(grid)
library(gridExtra)
library(cowplot)
library(caroline)
library(ggpmisc)
library(tidyr)
library(plotly)
library(tidyquant)

rm(list=ls())
#install.packages("ggpmisc")
Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "C")
setwd("C:\\Users\\fo06mafa\\Documents\\AuthenticApplications")

options("tikzLatex"='C:/Users/fo06mafa/AppData/Local/Programs/MiKTeX 2.9/miktex/bin/x64/pdflatex.exe') 
results = readRDS("C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\Data_amalgamate\\results.RDS")
player_table = readRDS("C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\Data_amalgamate\\player_table.RDS")


bys.numeric <- c("Teamwork", "Time.Management", "Microsoft.Office", "Administration", "Customer.Service",
                 #"the_change", 
                 "no_times_viewed", "length_viewed", 
                 "notelength", "total_time", 
                 #"deletednotelength", 
                 "feedback_length", 
                 "age", 
                 "SM_active",
                # "lib_con", "Twitter", "Instagram", "Facebook", "Snapchat", "LinkedIn","TikTok",
                 "SM_approval",
                # "stayintouch", "toomuchtime", "potentialemployers","professional", 
                 "privacy_no_problem",
               #  "lostControl", "advertisers", "employers",  #privacy
               #  "teams", "communicate", "administration", "customerService", "ITskills", 
               #  "experience", "education", "hobbies", "CVcontents", 
                 "SMcontents", "CV_SM_diff", 
                 "TimeSpentInstruc", "time_started", 
                 "time_on_email", 
                 "lat2", "long2",    
                 "TrumpShare", "mental_health_dayIndex", 
                 "TrumpShare2", "TrumpShare3", "mental_health_dayIndex2", 
                 "time_startednum", "mean_no_times_viewed", "mean_notelength", 
                 "session.size",
               "random", "random2", "random3"
                 )


bys.ordered <- c("experience_years", "education_yt", 
                 "skills_1.2", 
                 "interests_1.2",
                 "Order",
                 "skill_level", 
                 "Teamwork", "Time.Management", "Microsoft.Office", "Administration", "Customer.Service",
                 "the_change", "no_times_viewed",
                 "changed_mind",
                 "SM_active",
                 "lib_con", "Twitter", "Instagram", "Facebook", "Snapchat", "LinkedIn","TikTok",
                 "SM_approval",
                 "stayintouch", "toomuchtime", "potentialemployers","professional", 
                 "privacy_no_problem",
                 "lostControl", "advertisers", "employers",  #privacy
                 "teams", "communicate", "administration", "customerService", "ITskills", 
                 "experience", "education", "hobbies", "CVcontents", "SMcontents", "CV_SM_diff", 
                 "highest_education2", "marital_status2", "employment_status2",
                 "rural_urban_con", "urban_influence", "economy_type",
                 "random", "random2", "random3")

bys.unordered <- c("highest_education", "marital_status", "employment_status",
                   "applicant.gender", 
                   "participant.gender",  
                   "workExperienceInc", "educationInc", "health", "crime", "discrimination", "anythingElse", 
                   "graduate")

vars = c("CV_score",
         "notelength",
         "length_viewed")

cbp <- c(a="#999999", mh="#E69F00", g="#56B4E9", c="#009E73", nd="#F0E442", gl="#0072B2", b="#D55E00", "#CC79A7")


#data=results; measurevar="CV_score"; groupvars=c("treatment");na.rm=T; conf.interval=.95; .drop=TRUE
summarySE <- function(data=results, measurevar="CV_score", groupvars=c("treatment"), na.rm=T,
                      conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  #datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
#column=results$time_started;  number=6
bin_function = function(column, number=3){ #column=results$mean_notelength
  breaks = quantile(column, na.rm = T, probs = seq(0,1, length.out=number+1)) 
  breaks = unique(breaks)
  #while(breaks[1] == breaks[2]){
  #  breaks = breaks[-1]
  #}
  tags <- vector(length=(length(breaks)-1))
  if((class(breaks) == "POSIXct")[1]){
    breakschar = strftime(breaks, format="%H:%M:%S")
    for(b in 2:length(breaks)){ #b=2
      tags[b-1] = paste0("[", breakschar[b-1], "-", breakschar[b], ")")
    }
  } else {
    for(b in 2:length(breaks)){ #b=2
      #tags[b-1] = paste0(gsub("0.",".", round(breaks[b-1], 2)), "-", gsub("0.",".", round(breaks[b],2)))
      tags[b-1] = paste0("[", round(breaks[b-1], 2), "-", round(breaks[b],2), ")")
      
    }
  }
  #if(strsplit(tags[1], split = "-")[2] %in% c("",".")){
  #  tags[1] = paste0("0", tags[1])
  #}
  group_tags <- cut(column, 
                    breaks=breaks, 
                    include.lowest=TRUE, 
                    right=FALSE, 
                    labels=tags)
  out <- factor(group_tags, 
                levels = tags,
                ordered = TRUE)
  return(out)
}

for (i in dev.list()[1]:dev.list()[length(dev.list())]) {
  dev.off()
}

levels(results$treatment)

#var="CV_score"; y.label = "CV score"; by="experience_years"; y.positions=NA;  facet=NA; refgroup = "4"; x.label=""
#var="CV_score"; y.label = "CV score"; by="treatment"; y.positions=NA;  facet="experience_years"; refgroup="control"; x.label=""
#var="CV_score"; y.label = "CV score"; by="workExperienceInc"; y.positions=NA;  df=subset(results); facet="treatment"; refgroup = "0"; x.label=""
boxplot = function(var="CV_score", by="experience_years", df=results, facet=NA, refgroup=".all.", y.label = "CV score", x.label="", y.positions=NA){
  df[[by]] = factor(df[[by]], ordered=F)
  formula = as.formula(paste(var, "~", by))
  control.level = max(aggregate(formula, df, FUN = function(x){min(quantile(x, .75) + 0*IQR(x), max(x))})[,2])*1.1
  if(is.na(facet)){groupvar = NULL} else {groupvar = facet}
  stat.test <- compare_means(formula, data = df, method = "wilcox.test", ref.group = refgroup, group.by = groupvar,
                             symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                                symbols = c("****", "***", "**", "*", "ns")))
  
  if(is.na(y.positions)){stat.test$y.position = rep(control.level, nrow(stat.test))} else {stat.test$y.position = y.positions}
  stat.test$p.signif = gsub("ns", "", stat.test$p.signif)
  #stat.test$p.signif <- paste0(stat.test$p.signif, "*")
  #stat.test <- stat.test[c(1,4),]
  p1 = ggplot(df, aes_string(x=by, y=var, fill="treatment", group=by)) + 
    geom_boxplot(outlier.shape = NA, coef = 0) +
    stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="black", fill="black") +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0) +
    theme_minimal() 
  p1 = p1 + stat_pvalue_manual(data = stat.test, 
                               inherit.aes = FALSE,
                               label = "p.signif",
                               position = "identity",
                               remove.bracket = T,
                               #  hide.ns = T,
                               tip.length = tip.compar) # + lims(y=c(0,200)) 
  if(!is.na(facet)){p1 = p1 + facet_grid(as.formula(paste(".~", facet)), switch="both")}
  p1 = p1 + coord_cartesian(ylim=c(0,(control.level)))
  if("treatment" %in% c(by, facet)){
    p1 = p1 + scale_fill_manual(values = c("control" = unname(cbp["c"]),
                                                                           "gap" = unname(cbp["g"]),
                                                                           "gap_lie" = unname(cbp["gl"]),
                                                                           "mental_health" = unname(cbp["mh"]),
                                                                           "no_data" = unname(cbp["nd"]),
                                                                           "bad" = unname(cbp["b"]))) +
      theme(legend.position = "top",
            text = element_text(size=20),
            axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank()) + labs(fill="", y=y.label, x=x.label)
    if("treatment" %in% c(by)){p1 = p1 + theme(axis.text.x=element_blank())} else {p1 = p1 + theme(strip.text = element_blank())}
  } else {
    p1 = p1 + scale_fill_manual(values=c(rep(unname(cbp["a"]), length(unique(df[[by]]))))) +  
      theme(legend.position = "none",
            text = element_text(size=20),
            axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank()) + labs(fill="", y=y.label, x=by) 
  }
   
  return(p1)
}

boxplot(var="CV_score", y.label = "CV score", by="experience_years", y.positions=NA,  facet=NA, refgroup = "4")
boxplot(var="CV_score", y.label = "CV score", by="treatment", y.positions=NA,  facet="experience_years", refgroup = "control")
boxplot(var="CV_score", y.label = "CV score", by="education_yt", y.positions=NA,  facet=NA, refgroup = "1")
boxplot(var="CV_score", y.label = "CV score", by="treatment", y.positions=NA,  facet="education_yt", refgroup = "control")

boxplot(var="CV_score", y.label = "CV score", by="treatment", y.positions=NA,  df=subset(results, gap|control), facet="workExperienceInc", refgroup = "control")
boxplot(var="CV_score", y.label = "CV score", by="treatment", y.positions=NA,  df=subset(results), facet="workExperienceInc", refgroup = "control")

boxplot(var="CV_score", y.label = "CV score", by="workExperienceInc", y.positions=NA,  df=subset(results, gap|control), facet="treatment", refgroup = "0")
boxplot(var="CV_score", y.label = "CV score", by="workExperienceInc", y.positions=NA,  df=subset(results), facet="treatment", refgroup = "0")

boxplot(var="CV_score", y.label = "CV score", by="health", y.positions=NA,  df=subset(results), facet="treatment", refgroup = "0")
boxplot(var="CV_score", y.label = "CV score", by="treatment", y.positions=NA,  df=subset(results, mental_health|control), facet="health", refgroup = "control")
boxplot(var="CV_score", y.label = "CV score", by="health", y.positions=NA,  df=subset(results, mental_health|control), facet="treatment", refgroup = "0", x.label = "Noticed 'Health Problems'")

boxplot(var="CV_score", y.label = "CV score", by="SMcontents", y.positions=NA,  df=subset(results), facet="treatment", refgroup = "1")


boxplot(var="CV_score", y.label = "CV score", by="treatment", y.positions=NA,  df=subset(results, mental_health|control), facet="health", refgroup = "control")
boxplot(var="CV_score", y.label = "CV score", by="health", y.positions=NA,  df=subset(results, mental_health), facet=NA, refgroup = "0")


df. = results %>% cbind(subgroup=c(results$workExperienceIncLength != 0))
boxplot(var="CV_score", y.label = "CV score", by="treatment", y.positions=NA,  df=df., facet="subgroup", refgroup = "gap_lie")

df. = results$SM_active %>% bin_function(number = 7) %>% cbind(subgroup=. , results)
boxplot(var="CV_score", y.label = "CV score", x.label = "SM_active", by="subgroup", y.positions=NA,  df=df., facet=NA, refgroup = ".all.")




boxplot(var="notelength", y.label = "Note Length Characters", by="treatment", y.positions=NA,  facet=NA)
p1 = boxplot(var="notelength", y.label = "Note Length Characters", by="treatment", y.positions=NA,  facet=NA)
p2 = boxplot(var="length_viewed", y.label = "Minutes on CV", by="treatment", y.positions=NA,  facet=NA)
p3 = boxplot(var="no_times_viewed", y.label = "Times Clicked on CV", by="treatment", y.positions=NA,  facet=NA)

legend = get_legend(p1 + theme(legend.position = "top"))
plots = cowplot::plot_grid(
  p1 + theme(legend.position = "none"),  
  p2 + theme(legend.position = "none"), 
  #  p3 + theme(legend.position = "none"),
  nrow = 1)

cowplot::plot_grid(legend, plots, ncol = 1, rel_heights = c(0.2, 2))


##### points with confidence intervals
#variable="CV_score"; by = bys.ordered[2]; group="treatment"; df = results; include.count=T; with.line = T
#variable="CV_score"; by = "experience_years"; group=NA; df = results; include.count=T; with.line = T
score.means.by.var.line <- function(variable, by, group, df, include.count=F, x.label=NA, with.line = T){
  df = df %>% group_by_(.dots = list(by)) %>% dplyr::filter(n()>6*15) %>% ungroup()
  df = df[!is.na(df[[by]]),]
  form =  as.formula(paste(variable, "~", group))
  df[[by]] = as.factor(df[[by]])
  df = subset(df, !is.na(df[[variable]]))
  ref = levels(df[[by]])[1]
  results.sum <- summarySE(df, measurevar=variable, groupvars=c(by, group))
  dftemp = df
  dftemp[[by]] = factor(dftemp[[by]], ordered = F)
  try(stat.test <- compare_means(form,  data = dftemp, group.by = c(by), method = "wilcox.test", ref.group = ".all.")) #".all."))
  table = df %>% group_by_(.dots = list(by, group)) %>% dplyr::summarise(y.position=mean(!!sym(variable)))#    mean_ci(!!sym(variable))$ymax)
  colnames(table)[2:3] = c("group2", "y.position")
  stat.test = join(stat.test, table)
  colnames(stat.test)[which(colnames(stat.test) == "max")] = "y.position"
  #y.posi = rep(seq(max(table$max)*1.1, max(table$max), length = table(stat.test[,1])[1]),nrow(unique(stat.test[,1])))
  #try(stat.test <- mutate(stat.test, y.position= y.posi))
  pd <- position_dodge(0.2) # move them x to the left and right
  
  ylow = max(0, min(results.sum$mean-results.sum$ci, na.rm=T)*0.95)
  yhigh = min(10, max(results.sum$mean+results.sum$ci, na.rm=T)*1.05)
  p1 = ggplot(data = results.sum, aes_string(x=by, y="mean", group=group, colour=group)) + 
    geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, position=pd) +
    geom_point(position=pd) 
  if(with.line){p1 = p1 + geom_line(position=pd)}
  p1 = p1 +  geom_hline(yintercept = mean(df[[variable]], na.rm=T)) 
  if(is.numeric(results.sum[[by]])){p1 = p1 + lims(x=c(min(results.sum[[by]], na.rm=T)-0.5, max(results.sum[[by]], na.rm=T)+0.5))} 
  p1 = p1 + stat_pvalue_manual(data = stat.test,
                               inherit.aes = FALSE,
                               x = by,
                               label = "p.signif",
                              # position = pd,
                               position = "identity",
                               remove.bracket = T) 
  p1 = p1 + scale_fill_manual(values = c("control" = unname(cbp["c"]),
                                         "gap" = unname(cbp["g"]),
                                         "gap_lie" = unname(cbp["gl"]),
                                         "mental_health" = unname(cbp["mh"]),
                                         "no_data" = unname(cbp["nd"]),
                                         "bad" = unname(cbp["b"])))
  
  if(include.count){
    p1 = p1 +
      labs(y=variable, x=NULL) +
      theme_minimal() +
      theme(legend.title = element_blank(),
            legend.position = "top",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    p2 = ggplot(data = df, aes_string(x=by)) +
      geom_bar() +
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank())
    plot = cowplot::plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(4/4, 1/4))
  } else {
    p1 = p1 +
      labs(y=variable, x=by) +
      theme_minimal() +
      theme(legend.title = element_blank(),
            legend.position = "top",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    plot = p1
  }
  return(plot)
}


score.means.by.var.line(variable="CV_score", by = bys.ordered[1], group="treatment", df = results, include.count = T)


##### lines of best fit
#variable="CV_score";  by="no_times_viewed"; group="treatment"; df=results; include.count=T; x.label="Contents of the candidate's social media accounts (are important)"
score.means.by.var.line2 <- function(variable, by, group, df, include.count=T, x.label=NA){
  df = df[!df[[by]] %in% boxplot.stats(df[[by]])$out,]
  p1 = ggplot(df , aes_string(x=by, y=variable, group=group, colour=group)) + 
    #geom_point(position="jitter") +
    #geom_ma(ma_fun = SMA, n = 20, linetype = 1, size = 1, na.rm = TRUE) +
    geom_smooth(method=lm) +  # method=lm loess
    #lims(x=c(0,10)) +
    #geom_hline(yintercept = 0)+
    #facet_grid(treatment~.) +
    labs(y=variable, x=NULL) +
    theme_classic()
  p1 = p1 +  geom_hline(yintercept = mean(df[[variable]], na.rm=T)) 
  p1 = p1 + scale_colour_manual(values = c("control" = unname(cbp["c"]),
                                         "gap" = unname(cbp["g"]),
                                         "gap_lie" = unname(cbp["gl"]),
                                         "mental_health" = unname(cbp["mh"]),
                                         "no_data" = unname(cbp["nd"]),
                                         "bad" = unname(cbp["b"])))
  if(include.count){
    p1 = p1 + theme_minimal() +
      theme(legend.title = element_blank(),
            legend.position = "top",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    p2 = ggplot(data = df, aes_string(x=by)) +
      geom_bar() +
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank())
    if(!is.na(x.label)){p2 = p2 + xlab(x.label)}
    plot = cowplot::plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(4/4, 1/4))
  } else {
    p1 = p1 + theme_minimal() +
      theme(legend.title = element_blank(),
            legend.position = "top",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    plot = p1
  }
  return(plot)
}

score.means.by.var.line2("CV_score", "SMcontents","treatment",  results, include.count=T, x.label="Contents of the candidate's social media accounts (are important)")
score.means.by.var.line2("CV_score", "experience","CV.experience.factor",  results, include.count=T, x.label="Candidate's Experience (is important)")
score.means.by.var.line2("CV_score", "CVcontents","CV.experience.factor",  results, include.count=T, x.label="Contents of the candidate's CV (is) important)")
score.means.by.var.line2("CV_score", "education","education_yt.factor",  results, include.count=T, x.label="Candidate's Education (is important)")



score.means.by.var.line2("CV_score", "SM_active","treatment",  subset(results, control|mental_health|no_data), include.count=T, x.label="SM Activity")
score.means.by.var.line2("CV_score", "age","treatment",  subset(results, control|mental_health|no_data), include.count=T, x.label="Age")

score.means.by.var.line2("CV_score", "teams", "Teamwork",  results, include.count=T)
score.means.by.var.line2("CV_score", "Teamwork", "communicate", results, include.count=T)
score.means.by.var.line2("CV_score","ITskills", "Microsoft.Office",  results, include.count=T)
score.means.by.var.line2("CV_score", "administration","Administration",  results, include.count=T)
score.means.by.var.line2("CV_score", "customerService","Customer.Service",  results, include.count=T)


score.means.by.var.line("CV_score", "endorsements","skill_level",  results, include.count=T)

score.means.by.var.line2("CV_score", "experience", "experience_years",  results, include.count=T)
score.means.by.var.line2("CV_score", "education", "education_yt",  results, include.count=T)

score.means.by.var.line2("CV_score", "no_times_viewed", "experience_years",  results, include.count=T)
score.means.by.var.line2("CV_score", "length_viewed", "experience_years",  results, include.count=T)
score.means.by.var.line2("CV_score", "notelength", "experience_years",  results, include.count=T)

results$education_yt
score.means.by.var.line2("CV_score",  "CV_SM_diff", "treatment", results, include.count=T)


pdf("Paper/plots.pdf")
for(b in bys.ordered[1:6]){ #b="experience_years"
  print(paste("by",  b, sep=" "))
  plot = try(boxplot(var="CV_score", y.label = "CV score", x.label = b, by="treatment", facet = b, y.positions=NA, refgroup="control"))
  print(plot)
  plot = try(boxplot(var="CV_score", y.label = "CV score", by=b, y.positions=NA,  df=results, facet="treatment", refgroup = ".all."))
  print(plot)
}
for(b in bys.ordered[-c(1:6)]){ # b= bys.ordered[7]
  print(paste("by",  b, sep=" "))
  df. = results[[b]] %>% bin_function(number = 5) %>% cbind(subgroup=. , results)
  plot = try(boxplot(var="CV_score", y.label = "CV score", x.label = b, by="treatment", y.positions=NA,  df=df., facet="subgroup", refgroup = "control"))
  print(plot)
  plot = try(boxplot(var="CV_score", y.label = "CV score", x.label = b, by="subgroup", y.positions=NA,  df=df., facet="treatment", refgroup = levels(df.$subgroup)[1]))
  print(plot)
}
for(b in bys.numeric){ #b=bys1[1]
  print(paste("by",  b, sep=" "))
  df. = results[[b]] %>% bin_function(number = 5) %>% cbind(subgroup=. , results)
  plot = try(boxplot(var="CV_score", y.label = "CV score", x.label = b, by="treatment", y.positions=NA,  df=df., facet="subgroup", refgroup = "control"))
  print(plot)
  plot = try(boxplot(var="CV_score", y.label = "CV score", x.label = b, by="subgroup", y.positions=NA,  df=df., facet="treatment", refgroup = levels(df.$subgroup)[1]))
  print(plot)
  plot = try(score.means.by.var.line2("CV_score",  b, "treatment", results, include.count=T))
  print(plot)
}
for(b in bys.unordered){
  print(paste("by",  b, sep=" "))
  plot = try(boxplot(var="CV_score", y.label = "CV score", x.label = b, by="treatment", facet = b, y.positions=NA, refgroup="control"))
  print(plot)
  plot = try(boxplot(var="CV_score", y.label = "CV score", x.label = b, by=b, facet = "treatment", y.positions=NA, refgroup=levels(df.[[b]])[1]))
  print(plot)
}
dev.off()

pdf("Paper/plots.pdf")
for(b in bys.ordered[1:6]){
  print(paste("by",  b, sep=" "))
  plot = try(score.means.by.var.line(variable="CV_score", "treatment", by = b, df = results, include.count = F))
  print(plot)
}
for(b in bys.ordered[-c(1:6)]){
  print(paste("by",  b, sep=" "))
  plot = try(score.means.by.var.line(variable="CV_score", "treatment", by = b, df = results, include.count = T))
  print(plot)
}
for(b in bys.numeric){ #b=bys1[1]
  print(paste("by",  b, sep=" "))
  plot = try(score.means.by.var.line2("CV_score",  b, "treatment", results, include.count=T))
  print(plot)
}
for(b in bys.unordered){
  print(paste("by",  b, sep=" "))
  plot = try(score.means.by.var.line(variable="CV_score", by = b, group="treatment", df = results, include.count = T, with.line = F))
  print(plot)
}
dev.off()


ggplot(results , aes_string(x="skill_level", y="CV_score", group="treatment")) + 
  #geom_point(position="jitter") +
  geom_smooth(method=lm, aes(colour=treatment))+
  #geom_hline(yintercept = 0)+
  #facet_grid(treatment~.) +
  labs(y="CV_score", x=NULL) +
  theme_classic()
colnames(results)


