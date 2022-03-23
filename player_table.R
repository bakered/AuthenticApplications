Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "C")
setwd("C:\\Users\\fo06mafa\\Documents\\AuthenticApplications")
library(dplyr)
library(lme4)


checkpayment <- data.frame(results$myID, 
                           results$ID, 
                           results$mturkID, 
                           results$finished,
                           results$ethnicity,
                           results$feedback,
                           results$no_times_viewed,
                           results$length_viewed,
                           results$total_time
)
colnames(checkpayment) <- c("myID", "ID", "mturkID", "finished", 
                           "ethnicity", "feedback", "no_times_viewed", 
                            "length_viewed", "total_time")

tocol = ncol(checkpayment)
### agregate this by different functions
temp1 = aggregate(by=list(checkpayment$mturkID), x = checkpayment[,c((tocol-3):tocol)], FUN = function(x) min(x))
colnames(temp1)[(ncol(temp1)-3):ncol(temp1)] = paste0(colnames(temp1)[(ncol(temp1)-3):ncol(temp1)],"_min")
temp2 = aggregate(by=list(checkpayment$mturkID), x = checkpayment[,c((tocol-2):tocol)], FUN = function(x) median(x))
colnames(temp2)[(ncol(temp2)-2):ncol(temp2)] = paste0(colnames(temp2)[(ncol(temp2)-2):ncol(temp2)],"_median")
checkpayment2 = dplyr::left_join(temp1, temp2, by = c("Group.1"))

########## PLAYER TABLE ################

### add in a drop out bit ?

dput(colnames(results))

ag_cols<- which(colnames(results) %in% c("CV_score", "changed_mind", "no_times_viewed", "length_viewed", 
                                         "the_change", "notelength", "deletednotelength", "experience_years", "education_yt")) 

survcols<- which(colnames(results) %in% c("feedback", "feedback_length", "age", "participant.gender", "ethnicity", 
                                          "highest_education", "marital_status", "employment_status", "state", "state2",
                                          "lib_con", 
                                          "Twitter", "Instagram", "Facebook", "Snapchat", "LinkedIn", 
                                          "TikTok", "stayintouch", "toomuchtime", "potentialemployers", 
                                          "professional", "lostControl", "advertisers", "employers", 
                                          "workExperienceInc", "workExperienceIncText", "educationInc", "educationIncText", "health", "healthText", 
                                          "crime", "crimeText", "discrimination", "discriminationText", "anythingElse", "anythingElseText", 
                                          "workExperienceIncLength", "educationIncLength", "healthLength", 
                                          "crimeLength", "discriminationLength", "anythingElseLength",
                                          "teams", "communicate", "administration", "customerService", "ITskills", 
                                          "experience", "education", "hobbies", "CVcontents", "SMcontents", "CV_SM_diff",
                                          "ip", "lat", "long",
                                          "myID", "finished", 
                                          "SM_active", "SM_approval", "privacy_no_problem", "graduate", 
                                          "TrumpShare", "TrumpSharebin", "mental_health_dayIndex", "TrumpShare2", 
                                          "TrumpShare3", "state2", "mental_health_dayIndex2",
                                          "total_time", "TimeSpentInstruc", "mean_no_times_viewed", "mean_notelength", 
                                          "median_length_viewed", "min_length_viewed", "time_startednum", 
                                          "random", "random2", "random3", 
                                          "time_started","date_started", "date_started2", "date_started3", "email", "failed", "failed_bec", "time_on_email", "number_ips", "number_locs", "mturk_worker_id", "set",
                                          "final_page", "ip2", "lat2", "long2", "city", "identity", "freq", "freqID", "graduate", "session.size",
                                          "metro", "rural_urban_con", "urban_influence", "economy_type",
                                          "highest_education2", 
                                          "marital_status2", "employment_status2", "interests_1.2", "skills_1.2"))



player_table = aggregate(results[,ag_cols], list(results$mturkID), mean)
colnames(player_table)[1] <- "mturkID"
dim(player_table)
length(unique(player_table$mturkID))

surv = aggregate(results[,survcols], list(results$mturkID), FUN = function(x) x[1])
colnames(surv)[1] <- "mturkID"
player_table = merge(player_table, surv, by="mturkID", all = T)


for(treat in as.character(levels(results$treatment))){ #treat = as.character(levels(results$treatment))[1]
  colnum = ncol(player_table)+1
  player_table = merge(player_table, subset(results, treatment==treat, select = c("CV_score.std", "mturkID")), by="mturkID", all=T)
  colnames(player_table)[colnum] = treat
}




saveRDS(player_table, "C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\Data_amalgamate\\player_table.RDS")
#saveRDS(player_table, "C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\Data_amalgamate\\player_table.by.score.RDS")  


dput(colnames(player_table))

cols = c("experience_years", "education_yt", "CV_score", 
         "changed_mind", "the_change", "no_times_viewed", "length_viewed", 
         "notelength", "deletednotelength", "total_time", 
         "feedback_length", "age", "participant.gender",  
         "highest_education2", "marital_status2", "employment_status2",  
         "lib_con", "Twitter", "Instagram", "Facebook", "Snapchat", "LinkedIn", 
         "TikTok", "stayintouch", "toomuchtime", "potentialemployers", 
         "professional", "lostControl", "advertisers", "employers", "workExperienceInc", 
         "educationInc",
         "health", "crime", "discrimination", 
         "anythingElse", "teams", 
         "communicate", "administration", "customerService", "ITskills", 
         "experience", "education", "hobbies", "CVcontents", "SMcontents", 
         "TimeSpentInstruc", "time_started", 
         "time_on_email", 
         "lat2", "long2", "SM_active", "SM_approval", "privacy_no_problem", 
         "graduate", "TrumpShare", "mental_health_dayIndex", 
         "TrumpShare2", "TrumpShare3", "mental_health_dayIndex2", 
         "rural_urban_con", "urban_influence", "economy_type", 
         "random", "random2", "random3", 
         "time_startednum", "mean_no_times_viewed", "mean_notelength", 
         "session.size",
         "control", "gap", "gap_lie", "mental_health", "no_data", "bad")

#cols[!(cols %in% colnames(player_table))]
colssec = match(cols, colnames(player_table))
tempdf = subset(player_table, select = colssec)
#glimpse(tempdf)

tempdf$feedback_length[is.na(tempdf$feedback_length)] = 0
tempdf$participant.gender = as.numeric(tempdf$participant.gender == "Female")
tempdf$graduate = as.numeric(tempdf$graduate)

tempdf = apply(tempdf, 2, FUN = as.numeric)

cor(tempdf, method = "pearson", use = "complete.obs")
ggcorr(tempdf)

redcolssec = match(c("participant.gender", "age",  
                     "highest_education2", "marital_status2", "employment_status2",  
                     "lib_con", 
                     "SM_active",
                     # "lib_con", "Twitter", "Instagram", "Facebook", "Snapchat", "LinkedIn","TikTok",
                     "SM_approval",
                     # "stayintouch", "toomuchtime", "potentialemployers","professional", 
                     "privacy_no_problem",
                     #  "lostControl", "advertisers", "employers",  #privacy
                       "teams", "communicate", "administration", "customerService", "ITskills", 
                       "experience", "education", "hobbies", "CVcontents", "SMcontents", 
                     #"workExperienceIncLength", "educationIncLength","healthLength", "crimeLength", "discriminationLength", "anythingElseLength",
                     "total_time", "TimeSpentInstruc", "mean_no_times_viewed", "mean_notelength", 
                     "median_length_viewed", "min_length_viewed",
                     "control", "gap", "gap_lie", "mental_health", "no_data", "bad"
                  ),
                colnames(player_table))

tempdf = subset(player_table, select = redcolssec)
tempdf$participant.gender = revalue(tempdf$participant.gender,
                        c("Male"=0,
                          "Female" = 1,
                          "Other" = NA))

tempdf = apply(tempdf, 2, FUN = as.numeric)

#cor(tempdf, method = "pearson", use = "complete.obs")
ggcorr(tempdf)

