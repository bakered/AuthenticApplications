#### This code takes a Results page and produces a results page by creating new variables 
#### / data cleaning / changing data type

library(tidyverse)
library(plyr)
library(miceadds)
library(GGally)
library("rvest")
library("readxl")
library(gtrendsR)
#install.packages("stringr")

finishedonly = T
save = T

Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "C")
setwd("C:\\Users\\fo06mafa\\Documents\\AuthenticApplications")
Results = readRDS("C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\Data_amalgamate\\Results.RDS")


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
ag_function = function(column, agmethod="mean"){ #column=results$no_times_viewed
  if(agmethod=="mean"){
    agtab = aggregate(column, list(results$mturkID), mean)
  } else if(agmethod=="median"){
    agtab = aggregate(column, list(results$mturkID), median)
  } else if(agmethod=="min"){
    agtab = aggregate(column, list(results$mturkID), function(x){min(x, na.rm=T)})
  }
  colnames(agtab) = c("mturkID", "mean")
  out = dplyr::left_join(results, agtab, by= "mturkID")$mean
  return(out)
}
normalise <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

colnames(Results)

##choose variables
var_selection<- which(colnames(Results) %in% c("experience_years", "skills_1", "interests_1", "CV_template", "Order", "Gap", "Gender", "Experience", 
                                               "treatment", "skill_level", "Teamwork", "Time.Management", "Microsoft.Office", "Administration", "Customer.Service", "ID", 
                                               "education_yt", "CV_score", "changed_mind", "the_change", "no_times_viewed", 
                                               "length_viewed", "total_time", "notes", "deleted_notes", "notelength", 
                                               "deletednotelength", "feedback", "feedback_length", "age", 
                                               "gender", "ethnicity", "highest_education", "marital_status", 
                                               "employment_status", "state", "lib_con", "Twitter", "Instagram", 
                                               "Facebook", "Snapchat", "LinkedIn", "TikTok", "stayintouch", 
                                               "toomuchtime", "potentialemployers", "professional", "lostControl", 
                                               "advertisers", "employers", "workExperienceInc", "workExperienceIncText", 
                                               "educationInc", "educationIncText", "health", "healthText", "crime", 
                                               "crimeText", "discrimination", "discriminationText", "anythingElse", 
                                               "anythingElseText", "teams", "communicate", "administration", 
                                               "customerService", "ITskills", "experience", "education", "hobbies", 
                                               "CVcontents", "SMcontents", "myID", "mturkID", "mturk_worker_id", 
                                               "session.code", "finished", "TimeSpentInstruc", "time_started", 
                                               "leave_survey", "leave_applications", "ip", "lat", "long", "email", "failed_bec", "present_in_exp_date", 
                                               "time_on_email", "ip2", "lat2", "long2", "city", "session"))


c("list_scores", "list_rounds", "list_rev_orders", 
  "list_time_viewed", "list_clicked_on", "list_clicked_off", "list_pl_CV_order", 
  "list_pl_clicks", "drag", "dragtime", "CVscroll", "CVscrolltime", 
  "SMscroll", "SMscrolltime")

results = subset(Results,  select=var_selection)
if(finishedonly){
  results = subset(results, finished)
}


results$skills_1 = as.factor(results$skills_1)
results$interests_1 = as.factor(results$interests_1)

#table(results$Facebook)
SM_active_vars = c("Twitter", "Instagram", "Facebook", "Snapchat", "LinkedIn", "TikTok")
#so 1 is never use and 7 is a lot
for(var in SM_active_vars){
  results[[var]] = 8-results[[var]]
}
results$SM_active = rowMeans(subset(results, select = SM_active_vars))

#table(results$stayintouch)
## 7 means I approve of SM, 1 = dissaprove
results$stayintouch= 8 - results$stayintouch
results$potentialemployers= 8 - results$potentialemployers
results$professional= 8 - results$professional

SM_approval_vars =  c("stayintouch", "toomuchtime", "potentialemployers", "professional")
results$SM_approval = rowMeans(subset(results, select = SM_approval_vars))


# 7 is there is not problem, 1 is there are concerns
privacy_vars = c("lostControl", "advertisers", "employers")
results$privacy_no_problem = rowMeans(subset(results, select = privacy_vars))


#dput(levels(results$highest_education))

newvar = mapvalues(results$highest_education, 
                   from = c("Bachelorâ\200\231s degree", "Masterâ\200\231s degree", "Some college credit, no degree", 
                            "Associate degree", "High school graduate, diploma or the equivalent (for example: GED)", 
                            "Doctorate degree", "Professional degree", "Trade/technical/vocational training", 
                            "Some high school, no diploma"), 
                   to = c("Bachelor", "Master", "Some College", "Ass.Degree",  "High Scl",  
                          "Doctorate", "Pro.Degree", "Voc.Training", "Some High Scl"  
                           ))
results$highest_education = factor(newvar,
                                     levels = c("Some High Scl", "High Scl",  "Some College", 
                                                "Voc.Training", "Ass.Degree", "Bachelor", 
                                                "Master", "Pro.Degree", "Doctorate"),
                                     ordered = T)
results$graduate = mapvalues(results$highest_education,
                               from = c("Some High Scl", "High Scl",  "Some College", 
                                        "Voc.Training", "Ass.Degree", "Bachelor", 
                                        "Master", "Pro.Degree", "Doctorate"),
                               to = c(F,F,F,F,T,T,T,T,T))

load.Rdata("C:\\Users\\fo06mafa\\Documents\\CVs\\election_president.RData", "StateVoting")
StateVoting$Share = StateVoting$candidatevotes/StateVoting$totalvotes
StateVoting = subset(StateVoting, year == 2016 & 
                       candidate == "Trump, Donald J." &
                       party == "republican" &
                       writein == F, 
                     select = c(state, Share))
StateVoting = StateVoting[order(StateVoting$Share),]
StateVoting[(StateVoting$state == "District of Columbia"),]$state  = "Washington, D.C."
toadd = as.data.frame(results$state)
colnames(toadd) = "state"
toadd = dplyr::left_join(toadd, StateVoting, by = "state")
table(toadd$state == results$state)
results$TrumpShare = toadd$Share
results$TrumpSharebin = bin_function(results$TrumpShare)
results$state = factor(results$state, levels =  StateVoting$state, ordered = T)

#mental_health_day
mental_health_day = read.csv("C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\geoMap.csv")
rownames(mental_health_day)[which(rownames(mental_health_day)=="District of Columbia")] = "Washington, D.C."
mental_health_day$state = rownames(mental_health_day)
mental_health_day = mental_health_day[-1,]
colnames(mental_health_day) = c("mental_health_dayIndex", "state")
mental_health_day$mental_health_dayIndex = as.numeric(mental_health_day$mental_health_dayIndex)

toadd = as.data.frame(results$state)
colnames(toadd) = "state"
toadd = dplyr::left_join(toadd, mental_health_day, by = "state")
results$mental_health_dayIndex = toadd$mental_health_dayIndex

######## NEW TRUMPSHARE ###########
load.Rdata("C:\\Users\\fo06mafa\\Documents\\CVs\\county_pres_election.RData", "CountyVoting")
cities = read.csv("C:\\Users\\fo06mafa\\Documents\\CVs\\uscities.csv")
CountyVoting$Share = CountyVoting$candidatevotes/CountyVoting$totalvotes
CountyVoting = subset(CountyVoting, year == 2016 & 
                       candidate == "Donald Trump" &
                       party == "republican",
                      select = c(county, FIPS, Share))
CountyVoting = CountyVoting[order(CountyVoting$Share),]
CountyVoting$county[!(CountyVoting$FIPS %in% cities$county_fips)]


cities = dplyr::left_join(cities, CountyVoting, by=c("county_fips" = "FIPS"))
cities[(cities$state_name == "District of Columbia"),]$state_name  = "Washington, D.C."

######### NEW TRUMPSHARE 2020 ######################
CountyVoting20 = read.csv("C:\\Users\\fo06mafa\\Documents\\CVs\\2020_US_County_Level_Presidential_Results.csv")
CountyVoting20 = subset(CountyVoting20,
                      select = c(county_name, county_fips, per_gop))
CountyVoting20 = CountyVoting20[order(CountyVoting20$per_gop),]
CountyVoting20$county_name[!(CountyVoting20$county_fips %in% cities$county_fips)]
cities = dplyr::left_join(cities, CountyVoting20, by=c("county_fips" = "county_fips"))

########### COUNTY STATS #################
county_stats = read_excel("C:\\Users\\fo06mafa\\Documents\\CVs\\ERSCountyTypology2015Edition.xls")
colnames(county_stats) = county_stats[3,]
county_stats = county_stats[-c(1:3),]
county_stats = as.data.frame(county_stats)
county_stats[,4:17] = lapply(county_stats[,4:17], as.numeric)
county_stats$FIPStxt = as.integer(county_stats$FIPStxt)

county_pop = read_excel("C:\\Users\\fo06mafa\\Documents\\CVs\\PopulationEstimates.xls")
colnames(county_pop) = county_pop[2,]
county_pop = county_pop[-c(1:2),]
county_pop = as.data.frame(county_pop)
county_pop$FIPStxt = as.integer(county_pop$FIPStxt)
county_pop[,4:165] = lapply(county_pop[,4:165], as.numeric)

########create arcgis "USA facebook Users county level"

####################### MERGE INTO CITIES DATABASE #####################
county_stats = dplyr::left_join(county_stats, county_ethnic, by=c("FIPStxt" = "county_fips"))
cities = dplyr::left_join(cities, county_stats, by=c("county_fips" = "FIPStxt"))
cities = dplyr::left_join(cities, county_pop, by=c("county_fips" = "FIPStxt"))

# population  ???
sub.est2019_all <- read.csv("~/Downloads/sub-est2019_all.csv", header=FALSE)


#################### NEW mental_health_day ON DMA AREA #############################
mental_health_day2 = gtrends(c("mental health day"), time = "all", gprop = "web", geo = c("US"))
mental_health_day2df = mental_health_day2$interest_by_dma
load.Rdata("C:\\Users\\fo06mafa\\Documents\\CVs\\county_dma.RData", "countydma")

statlist = unique(cities$state_id)
statlist = paste(statlist, collapse = "|")
convert = function(a, c){ #a=sort(mental_health_day2df$location)
  if(c){
    for(s in 1:length(a)){ #s=2
      s1 = unlist(strsplit(a[s], split="\\-|\\&"))
      last = s1[length(s1)]
      for(d in 1:length(s1)){
        if(!grepl(statlist, s1[d])){
          s1[d] = paste0(s1[d], substr(last, nchar(last)-2, nchar(last)))
        }
      }
      a[s] = paste(s1, sep="", collapse="")
    }
  }
  a = as.character(tolower(a))
  a = gsub("[[:punct:]]", "", a)
  a = gsub("[[:space:]]", "", a)
  a = replace(a, a=="birminghamalannistonaltuscaloosaal" , "birminghamal" )
  a = replace(a, a== "charlottesville", "charlottesvilleva" )
  a = replace(a, a=="cheyennewyscottsblufne" , "cheyennewyscottsbluffne" )
  a = replace(a, a=="clevelandohakronohcantonoh" ,  "clevelandohakroncantonoh")
  a = replace(a, a== "ftsmitharfayarspringdalearrogersar" , "ftsmitharfayettevillearspringdalearrogersar" )
  a = replace(a, a=="harrisonburg" , "harrisonburgva" )
  a = replace(a, a=="huntsvillealdecaturalflorenceal" , "huntsvillealdecaturflorenceal"   )
  a = replace(a, a== "kingsporttnjohnsoncitytnbristoltn" , "trivacitiestnva" )
  a = replace(a, a== "minotndbismarcknddickinsonnd", "minotndbismarcknddickinsonwillistonnd"   )
  a = replace(a, a== "mobilealpensacolaflftwaltonfl" , "mobilealpensacolaftwaltonbeachfl" )
  a = replace(a, a==  "montgomeryalselmaal","montgomeryselmaal"  )
  a = replace(a, a== "norfolkportsmthnewptnws","norfolkvaportsmouthvanewportnewsva"  )
  a =  replace(a, a== "paducahkycapegirardeaumoharrisburgilmtvernonks", "paducahkycapegirardeaumoharrisburgilmountvernonil" )
  a = replace(a, a== "raleighncdurhamncfayettevillenc", "raleighncdurhamfayettevillenc" )
  a = replace(a, a== "richmondpetersburg","richmondvapetersburgva"  )
  a = replace(a, a== "roanokelynchburg" , "roanokevalynchburgva" )
  a = replace(a, a== "santabarbaracasanmarcoscasanluisobispoca"    , "santabarbaracasantamariacasanluisobispoca"   )
  a = replace(a, a== "siouxfallssdmitchellsd"   ,"siouxfallsmitchellsd"   )
  a = replace(a, a== "tricitiestnva" , "trivacitiestnva" )
  a = replace(a, a== "tucsonazsierravistaaz", "tucsonsierravistaaz" )
  a = replace(a, a== "tylertxlongviewtxlufkintxnacogdochestx" , "tylertxlongviewlufkintxnacogdochestx" )
  a = replace(a, a== "washingtondchagrstwn"  , "washingtondchagerstownmd" ) 
  a = replace(a, a== "tampaflstpetersburgsarasotafl"  , "tampaflstpetersburgflsarasotafl")
  if(!c){
    a[grepl("Riverside", countydma$COUNTY)] = "palmspringsca"
  }
  return(a)
  }

countydma$dmalink = convert(countydma$DMA, F)
mental_health_day2df$dmalink = convert(mental_health_day2df$location, T)
countydma = dplyr::left_join(countydma, mental_health_day2df, by = "dmalink")

convert2 = function(a){ #a=countydma$COUNTY
  a = gsub("[[:space:]]", "", a)
  a = gsub("Parish", "", a)
  a = gsub("City", "", a)
  a = gsub("Borough", "", a)
  a = gsub("[[:punct:]]", "", a)
  a = gsub("Saint", "St", a)
  a = tolower(a)
  a = sapply(a, FUN = function(x) {
    str = substr(x, 1,4)
    str2 = substr(x, 1,5)
    str3 = substr(x, 1,9)
    if(str3 %in% c("Northwest", "Southeast")){
      toreturn = paste0(substring(x,10),str3)
    } else if(str %in% c("East", "West")){
      toreturn = paste0(substring(x,5),str)
    } else if(str2 %in% c("North", "South")){
      toreturn = paste0(substring(x,6), str2)
    } else {
      toreturn = x
    }
    return(toreturn)
  })
  a = unlist(a)
  return(a)
}

countydma$link2 = convert2(countydma$COUNTY)
cities$link2 = convert2(cities$county_name)
countydma$STATE = gsub("[[:space:]]", "", countydma$STATE)

table(mental_health_day$state)




cities$DMA = NA
cities$mental_health_day2 = NA
for(r in 1:nrow(cities)){ #r=11811
  find = subset(countydma, STATE==cities$state_id[r] & link2==cities$link2[r])
  if(length(unique(find$location)) == 1){
    cities$DMA[r] = find$location[1]
    cities$mental_health_day2[r] = find$hit[1]
  } else if(length(unique(find$location)) > 1){
    print(paste("greater", cities$link2[r]))
    find = subset(mental_health_day, state==cities$state_name[r])
    if(nrow(find) == 1){
      cities$mental_health_day2[r] = find$mental_health_dayIndex
    }else {
      print(paste("state", cities$state_name[r]))
      stop()
    } 
  } else {
    find = subset(mental_health_day, state==cities$state_name[r])
    if(nrow(find) == 1){
    cities$mental_health_day2[r] = find$mental_health_dayIndex
    }else {
      print(paste("state", cities$state_name[r]))
    } 
  }
}



results$city[!(results$city %in% cities$city)]
toadd = subset(results, select=c(state, city,lat2,long2))
toadd$Share = NA
toadd$per_gop = NA
toadd$state2 = NA
toadd$mental_health_dayIndex2 = NA
toadd$DMA= NA
toadd$metro = NA
toadd$rural_urban_con = NA
toadd$urban_influence = NA
toadd$economy_type = NA


#table(toadd$city)
for(r in 1:nrow(toadd)){ #r=2
  if(!is.na(toadd$city[r])){
    find = subset(cities, city==toadd$city[r])
    if(nrow(find)==0){
      cities$distance = sqrt(abs(toadd$lat2[r] - cities$lat)^2 + abs(toadd$long2[r] - cities$lng)^2)
      find = subset(cities, cities$distance==min(cities$distance))
      print(toadd$city[r])
      print(find$city)
    }
    find$distance = sqrt(abs(toadd$lat2[r] - find$lat)^2 + abs(toadd$long2[r] - find$lng)^2)
    find = subset(find, find$distance==min(find$distance))
    if(nrow(find) == 1){
      toadd$Share[r] = find$Share
      toadd$per_gop[r] = find$per_gop
      toadd$state2[r] = find$state_name
      toadd$mental_health_dayIndex2[r] = find$mental_health_day2
      toadd$DMA[r] = find$DMA
      
      toadd$metro[r] = find$`Metro-nonmetro status, 2013 0=Nonmetro 1=Metro`
      toadd$rural_urban_con[r] =find$`Rural-urban_Continuum Code_2013`
      toadd$urban_influence[r] =find$Urban_Influence_Code_2013
      toadd$economy_type[r] =find$Economic_typology_2015
     # colnames(find)
      
    } else {
      print(paste(toadd$city[r], r))
    }
  }
}
results$TrumpShare2 = toadd$Share
results$TrumpShare3 = toadd$per_gop
results$state2 = toadd$state2
results$mental_health_dayIndex2 = toadd$mental_health_dayIndex2
results$DMA = toadd$DMA
results$metro = toadd$metro
results$rural_urban_con = toadd$rural_urban_con
results$urban_influence = toadd$urban_influence
results$economy_type = toadd$economy_type

table(is.na(results$TrumpShare2))

##fill in the gaps
results$TrumpShare2[is.na(results$TrumpShare2)] = results$TrumpShare[is.na(results$TrumpShare2)]
results$TrumpShare3[is.na(results$TrumpShare3)] = results$TrumpShare2[is.na(results$TrumpShare3)]
results$mental_health_dayIndex2[is.na(results$mental_health_dayIndex2)] = results$mental_health_dayIndex[is.na(results$mental_health_dayIndex2)]

c("metro", "rural_urban_con", "urban_influence", "economy_type")


new = readRDS("C:\\Users\\fo06mafa\\Documents\\CVs\\state_age.RDS")
new = new[,2:3]
colnames(new) = c("state", "stateage")
new$state[new$state == "District of Columbia"] = "Washington, D.C."
toadd = as.data.frame(results$state)
colnames(toadd) = "state"
toadd = dplyr::left_join(toadd, new, by = "state")
results$stateage = toadd$stateage

#random
new = data.frame(unique(results$mturkID))
new$random = rnorm(nrow(new))
new$random2 = rnorm(nrow(new))
new$random3 = rnorm(nrow(new))
colnames(new)[1] = c("mturkID")
toadd = as.data.frame(results$mturkID)
colnames(toadd) = c("mturkID")
toadd = dplyr::left_join(toadd, new, by = "mturkID")
results$random = toadd$random
results$random2 = toadd$random2
results$random3 = toadd$random3

#table(results$experience_years)
CV.experience.factor = revalue(as.factor(results$experience_years),
                               c("4" = 1,
                                 "6" = 2,
                                 "12.25" = 3,
                                 "12.75" = 4,
                                 "17.5" = 5,
                                 "18.5" = 6
                                ))

results$CV.experience.factor = factor(CV.experience.factor,
                                        levels = c("1", "2", "3", "4", "5", "6"), #levels(CV.experience.factor),
                                        ordered = T)

table(results$education_yt)
results$education_yt.factor = as.factor(results$education_yt)

##adjust from seconds to minutes
results$total_time = results$total_time/60
results$length_viewed = results$length_viewed/60
results$TimeSpentInstruc = results$TimeSpentInstruc/60
results$date_started2 = results$time_started
results$date_started = as.Date(results$time_started)
results$date_started_fac = factor(results$date_started, ordered = T)


times = strftime(results$time_started, format="%H:%M:%S")
results$time_started = as.POSIXct(times, format="%H:%M:%S")
results$time_startedbin = bin_function(results$time_started)
results$time_startednum = as.numeric(results$time_started)


##mean number of times viewed CV, measure of effort
temp = ag_function(results$no_times_viewed)
head(temp)
head(results$no_times_viewed)
results$mean_no_times_viewed = ag_function(results$no_times_viewed)

##mean note length
results$mean_notelength = ag_function(results$notelength)
results$feedback_length = nchar(results$feedback)
results$median_length_viewed = ag_function(results$length_viewed, "median")
results$min_length_viewed = ag_function(results$length_viewed, "min")



table(results$ethnicity, results$gender)/6

results$identity = with(results, interaction(ethnicity,  gender))
results$identity = factor(results$identity, levels = c(levels(results$identity), "Other"))
results$identity[which(results$identity %in% names(table(results$identity))[table(results$identity)<20*6])] = "Other"
results$identity = droplevels(results$identity)
table(results$identity)/6


results$freq = NA
pltab = subset(results, !duplicated(mturkID), select=c(mturkID, lat2, long2, freq, date_started2))
pltab = pltab[order(pltab$date_started2),]
for(r in 1:nrow(pltab)){
  pltab$freq[r] = sum(pltab$lat2[1:r]==pltab$lat2[r]&pltab$long2[1:r]==pltab$long2[r], na.rm = T)
}
toadd = as.data.frame(results$mturkID)
colnames(toadd) = "mturkID"
toadd = dplyr::left_join(toadd, pltab, by = "mturkID")
results$freq = toadd$freq

results$freqID = paste(results$long2, results$lat2)
results$freqID[!(results$freqID %in% results$freqID[results$freq>1])] = NA

agtab = dplyr::count(results, session.code, mturkID)
#View(subset(results, select=c("session.code", "mturkID")))
#View(agtab)
agtab$n = NULL
agtab = dplyr::count(agtab, session.code)
colnames(agtab)[2] = "session.size"
results = dplyr::left_join(results, agtab, by="session.code")




##print variables
if(F){
  for(n in 1:ncol(results)){
  #print(n)
 print(paste(colnames(results)[n], "class:", class(results[,n])))
}
}



results$Female = results$Gender == "Female"
vars2 = match(c("experience_years",
                "Female", 
                "Order", 
                "education_yt", 
                "length_viewed", 
                "total_time", 
                "TimeSpentInstruc",
                "notelength", 
                "lib_con", 
                "age", 
                "TrumpShare",
                "mental_health_dayIndex"
                ), 
                colnames(results))

tempdf = subset(results, select = vars2)
tempdf$Female = as.numeric(tempdf$Female)
tempdf = apply(tempdf, 2, FUN = as.numeric)

cor(tempdf, method = "pearson", use = "complete.obs")
ggcorr(tempdf)

#table(results$highest_education)
results$highest_education2 = as.integer(as.character(revalue(results$highest_education, replace = c("Some High Scl" = 1,
                                                                                                 "High Scl" = 2,
                                                                                                 "Some College" = 3,
                                                                                                 "Voc.Training" = 4,
                                                                                                 "Ass.Degree" = 5,
                                                                                                 "Bachelor" = 6,
                                                                                                 "Master" = 7,
                                                                                                 "Pro.Degree" = 8,
                                                                                                 "Doctorate" = 9))))
#table(results$marital_status)
results$marital_status2 = as.integer(as.character(revalue(results$marital_status, replace = c("Not Married" = 1,
                                                                                              "Married or Domestic Partnership" = 2,
                                                                                              "Divorced" = 3,
                                                                                              "Widowed" = 4))))


results$employment_status2 = as.integer(as.character(revalue(results$employment_status, replace= c("A student" = 1,
                                                                                                      "Employed for wages" = 2,
                                                                                                      "Self-employed" = 3,
                                                                                                      "Military" = 4,
                                                                                                      "A homemaker" = 5,
                                                                                                      "Out of work and looking for work" = 6,
                                                                                                      "Out of work but not currently looking for work" = 7,
                                                                                                      "Unable to work" = 8,
                                                                                                      "Retired" = 9,
                                                                                                      "Other" = 10
))))

results$interests_1.2 = as.integer(as.character(revalue(results$interests_1, replace = c("I am a keen cyclist and runner. Last year I completed the Atlantic City marathon." = 2, 
                                                                                           "I am an avid portrait photographer and recently reached the final in the ‘Shoot the Frame’ photography competition." = 3, 
                                                                                           "In my freetime I enjoy reading and watching tv." = 6, 
                                                                                           "In my spare time I enjoy swimming and playing volleyball, I play for a team which competes in local competitions." = 4, 
                                                                                           "In my spare time I enjoy yoga and producing nature videos and I volunteer for a local charity “The food trust” helping them create media content for their website." = 1, 
                                                                                           "Outside of work I enjoy painting and regularly attend a life drawing class." = 5
))))
results$interests_1.2 = 7 - results$interests_1.2
results$skills_1.2 = as.integer(as.character(revalue(results$skills_1, replace = c("Administration" = 2, 
                                                                                   "Good Team Player" = 3, 
                                                                                   "Good Time Management" = 5, 
                                                                                   "Management Skills" = 1, 
                                                                                   "Strong organisational skills" = 4, 
                                                                                   "Word" = 6
))))
results$skills_1.2 = 7- results$skills_1.2
  

results$date_started2 = as.POSIXct(results$date_started2)
results$date_started3 = as.numeric(results$date_started2)


results$treatment = factor(results$treatment, levels = c("control", "gap", "gap_lie", "mental_health", "no_data", "bad"))
#results$treatment = factor(results$treatment, levels = rev(c("control", "gap", "gap_lie", "mental_health", "no_data", "bad")))

results$experience_years = as.numeric(results$experience_years)


results = ddply(results, c("mturkID"), transform, x.std = scale(CV_score))
colnames(results)[which(colnames(results) == "x.std")] = "CV_score.std"

text_vars = c("workExperienceIncText", "educationIncText", "healthText", "crimeText", "discriminationText", "anythingElseText")
for(var in text_vars){ #var = text_vars[1]
  colnum = ncol(results)+1
  results[[var]][is.na(results[[var]])] = ""
  tomerge = aggregate(as.formula(paste(var, "~ mturkID")), results, FUN = function(x) nchar(x[1]))
  results = merge(results, tomerge, by="mturkID", all=T)
  colnames(results)[colnum] = gsub("Text", "Length", var)
}


results$CV_SM_diff = results$CVcontents - results$SMcontents
results$endorsements = results$Teamwork + results$Time.Management + results$Microsoft.Office + results$Administration + results$Customer.Service


results$Teamwork[results$treatment == "no_data"] = NA
results$Time.Management[results$treatment == "no_data"] = NA
results$Microsoft.Office[results$treatment == "no_data"] = NA
results$Administration[results$treatment == "no_data"] = NA
results$Customer.Service[results$treatment == "no_data"] = NA

results$control = results$treatment == "control"
results$gap = results$treatment == "gap" | results$treatment == "gap_lie"
results$gap_lie = results$treatment == "gap_lie"
results$mental_health = results$treatment == "mental_health"
results$no_data = results$treatment == "no_data"
results$bad = results$treatment == "bad"

colnames(results)[which(colnames(results) == "gender")] = "participant.gender"
colnames(results)[which(colnames(results) == "Gender")] = "applicant.gender"




results = results[order(results$date_started3),]
if(save){
saveRDS(results, "C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\Data_amalgamate\\results.RDS")
#saveRDS(results, "C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\Data_amalgamate\\results.by.score.RDS")
}



hist(results$time_started, breaks="hours")

results$experience_years = as.numeric(results$experience_years)
ggplot(results, aes(x=experience_years, y=CV_score), group=treatment) + 
  geom_point(position="jitter", aes(colour=treatment)) +
  geom_smooth(method=lm) +
  facet_grid(treatment~.) +
  lims(y=c(0,10)) +
  theme_classic()

print(ggplot(results, aes(x=as.numeric(experience_years), y=CV_score, colour=treatment)) + 
        geom_point(position="jitter") +
        geom_smooth(method=lm)+
        lims(y=c(0,10)) +
        theme_classic())



results$notes %>% grepl(pattern="gap") %>% table(results$treatment)



