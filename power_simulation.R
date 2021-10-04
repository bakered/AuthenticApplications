
###### POWER ANALYSIS ######
# produces a plot that shows the power (chance of a significant effect at 0.05 
# level) for a standard regression with one treatment and one control given n
# subjects and e effect size.
# I assume there is an underlying function score = \betaX + \tauW + \epsilon
# and then participants round to the nearest whole number.

library(dplyr)
library(lfe)
library(ggplot2)
#install.packages("lfe")

#### SIMULATE DATA ####
# takes in n number of participants and e effect size and returns dataframe
# each particpant views 6 applications and gives a score out of 10


#X possible values from which to draw from
pool = data.frame(
experience_pool = c(4, 6, 12, 13, 17, 18),
education_pool = c(1, 4, 5, 6, 7, 8),
skills_pool = c(1, 2, 3, 4, 5, 6),
interests_pool = c(1, 2, 3, 4, 5, 6),
female_pool = c(0, 1),
order_pool = c(1, 2, 3, 4, 5, 6)
)
#scale data as in the linear model beta values used below
pool = data.frame(scale(pool[,1:4]), pool[,5, drop=F], scale(pool[,6, drop=F]))

#function creates simulated data frame
create_sim_results = function(n, e){ #n = 100; e= 0
  #create six rows for one particpant, one row for each CV
  create_six_rows = function(){
    data.frame(
      CV = seq(1, 6, 1),
      experience = sample(pool$experience_pool, 6, replace = F),
      education = sample(pool$education_pool, 6, replace = F),
      skills = sample(pool$skills_pool, 6, replace = F),
      interests = sample(pool$interests_pool, 6, replace = F),
      female = sample(pool$female_pool, 6, replace = F),
      order = sample(pool$order_pool, 6, replace = F),
      treatment = sample(c(0, 0, 0, 0, 0, 1), 6, replace = F)
    )
  }
  # join all participants together into one dataframe
  list = replicate(n, create_six_rows(), simplify = FALSE)
  sim_results =  cbind(data.frame(participant = rep(1:n, each = 6)), 
                       bind_rows(list))
  
  # raw CV_score using beta values from previous experiment
  sim_results$CV_score = 5.992 +
    2.468 * sim_results$experience +
    0.587 * sim_results$education +
    0.183 * sim_results$skills + 
    0.121 * sim_results$interests +
    0.046 * sim_results$female +
    -0.106 * sim_results$order +
    e * sim_results$treatment +
    rnorm(nrow(sim_results), sd = 2.168)
  
  #round to nearest whole number
  sim_results$CV_score = round(sapply(sim_results$CV_score, function(y){min(max(y,0),10)}))
  #hist(sim_results$CV_score)
  return(sim_results)
}

#### STANDARD REGRESSION ####
# runs a linear model with clustered standard errors on participant and records 
# whether treatment effect is significant

is_significant = function(n, e){
  sim_results = create_sim_results(n = n, e = e)
  linear_model = felm(CV_score ~ experience + education + skills + 
                        interests + female + order + treatment  | 0 | 0 | participant, data = sim_results)
  significant = summary(linear_model)$coef[8, 4] < 0.05
  return(significant)
}


#### POWER CALCULATION ####
# for n participants and e effect size we run s simulations and record the power

find_power = function(n, e, s){
  mean(replicate(s, is_significant(n=n, e=e)))
}


#### CREATE POWER DATAFRAME ####
# creates a data frame and fills it in with find_power function

power = data.frame(
  n = rep(seq(400,1500, 120), each = 6),
  e = rep(seq(0, 0.5, 0.1), 10),
  s = rep(400, 60)
)
# apply find_power function to each row in power dataframe
power$power = mapply(find_power, power$n, power$e, power$s)


#### PLOT ####
# plots power (y-axis) for given number of particpants (x-axis)  and effect size
# (line type)

power$e = factor(power$e, ordered=T)
ggplot(power, aes(x = n, y=power, group = e, colour= e)) + 
  geom_line() +
  geom_point() +
  geom_hline(yintercept=0.05, colour="red",linetype="dashed") +
  annotate("text", x=0.23, y=0.05, label="power=0.05", hjust=0, vjust=1.2, size=3, colour="red")+
  labs(title="Power", x="Number Participants", y = "Power")+
  scale_colour_discrete(name="Effect Size (/10)") +
  theme_classic() 
