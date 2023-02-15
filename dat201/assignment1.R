

df = read.csv("assignment1.csv")


# ------------ DATA CLEANING AND PREP ------------
#convert the date fields from char to Date mode
df$hospDis = as.Date(df$hospDis, format="%m/%d/%Y")
df$hospAd = as.Date(df$hospAd, format="%m/%d/%Y")
df$DOB = as.Date(df$DOB, format="%m/%d/%Y")

# replace all NA values to 0
df[is.na(df)] = 0


# ------------ QUESTIONS TO ANSWER ------------
# 1. Average days in hospital
df$days = as.numeric(df$hospDis - df$hospAd)
mean(df$days)             # mean = 6.264529 days
median(df$days)           # median = 5 days


# 2. Average age of pts as of jan01,2023
asof = as.Date("2023-01-01")
df$age = as.numeric((as.Date("2023-01-01") - df$DOB)/365)
mean(df$age)              # mean = 66.95729 
median(df$age)            # median = 67.60548


# 3. How many patients have (a) died, (b) re-hopitalized 
sum(df$death)             # 4 patients have died
sum(df$reHosp)            # 116 have been rehospitalized


# 4. How many patients have any Dr visit
df$hadVisit = df$drVisIP | df$drVisVR
sum(df$hadVisit)          # 416 


# 5. Provide summary statistics for the vacation days.
summary(df$datV)          # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
                          # 2.00   30.00   31.00   29.75   31.00   31.00 


# ------------ GENERAL COMMENTS ------------

df[df$death==1,]
# - Only 4 deaths out of 499 observations (proportion of 0.8%).  A larger 
#   sample would be needed to draw conclusions about correlation regarding
#   death.

boxplot(df$age ~ df$death)
summary(df$age[df$death==1])
summary(df$age[df$death==0])
# - Average age of those who died is higher than those that lived.  
# - Range of ages for deaths (60-95) is higher and narrower than those 
#   that lived (43-95).


boxplot(df$datV ~ df$death)
summary(df$datV[df$death==1])
summary(df$datV[df$death==0])
# - Those that died had significantly less time without symptoms.  
#    - Several patients had vacation days within the same range of those that 
#      died, but these are all outliers.
#    - For non-deaths, the median is equal to max value.  Assumption is the 
#      study ceased to track patients after 31 days of no symptoms.  

boxplot(df$days ~ df$death)
# - Those that died spent longer time in the hospital (median of 15 days vs 5).


