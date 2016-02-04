# Retention Exploration

# ------------ <<Functions: Data Structurization for all users>> ------------

# Step 1 - Getting cleaned full Registration Records

GetCleanRegst = function(full_history){
  history = full_history
  index_sign_up=which(history$action=='sign_up')
  
  all.regist = list()
  all.regist$id = history$user_id[index_sign_up]
  all.regist$regtime = round.POSIXt(as.POSIXlt(history$event_time[index_sign_up]),units = 'hours')
  
  # Sorting
  
  all.regist = data.frame(all.regist)
  all.regist = all.regist[order(all.regist$id),]
  
  # Cleaning
  n.all.regist = length(all.regist$id)
  
  
  # Step 1: Checking if the dataset 'all.regist' has Duplicates
  # <Analysis>
  n.duplicated = length(which(duplicated(all.regist$id)))
  rate.duplicates = round(n.duplicated/n.all.regist,digits = 5)
  
  # Step 2: Getting the index (for 'all.regist') of Duplicates and Those having dissimilar registration Duplicates
  index_dupe = which(duplicated(all.regist$id))
  index_dupe_disagree = index_dupe[which(all.regist$regtime[index_dupe]!=all.regist$regtime[index_dupe-1])]
  
  # Step 3: Deleting those having dissimilar duplicates & Keeping one record of those having similar duplicates
  
  index_rmv=unique(c(index_dupe,index_dupe_disagree-1))
  label_keep=rep(1,n.all.regist);label_keep[index_rmv]=0 # labeling if to be kept
  
  clean.regist=subset(all.regist,label_keep==1)
  clean.regist$regtime = as.POSIXlt(clean.regist$regtime)
  
  result = list(data=clean.regist,rate.dup=rate.duplicates)
  return(result)
}

# Step 2 - Subsetting Registration Records (which is early enough for determining retention status
# The following function only keep Registration Records 28 days before latest registration date 
# (This is for Retention Measurement based on whether having actions during 21st - 28th day after registration)

# Random Sample Selector (of size n.random)
# Note: regtime.units = 'days' or 'hours'; it is the units of registration regtime for the output action data
# (choosing 'hours' if considering registration hour factor)

SubsetRegist=function(clean.regist, n.random){
  # getting deadline
  deadline = max(clean.regist$regtime) - as.difftime(28,units='days') # can be modified
  
  subset.regist = clean.regist[which(clean.regist$regtime<deadline),];m=length(subset.regist$id)
  if (n.random != -1) {subset.regist = subset.regist[sample(1:m, n.random),] }
  subset.regist$regtime = as.POSIXlt(subset.regist$regtime)$yday
  n.subset.regist = length(clean.regist$id)
  
  result=list(data=subset.regist,n=n.subset.regist)
  return(result)
}

# Step 3 - Getting Corresponding Action Records of Step 2

GetCleanAction=function(full.action,selected.regist){
  
  history=full.action
  data_regist_full = selected.regist
  
  # Extracting datalines of valid users from action history
  
  data_action_full_temp = history[which(history$user_id%in%data_regist_full$id),]
  data_action_full_temp = data_action_full_temp[order(data_action_full_temp$user_id,data_action_full_temp$event_time),]
  
  # check: which(unique(data_action_full_temp$user_id)!=data_regist_full$id)
  
  summary = table(data_action_full_temp$user_id,data_action_full_temp$action)
  
#   data_action_full = data.frame(id=data_action_full_temp$user_id, time=data_action_full_temp$event_time,
#                                 action=data_action_full_temp$action)
  
  n.action_full = length(data_action_full_temp$action)
  names(data_action_full_temp)[1:2]=c('time','id')
  # MultiAct Labels
  
  label.multiact = rowSums(summary)>1 # Logical
  n.regist_multiact=sum(as.numeric(label.multiact))
  
  result=list(data=data_action_full_temp,count.summary=summary,n=n.action_full,
              label=label.multiact,n.multi=n.regist_multiact)
  
}

# --------- <<Functions: Data Structurization for MultiAct Users only>> ---------

# --- Structurization for Diff_time and Label
# Method: Joining Regist & Action Table, Structuration based on that
StrctData_Multi=function(data_regist_sample,data_action_sample,label_multi_sample){
  data_regist_multiact = subset(data_regist_sample,label_multi_sample==T)
  
  # Constructing <Data_Action_MultiAct> (action history for multiact-user)
  
  data_action_multiact = data_action_sample[which(data_action_sample$id %in% data_regist_multiact$id),]
  data_action_multiact = data_action_multiact[order(data_action_multiact$id),]
  
  rfmt_data_action = merge(x = data_action_multiact, y = data_regist_multiact, by = "id", all.x = TRUE) # Left outer join
  rfmt_data_action$time_elps = rfmt_data_action$time - rfmt_data_action$regtime
  
  result=list(regist=data_regist_multiact,action=rfmt_data_action)
  return(result)
}

# Alternative Method: Join/Merge Not Used
StrctData2_Multi=function(data_regist_sample, data_action_sample, label_multi_sample){
  data_regist_multiact = data.frame(subset(data_regist_sample,label_multi_sample==T))
  
  # Constructing <Data_Action_MultiAct> (action history for multiact-user)
  
  data_action_multiact = data_action_sample[which(data_action_sample$id %in% data_regist_multiact$id),]
  data_action_multiact = data_action_multiact[order(data_action_multiact$id),]
  
  if (length(which(unique(data_action_multiact$id)!=data_regist_multiact$id))==0) print('Bingo!!!!!!!!!!!!!!!!!!!!!!1')
  
  nn = length(data_action_multiact$action)
  
  # Computing Time Elapsed of each action time since registration
  
  # a) Getting index of the first & last action for each user
  d_action_sub_1to2ndlast = data_action_multiact$id[1:(nn-1)]
  d_action_sub_2tolast = data_action_multiact$id[2:nn]
  
  index_last_action = which(d_action_sub_1to2ndlast != d_action_sub_2tolast)
  if (index_last_action[length(index_last_action)]==nn-1){index_last_action = 
    c(index_last_action,nn)}
  
  index_first_action = which(d_action_sub_2tolast != d_action_sub_1to2ndlast)+1
  if (index_first_action[1]==2) index_first_action = c(1,index_first_action)
  
  index_first_last_action = cbind(index_last_action,index_first_action)
  
  # b) Getting Time Elapsed for each action
  diff_t = rep(0,length(d_regist_subset$id))
  for (k in 1:length(d_regist_subset$id)){
    diff_t[index_first_action[k]:index_last_action[k]]=
      d_action_subset$time[index_first_action[k]:index_last_action[k]] - d_action_subset$time[index_first_action[k]]
    print(k)
  }
  
  data_action_multiact$diff_t = diff_t
  
  result = list(data_regist=data_regist_multiact,data_action=data_action_multiact)
  return(result)
}

# Retention Label
# Getting retention labels for sampled (multiact) registration data

# (Defn for Retention: active during 4th wk of registration)

retent_lable_wk4 = function(data_action_multiact,data_regist_multiact){
  retained_id = unique(data_action_multiact$id[which (data_action_multiact$time_elps >= 21 & data_action_multiact$time_elps <= 28)])
  retention_lable=rep(0,length(data_regist_multiact$id)); retention_lable[which(data_regist_multiact$id%in%retained_id)]=1
  return(retention_lable)
}

# Predicor (Action Frequency/Binary) Reformator

rft_action_binary=function(smr_tbl_action_count,group_cov,name_groupedcov){
  count_action = smr_tbl_action_count
  # colnames(freq_action)=list_action
  # Getting index of grouped factors
  xx=NULL; binary = list()
  for (i in 1:length(group_cov)){
    index = which(colnames(count_action)%in%group_cov[[i]])
    if (length(index)==1) {xx=count_action[,index]}
    else {xx = rowSums(count_action[,index])}
    binary[[i]]=xx>0
  }
  
  result = data.frame(binary)
  colnames(result) = name_groupedcov
  
  return(result)
}
rft_action_freq=function(smr_tbl_action_count,group_cov,name_groupedcov){
  
  freq_action = smr_tbl_action_count/rowSums(smr_tbl_action_count)
  # colnames(freq_action)=list_action
  # Getting index of grouped factors
  xx = list()
  for (i in 1:length(group_cov)){
    index = which(colnames(freq_action)%in%group_cov[[i]])
    if (length(index)==1) {xx[[i]]=freq_action[,index]}
    else {xx[[i]] = rowSums(freq_action[,index])}
  }
  
  result = data.frame(xx)
  colnames(result) = name_groupedcov
  
  return(result)
}
rft_action_count=function(smr_tbl_action_count,group_cov,name_groupedcov){
  
  count_action = smr_tbl_action_count
  # colnames(freq_action)=list_action
  # Getting index of grouped factors
  xx = list()
  for (i in 1:length(group_cov)){
    index = which(colnames(count_action)%in%group_cov[[i]])
    if (length(index)==1) {xx[[i]]=count_action[,index]}
    else {xx[[i]] = rowSums(count_action[,index])}
  }
  
  result = data.frame(xx)
  colnames(result) = name_groupedcov
  
  return(result)
}

# Training / Testing Cases Division (??? to be organized)

dividing = function(predictor,response,n.train){
  n.cases = length(response)
  resample_index = sample(1:n.train,5000,replace = F)
  training_index = resample_index[1:n.train];testing_index = resample_index[n.train+1:n.cases]
  # 
  xtrain = predictor[training_index,]
  ytrain = response[training_index]
  
  xtest = predictor[testing_index,]
  ytest = response[testing_index]
  
  result = list(xtrain,ytrain,xtest,ytest)
  return(result)
}

##################### Analysis Begins #########################

# ======== Pre-Analysis ==========

# <<Pre-Analysis: Inputting Data>> 
history = read.csv('history_16Jan_unsorted.csv',header=FALSE)
names(history)=c('event_time','user_id','client_application_id', 'action', 'to_id', 'ip')
# <<Pre-Analysis: Structurizing Data>> 
# Step 1: Getting and Cleaning Full Registration Data
getresult1 = GetCleanRegst(history)
d_regist_full=getresult1$data
rate_dup = getresult1$rate.dup

# --- Proceed the following if Registration Hour factor is NOT considered) ---
# Step 2: Subsetting Full Registration Data 
# (Keeping all observations registered early enough so that retention status can be measured)

getresult2 = SubsetRegist(d_regist_full,26000)
d_regist_subset = getresult2$data
n_d_regist_subset = getresult2$n

# Step 3: Getting Corresponding Action Data of Subsetted Registration Data
getresult3 = GetCleanAction(history,d_regist_subset)
d_action_subset = getresult3$data

d_action_subset$time=strtrim(d_action_subset$time,10)
d_action_subset$time = as.POSIXlt(d_action_subset$time)$yday

s_action_subset = getresult3$count.summary
l_subset_multi = getresult3$label

n.subset.action = getresult3$n
n.regist_multi = getresult3$n.multi

# <Analysis> Rate of Having further actions after registration 
rate_multiact = n.regist_multi/length(d_regist_subset$id);rate_multiact

# Step 4: 
getresult4 = StrctData_Multi(d_regist_subset,d_action_subset,l_subset_multi)
d_regist_subset_multi = getresult4$regist; d_action_subset_multi = getresult4$action
d_regist_subset_multi$retention = retent_lable_wk4(d_action_subset_multi,d_regist_subset_multi)

# ---------------- Summary Analysis: Strong/Weak Retention Rate Overview
# rate_retention_subset = sum(l_subset_retention)/length(l_subset_retention)
# rate_multiact_subset = sum(l_subset_multi)/length(l_subset_multi)
# rate_noact_outof_noret = sum(l_subset_multi)/(length(l_subset_retention)-sum(l_subset_retention))

# ======== Pre-Analysis Random Sampling =============
# <Structurize Data>

# getresult4 = GetData_Multi(d_regist_subset,d_action_subset,l_subset_multi)
# d_regist_multi=getresult4$data_regist; d_regist_multi$retention = subset(l_subset_retention,l_subset_multi)
# d_action_multi=getresult4$data_action 

# check: which(d_action_multi$time_elps<0); which(d_regist_multi$id != unique(d_action_multi$id))

# <Getting y>
retention = as.factor(d_regist_subset_multi$retention)

index_Ret = which(retention=='1');n.random2 = length(index_Ret)
# index_cases = sort(c(sample(which(retention=='0'),n.random2),index_Ret))
index_notRet = which(retention=='0')
index_cases = sort(c(index_notRet[which(rbinom(length(index_notRet),1,0.09)==1)],index_Ret))

y = retention[index_cases] # check: length(which(y=='0'))/length(which(y=='1'))

# <Getting x> (Action Records Before 21st day since registration)
# d = subset(d_action_subset_multi,d_action_subset_multi$time_elps<21)
d = subset(d_action_subset_multi,d_action_subset_multi$id%in%d_regist_subset_multi$id[index_cases] & d_action_subset_multi$time_elps<21)
s_x = table(d$id,d$action)

# Grouping x
xx = colnames(s_x); xx=xx[c(-27,-16)]
x1 = c('photo_added_to_gallery','photo_comment','photo_published','photo_remove_vote','photo_upload',
       'photo_vote','follow_user')
x2 = c('group_discussion_created','group_photo_discussion_deleted')

# X as count predictor
# x_count = rft_action_count(s_x,as.list(xx),xx)

x_count = rft_action_count(s_x,as.list(x1),x1)

# X as binary factor
# x_binary = rft_action_binary(s_x,list(x1,x2),c('Pos','Neg'))
x_binary = rft_action_binary(s_x,as.list(xx),x1)

# X as frequency predictor
# x_freq = rft_action_freq(s_x,list(x1,x2),c('Pos','Neg')) # check: which(x_freq[[1]]>1 | x_freq[[1]]<0)
x_freq = rft_action_freq(s_x,as.list(xx),xx)

# <Analysis> Count Factor

# dd=dividing(y,x_count)

aa = rbinom(1:length(y),1,0.8)==1
training_index = sort(which(aa==1));testing_index = sort(which(aa==0))

xxforall = x_count
xtrain = xxforall[training_index,]
ytrain = y[training_index]

xtest = xxforall[testing_index,]
ytest = y[testing_index]

fit_xcount = glm(ytrain~.,data=xtrain,family = binomial(link='logit')); summary(fit_xcount)

# pred.logit=predict.glm(fit_xcount,blcd_xtest)
pred.logit=predict.glm(fit_xcount,xtest)
pred.class=round(exp(pred.logit)/(1+exp(pred.logit)))
# sensitivity = length(which(pred.class==1&blcd_ytest=='1'))/length(which(blcd_ytest=='1'));sensitivity
# specificity = length(which(pred.class==0&blcd_ytest=='0'))/length(which(blcd_ytest=='0'));specificity
sensitivity = length(which(pred.class==1&ytest=='1'))/length(which(ytest=='1'));sensitivity
specificity = length(which(pred.class==0&ytest=='0'))/length(which(ytest=='0'));specificity


library(randomForest)
# fit_xcount2 = randomForest(y=ytrain,x=xtrain,ytest=blcd_ytest,xtest=blcd_xtest);fit_xcount2
fit_xcount2 = randomForest(y=ytrain,x=xtrain,ytest=ytest,xtest=xtest);fit_xcount2

importance(fit_xcount2)

# <Analysis> Frequency Factor
fit_xfreq = glm(retention~.,data=x_freq,family = binomial(link='logit')); summary(fit_xfreq)
fit_xfreq2 = randomForest(y=retention,x=x_freq);fit_xfreq2


# <Analysis> Binary Factor | Not Recommended for Action
fit_xbinary = glm(retention~.,data=x_binary,family = binomial(link='logit')); summary(fit_xbinary)
fit_xbinary2 = randomForest(y=retention,x=x_binary); fit_xbinary2

# ##### The following for Relationship Exploration of Retention VS Registration Time (with Hour Factor)
# library(MASS)
# contigency = table(d_regist_multi$retention, as.factor(d_regist_multi$time$hour));summary(contigency)
# ratio = round(contigency[2,]/colSums(contigency),digits=2) # Retention Rate Among MultiAct New Users
# 
# print(ratio[order(ratio,decreasing = TRUE)])
# plot(ratio,type='s',ylab = 'Ratio',xlab='Registration Hour', main = 'Retention ratio for MultiAct User')
# # <Questions> Grouping Hours?
# ####### stats Analysis

# # Registration Hour Factor
# # (Try different Reference Level of Hour Factor)
# hour_regist = as.factor(d_regist_subset$time$hour)
# fit1 = glm(retention ~ hour_regist, family = binomial(link = 'logit'))
# fit2 = glm(retention ~ relevel(hour_regist,ref='6'), family = binomial(link = 'logit'))

# # Time Factor Reconstruction
# workingtime=rep('Working Time',n.regist_full)
# d=data.frame(hour=d_regist$hour,day=data_regist_full$weekday)
# workingtime[which(d$hour%in%c(0:7,19:23)|d$day%in%c('Saturday','Sunday'))]='Non-Working Time'
# barplot(table(data_regist_full$multiact,workingtime),col=c('lightgrey','pink'),main='NoAct/MultiAct Distribution')
# 
# summary(table(data_regist_full$multiact,workingtime))
# pie(table(data_regist_full$multiact),col = c('grey','pink'),labels=c('No Actions','Multiple Actions'),main = 'Distribution: NoAct/Act Since Registration')
# 
# hist(data_regist_full$hour)
# 
# barplot(table(data_regist_full$multiact, data_regist_full$hour), main="MultiAct Distribution by Hours ",
#         xlab="Registration Hour", col=c("lightgrey","pink"),legend = c('Having No Further Actions since Registration','Having No Further Actions since Registration'))   # grey stands for No Act, pink Stands for MultiAct
# plot(data_regist_full$hour)
# 
# count=table(data_regist_full$multiact, data_regist_full$weekday)
# barplot(count[,c(2,6,7,5,1,3,4)], main="MultiAct Distribution by Weekdays ",col=c("lightgrey","pink"),xlab="Registration Weekdays",ylab = 'count')   # grey stands for No Act, pink Stands for MultiAct

###################################################################


###########
#######The Following Are all for MultiAct Users
###########

# Registration 


# 


retention=as.factor(d_regist_multi$retention)

d_action_1day = subset(d_action_multi,d_action_multi$time_elps<1)

summary.action.1day = table(d_action_1day$id,d_action_1day$action) # cannot use data.frame
action.list=colnames(summary.action.1day); n.action.list=length(action.list)
binary_action_1day = summary.action.1day>1

data_action_2day = d_action_multi[which(d_action_multi$time_elps==2),]
summary.action_2day = table(data_action_2day$id,data_action_2day$action)
binary_action_2day = summary.action_2day>1
label_return_2day = d_regist_multi$id%in%unique(data_action_2day$id)

fit3=glm(relevel(retention,ref=TRUE) ~ label_return_2day,family=binomial(link='logit'))
rate_ret2day = length(which (retention=='1' & label_return_2day == T))/sum(as.numeric(label_return_2day))
length(which (retention=='1' & label_return_2day == T))/sum(as.numeric(retention)-1)

summary(fit3)

data_action_bf21day = d_action_multi[which(d_action_multi$time_elps<21),]
summary.action_bf21day = table(data_action_bf21day$id,data_action_bf21day$action)
binary_action_bf21day = summary.action_bf21day>1
frequency_action_bf21day = summary.action_bf21day/rowSums(summary.action_bf21day)

retention

fit = glm(as.factor(retention)~frequency_action_bf21day,family = binomial(link='logit'))
summary(fit)

# check: which(as.numeric(rownames(summary.action.1day))!=data_regist_multiact$id)

fit=randomForest(retention ~ label_return_2day)
summary(fit)

fit2=randomForest(retention ~., data=rft_action_predictor(binary_action_bf21day))
summary(fit2)



importance(fit2)



fitsummary.action_data_multiact=table(data_action_multiact$id,data_action_multiact$action)
action.list=colnames(summary.action_data_multiact)
index_login=which(action.list=='login');index_login



freq_action=summary.data_action_multiact/rowSums()
binary_action=as.numeric(summary.data_action_multiact>0)

