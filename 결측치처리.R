library(tidyverse)
library(data.table)
library(magrittr)
setwd("C:/Users/wlsgy/OneDrive/바탕 화면/백신 주제")

train_x <- fread('training_set_features.csv',
                        header=TRUE, 
                        stringsAsFactors = F, 
                        data.table = F)
train_y <- fread('training_set_labels.csv',
                        header=TRUE, 
                        stringsAsFactors = F, 
                        data.table = F)
train <- cbind(train_x, train_y[,2:3])

train %>% str()
train %>% dim()
#NA 개수
train %>% is.na %>% colSums()
#NA 비율
na.percentage <- data.frame(vars=names(train),
                       na = apply(train, 2, function(x) sum(is.na(x)))) %>% 
  mutate(perc = na/nrow(train)) %>% select(perc)
na.percentage
na.count <- apply(train, 2, function(x) sum(is.na(x)))
na.count

############
##재범주화##
############
library(plyr)

# age_group
train$age_group <- train$age_group %>% 
  revalue(c('18 - 34 Years' = 0, '35 - 44 Years' = 1, '45 - 54 Years' = 2, '55 - 64 Years' = 3, '65+ Years' = 4))

# education
train$education <- train$education %>% 
  revalue(c('< 12 Years' = 0, '12 Years' = 1, 'Some College' = 2, 'College Graduate' =3))

# income_poverty
train$income_poverty <- train$income_poverty %>% 
  revalue(c('Below Poverty' = 0, '<= $75,000, Above Poverty' = 1, '> $75,000' = 2))

# employment_industry
distinct(train, employment_industry)

train$employment_industry <- train$employment_industry %>% 
  revalue(c('pxcmvdjn' = 'A', 'rucpziij' = 'B', 'wxleyezf' = 'C', 'saaquncn' = 'D', 'xicduogh' = 'E',
            'ldnlellj' = 'F', 'wlfvacwt' = 'G', 'nduyfdeo' = 'H', 'fcxhlnwr' = 'I', 'vjjrobsf' = 'J',
            'arjwrbjb' = 'K', 'atmlpfrs' = 'L', 'msuufmds' = 'N', 'xqicxuve' = 'M', 'phxvnwax' = 'O',
            'dotnnunm' = 'P', 'mfikgejo' = 'Q', 'cfqqtusy' = 'R', 'mcubkhph' = 'S', 'haxffmxo' = 'T',
            'qnlwzans' = 'U'))

# employment_occupation
distinct(train, employment_occupation)

train$employment_occupation <- train$employment_occupation %>% 
  revalue(c('xgwztkwe' = 'a', 'xtkaffoo' = 'b', 'emcorrxb' = 'c', 'vlluhbov' ='d', 'xqwwgdyp' = 'e',
            'ccgxvspp' = 'f', 'qxajmpny' = 'g', 'kldqjyjy' ='h', 'mxkfnird' = 'i', 'hfxkjkmi' = 'j',
            'bxpfxfdn' = 'k', 'ukymxvdu' = 'l', 'cmhcxjea' = 'n', 'haliazsg' = 'm', 'dlvbwzss' = 'o',
            'xzmlyyjv' = 'p', 'oijqvulv' = 'q', 'rcertsgn' = 'r', 'tfqavkke' = 's', 'hodpvpew' = 't',
            'uqqtjvyb' = 'u', 'pvmttkik' = 'v', 'dcjcmpih' = 'w'))

# hhs_geo_region
train$hhs_geo_region <- train$hhs_geo_region %>% 
  revalue(c('oxchjgsf' = 'Region1', 'bhuqouqj' = 'Region2', 'qufhixun' = 'Region3', 'lrircsnp' = 'Region4',
            'atmpeygn' = 'Region5', 'lzgpxyit' = 'Region6', 'fpwskwrf' = 'Region7', 'mlyzmhmf' = 'Region8',
            'dqpwygqj' = 'Region9', 'kbazzjca' = 'Region10'))

train %<>% mutate(employment_status = ifelse(train$employment_status == "", "Non Response", train$employment_status))
train %<>% mutate(employment_industry = ifelse(train$employment_industry == "", "Non Response", train$employment_industry))
train %<>% mutate(employment_occupation = ifelse(train$employment_occupation == "", "Non Response", train$employment_occupation))
train %<>% mutate(income_poverty = ifelse(train$income_poverty == "", "Non Response", train$income_poverty))
train %<>% mutate(education = ifelse(train$education == "", "Non Response", train$education))
train %<>% mutate(marital_status = ifelse(train$marital_status == "", "Non Response", train$marital_status))
train %<>% mutate(rent_or_own = ifelse(train$rent_or_own == "", "Non Response", train$rent_or_own))


no_NA <- function(x) replace(x, is.na(x), "Non Response")
train<-apply(train, 2, no_NA)
train%<>%as.data.frame()

write.csv(train, file = "final_train.csv", row.names = FALSE)

new_train <- fread('final_train.csv',
                 header=TRUE, 
                 stringsAsFactors = F, 
                 data.table = F)
###################
###NA imputation###
###################
library(mice)
library(VIM)
aggr(train,prop=FALSE,numbers=TRUE, col = c('brown', 'dark blue'))
na.count[which.max(na.count)]
#근데 우리 데이터는 MNAR라고 할 수 있지 않을까. ...?/

# NA 그냥 삭제하는 방법...!
train.without.na <- train %>% na.omit
train.without.na %>% dim()
apply(train.without.na, 2, function(x) sum(is.na(x)))
train.without.na %>% select(contains('behavioral')) %>% 
  gather("behavioral", "value") %>% 
  ggplot() +
  geom_bar(aes(x=factor(value)), fill = 'dark blue', alpha = 0.7) +
  facet_wrap(~behavioral) +
  theme_light()

#MICE
mice_train <- mice(data=train, m=5, method='cart',seed=500)
mice.cart.train <- complete(mice_train)
write.csv(mice.cart.train, file="mice_cart_train.csv", row.names=FALSE)

mice_train <- fread('mice_cart_train.csv',
                        header=TRUE, 
                        stringsAsFactors = F, 
                        data.table = F)
mice_train %>% select(contains('behavioral')) %>% 
  gather("behavioral", "value") %>% 
  ggplot() +
  geom_bar(aes(x=factor(value)), fill = 'dark blue', alpha = 0.7) +
  facet_wrap(~behavioral) +
  theme_light()

#Deterministic regression imputation
imp <- mice(data= train, method = "norm.predict", m = 1)
reg_train <- complete(imp)
write.csv(reg_train, file = "reg_train.csv", row.names = FALSE)
reg_train <- fread('reg_train.csv',
                             header=TRUE, 
                             stringsAsFactors = F, 
                             data.table = F)


#Hotdeck imputation
hotdeck_train<-hotdeck(train)
hotdeck_train %>% head()
write.csv(hotdeck_train, file = "hotdeck_train.csv", row.names = FALSE)
hotdeck_train <- fread('hotdeck_train.csv',
                             header=TRUE, 
                             stringsAsFactors = F, 
                             data.table = F)

########################
########시각화##########
########################
behavioral_train <- train %>% select(contains("behavioral"), h1n1_vaccine,
                                     seasonal_vaccine)
behavioral_train %>% str()
#binary data 시각화 찾아보고
#VIM으로 NA 파악하는 시각화
library(VIM)
aggr(behavioral_train, prop=FALSE, numbers=TRUE, 
     col = c("#6397D6", "#D199F2"))

behavioral_train <- train %>% select(contains('behavioral'), employment_status)

behavioral_train %>% gather(-employment_status, key = 'behavioral', value = 'value') %>% 
  ggplot(aes(x = factor(value), fill = employment_status)) +
  geom_bar()+ facet_wrap(~behavioral) +theme_bw()

behavioral_train2 <- train %>% select(contains('behavioral'), sex)

behavioral_train2 %>% gather(-sex, key = 'behavioral', value = 'value') %>% 
  ggplot(aes(x = factor(value), fill = sex)) +
  geom_bar()+ facet_wrap(~behavioral) +theme_bw()

behavioral_train3 <- train %>% select(contains('behavioral'), h1n1_vaccine)
behavioral_train3 %>% gather(-h1n1_vaccine, key = 'behavioral', value = 'value') %>% 
  ggplot(aes(x = factor(value), fill = factor(h1n1_vaccine))) +
  geom_bar(position = "fill")+ facet_wrap(~behavioral) +theme_bw()

behavioral_train4 <- train %>% select(contains('behavioral'), seasonal_vaccine)
behavioral_train4 %>% gather(-seasonal_vaccine, key = 'behavioral', value = 'value') %>% 
  ggplot(aes(x = factor(value), fill = factor(seasonal_vaccine))) +
  geom_bar(position = "fill")+ facet_wrap(~behavioral) +theme_bw()

train %>% select(contains('behavioral')) %>% 
  gather("behavioral", "value") %>% 
  ggplot() +
  geom_bar(aes(x=factor(value)), fill = '#9795DB') +
  facet_wrap(~behavioral) +
  theme_light()

train %>% 
  group_by('behavioral') %>% 
  ggplot(aes(x  = 'behavioral'))+geom_bar(aes(fill = employment_status))


##################
#####Sampling#####
##################
library(ROSE)
library(rpart)

# over sampling 아님 ... 그냥 무작위로 해본거거
h1n1_both <- ovun.sample(h1n1_vaccine~., data = h1n1_train, method = "both", p=0.5,  N=2000)$data

train_both <- ovun.sample(h1n1_vaccine~., data = train, method = "both", p=0.5,  N=20000)$data

train_both2 <- ovun.sample(seasonal_vaccine~., data = train_both, method = "both", p=0.5,  N=3000)$data

# y변수 분포
train_both2 %>% select(h1n1_vaccine, seasonal_vaccine) %>% 
  gather("vaccine", "value") %>% 
  ggplot() +
  geom_bar(aes(x=factor(value))) +
  facet_wrap(~vaccine) +
  theme_light() 


train_both %>% select(h1n1_vaccine, seasonal_vaccine) %>% 
  gather("vaccine", "value") %>% 
  ggplot() +
  geom_bar(aes(x=factor(value))) +
  facet_wrap(~vaccine) +
  theme_light() 

train_both2 %>% select(contains('behavioral')) %>% 
  gather("behavioral", "value") %>% 
  ggplot() +
  geom_bar(aes(x=factor(value)), fill = '#6397D6') +
  facet_wrap(~behavioral) +
  theme_light()

train %>% select(contains('behavioral')) %>% 
  gather("behavioral", "value") %>% 
  ggplot() +
  geom_bar(aes(x=factor(value)), fill = '#6397D6') +
  facet_wrap(~behavioral) +
  theme_light()

train %>% select(contains('opinion')) %>% 
  gather("opinion", "value") %>% 
  ggplot() +
  geom_bar(aes(x=factor(value)), fill = '#9795DB') +
  facet_wrap(~opinion) +
  theme_light()


########################
#Sample Size Gridsearch#
########################
library(ROSE)
sample_size<-expand.grid(N1 = seq(15000, 25000, 1000), 
                         N2 = seq(15000, 25000, 1000),
                        size_diff_h1n1 = NA, size_diff_seasonal = NA)


for(i in 1:nrow(sample_size)){
  train_both <- ovun.sample(h1n1_vaccine~., data = train,
                            method = 'both', p = 0.5, 
                            N = sample_size[i,1])$data
  train_both2 <- ovun.sample(seasonal_vaccine~., data = train_both,
                             method = 'both', p = 0.5, 
                             N = sample_size[i,2])$data
  sample_size$size_diff_h1n1[i] = abs(sum(train_both2$h1n1_vaccine == 1)
                                      - sum(train_both2$h1n1_vaccine ==0))
  sample_size$size_diff_seasonal[i] = abs(sum(train_both2$seasonal_vaccine == 1)
                                          - sum(train_both2$seasonal_vaccine == 0))
}

sample_size[which.min(sample_size$size_diff_h1n1),]
sample_size[which.min(sample_size$size_diff_seasonal),]


sample_size$diff_sum = sample_size$size_diff_h1n1 + sample_size$size_diff_seasonal

sample_size[which.min(sample_size$diff_sum),]

##optimal sample size 19000, 16000
train_both <- ovun.sample(h1n1_vaccine~., data = train, 
                          method = "both", p=0.5,  N=19000)$data
train_both2 <- ovun.sample(seasonal_vaccine~., data = train_both, 
                           method = "both", p=0.5,  N=16000)$data


train %>% select(h1n1_vaccine, seasonal_vaccine) %>% 
  gather("vaccine", "value") %>% 
  ggplot() +
  geom_bar(aes(x=factor(value)), fill = '#9795DB') +
  facet_wrap(~vaccine) +
  theme_light()

train_both2 %>% select(h1n1_vaccine, seasonal_vaccine) %>% 
  gather("vaccine", "value") %>% 
  ggplot() +
  geom_bar(aes(x=factor(value)), fill = '#9795DB') +
  facet_wrap(~vaccine) +
  theme_light() 

#원래 분포 !
train %>% select(h1n1_vaccine, seasonal_vaccine) %>% 
  gather("vaccine", "value") %>% 
  ggplot() +
  geom_bar(aes(x=factor(value)), fill = 'dark blue') +
  facet_wrap(~vaccine) +
  theme_light() 

######################
######clustering######
######################

##############
#####h1n1#####
##############
lasso_variable = c("h1n1_concern","h1n1_knowledge","behavioral_antiviral_meds","behavioral_avoidance",
"behavioral_face_mask","behavioral_wash_hands","behavioral_large_gatherings",
"behavioral_outside_home","behavioral_touch_face","doctor_recc_h1n1",
"doctor_recc_seasonal","chronic_med_condition","child_under_6_months",
"health_insurance","opinion_h1n1_risk","opinion_seas_risk",
"opinion_seas_sick_from_vacc","household_children")

stepwise_variable = c("h1n1_knowledge", "behavioral_antiviral_meds",
  "behavioral_face_mask","behavioral_large_gatherings","doctor_recc_h1n1",
  "doctor_recc_seasonal","chronic_med_condition", "child_under_6_months","health_worker",
  "health_insurance", "opinion_h1n1_vacc_effective", "opinion_h1n1_risk",  "opinion_h1n1_sick_from_vacc",
  "opinion_seas_vacc_effective","opinion_seas_risk","opinion_seas_sick_from_vacc",
  "age_group","education","race", "sex","income_poverty", "rent_or_own",
  "employment_status","hhs_geo_region","census_msa","household_adults",
  "employment_industry")
common_variable = lasso_variable %in% stepwise_variable
common_variable = lasso_variable[c(2,3,4,5,7,10,11, 12, 13, 14, 15, 16, 17)]
common_variable
library(GoodmanKruskal) 
train_common=GKtauDataframe(train[,common_variable])
plot(train_common)
common_variable = common_variable[-c(6:7)]

## 거리계산
library(caret)
index = createDataPartition(train$h1n1_vaccine, p = 0.5)
train_cluster = train[index$Resample1,common_variable]
library(cluster)
dis = daisy(train_cluster,metric=c('gower')) 
summary(dis)

#군집 개수 지정
sil_width <- c(NA)
for(i in 2:12){  
  pam_fit <- pam(dis, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:12, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:12, sil_width)

#클러스터링
pam_fit <- pam(dis, diss = TRUE, 3)
pam_results <- train_cluster %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary


library(Rtsne)
tsne_obj <- Rtsne(dis, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) 

train_label = train[index$Resample1,] %>% select("h1n1_vaccine")
result_pam <- cbind(pam_fit$clustering, train_cluster)
result_pam <- cbind(result_pam, train_label)
result_pam %>% head()


result_pam %>% 
  ggplot() +
  geom_bar(aes(x=`pam_fit$clustering`, fill = factor(health_insurance)),
           position="fill")

##################
#####seasonal#####
##################

stepwise_seasonal = c("h1n1_concern", "h1n1_knowledge", "behavioral_antiviral_meds",
                        "behavioral_touch_face","doctor_recc_h1n1", "doctor_recc_seasonal", 
                        "chronic_med_condition", "child_under_6_months", "health_worker",
                        "health_insurance", "opinion_h1n1_vacc_effective","opinion_h1n1_risk",
                        "opinion_h1n1_sick_from_vacc", "opinion_seas_vacc_effective", 
                        "opinion_seas_risk","opinion_seas_sick_from_vacc", "age_group",
                        "education", "race", "income_poverty", "marital_status", "rent_or_own",
                        "employment_status", "hhs_geo_region", "census_msa", "household_children",
                        "employment_industry", "employment_occupation" )
length(stepwise_seasonal)
lasso_seasonal = c("h1n1_concern", "h1n1_knowledge", "behavioral_antiviral_meds",
                   "behavioral_touch_face", "doctor_recc_h1n1", "doctor_recc_seasonal",
                   "child_under_6_months", "health_worker", "opinion_seas_vacc_effective",
                   "opinion_seas_risk", "opinion_seas_sick_from_vacc", "age_group",
                   "education", "race", "income_poverty", "rent_or_own","hhs_geo_region",
                   "household_children")
length(lasso_seasonal)

common_seasonal = lasso_seasonal %in% stepwise_seasonal
common_seasonal = lasso_seasonal
library(GoodmanKruskal) 
train_seasonal=GKtauDataframe(train[,common_seasonal])
plot(train_seasonal)
common_seasonal = common_seasonal[-c(5:6)]
common_seasonal
## 거리계산
library(caret)
index_seasonal = createDataPartition(train$seasonal_vaccine, 
                                     p = 0.3)
train_cluster2 = train[index_seasonal$Resample1,common_seasonal]
library(cluster)
dis = daisy(train_cluster2,metric='gower') 
summary(dis)

#군집 개수 지정
sil_width <- c(NA)
for(i in 2:18){  
  pam_fit <- pam(dis, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:18, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:18, sil_width)

#클러스터링
pam_fit <- pam(dis, diss = TRUE, 3)
pam_results <- train_cluster %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

library(Rtsne)
tsne_obj <- Rtsne(dis, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) 

train_label = train[index$Resample1,] %>% select("h1n1_vaccine")
result_pam <- cbind(pam_fit$clustering, train_cluster)
result_pam <- cbind(result_pam, train_label)
result_pam %>% head()


result_pam %>% 
  ggplot() +
  geom_bar(aes(x=`pam_fit$clustering`, fill = factor(health_insurance)),
           position="fill")