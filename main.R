if (!require("pacman")) install.packages("pacman") 
#{pacman} is used for its convenience

pacman::p_load(caret
               ,dplyr
               ,e1071
               ,Hmisc
               ,lubridate
               ,ggplot2
               ,StanHeaders
               ,prophet
               ,gridExtra
               ,caret
               ,arules
               ,arulesViz
               ,corrplot
               ,magrittr
               ,plotly
               ,splitstackshape
               ,randomForest
)

source("Utils.R")
options(digits.secs = 3) 
options(scipen = 999)

###Read Data
 
df <- read.delim(".../DataSet.txt", header = TRUE, sep = "\t", dec = ".") %>% 
  dplyr::mutate(Transaction_Timestamp = strptime(Transaction_Timestamp, "%Y-%m-%d %H:%M:%OS"))
 
str(df)

# Generate New Columns

df <- df %>%
  dplyr::arrange(Account_ID, Transaction_Timestamp) %>%
  dplyr::mutate(Hour = as.numeric(as.factor(format(Transaction_Timestamp,'%H'))),
         wDay = Transaction_Timestamp$wday,
         Day = as.numeric(as.factor(format(Transaction_Timestamp,'%d')))) %>%
  utilGrouping()

temp1 <- df %>%
  dplyr::group_by(groups) %>%
  dplyr::summarise(Total_TGroup = n()) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(df,by=c('groups'))

temp2 <- df %>%
  dplyr::filter(Transaction_Status=='Approved')%>%
  dplyr::group_by(groups) %>%
  dplyr::summarise(Total_TGroup_App = n()) %>%
  dplyr::ungroup()

df <- dplyr::left_join(temp1,temp2,by="groups")

df <- df %>%
  dplyr::arrange(Account_ID, Transaction_Timestamp) %>%
  dplyr::group_by(groups) %>%
  dplyr::mutate(timediff = lubridate::as.duration(lag(Transaction_Timestamp) %--% Transaction_Timestamp)) %>%
  dplyr::ungroup() %>%
   dplyr::mutate(timediff_sec = as.numeric(timediff))

df <- df %>%
  group_by(groups) %>% 
  arrange(Transaction_Timestamp)%>% 
  mutate(Flag_A = factorChange(Factor_A),
         Flag_B = factorChange(Factor_B),
         Flag_D = factorChange(Factor_D),
         Flag_E = factorChange(Factor_E))

# Group Factor_E based on Factor_C and approval ratio (0-0.2 0.2-0.66 0.66-0.96 0.96-1.0)

df <- df %>% mutate(Factor_E = stringr::str_replace_all(Factor_E, c('MCE' = 'Group1','MPX'="Group1",
                                                                    'MCU'="Group2",'MDU'="Group2",'MIU'="Group2",'MBK'="Group2",'MEO'="Group2",
                                                                    'MPF'="Group2",'MPV'="Group2",'MPG'="Group2",'MHH'="Group2",'MRG'="Group2",
                                                                    'MPO'="Group3",'MUS'="Group3",'MPT'="Group3",'MPM'="Group3",'MCP'="Group3",
                                                                    'MPY'="Group3",'MCO'="Group3",'MBD'="Group3",'MWB'="Group3",'MGF'="Group3",
                                                                    'G1'="Group4",'J3'="Group4",'J4'="Group4",'K'="Group4",
                                                                    'MCT'="Group5",'MDH'="Group5",'MLA'="Group5",'MNW'="Group5",'MPN'="Group5"))) 

#Are variables changing within billing series? (dynamic or static variable?)

isDynamic(df, df$Factor_A) # returns number of changing rows
isDynamic(df, df$Factor_B) # returns number of changing rows
isDynamic(df, df$Factor_C) # returns number of changing rows
isDynamic(df, df$Factor_D) # returns number of changing rows
isDynamic(df, df$Factor_E) # returns number of changing rows


# Association Rule Mining 

aRulesDf <- df %>% select(Factor_A,Factor_B,Factor_D,Factor_E,Transaction_Status,Hour,wDay,Day) %>% 
  mutate_all(.funs = as.factor) %>% 
  ungroup() %>% 
  arules::transactions()

aRules <- apriori(data=aRulesDf, 
                  parameter=list (supp=0.001, conf = 0.01, minlen=3), 
                  appearance = list (rhs="Transaction_Status=Declined"))

inspect(head(sort(aRules, by = "confidence"), 100))

plot(aRules[1:20],method = 'graph', control = list(type = "items"))

# Plots

temp1 <- df %>% 
  group_by(Day) %>% 
  summarise(n_total=n()) %>% 
  ungroup()

temp2 <- df %>% 
  filter(Transaction_Status=="Declined") %>% 
  group_by(Day) %>% 
  summarise(n_declined=n()) %>% 
  left_join(temp1,by=c('Day')) 

temp2 <- temp2 %>% 
  mutate(ratio = n_declined/n_total)

temp2$wDay<-c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday", "Saturday")[temp2$wDay + 1]

level_order <- c('Monday', 'Tuesday', 'Wednesday','Thursday', 'Friday','Saturday', 'Sunday')

daytemp <- temp2 %>% 
  ungroup() %>% 
  group_by(Day) %>% 
  summarise(ratio = sum(n_declined)/sum(n_total),n_total)

ggplot(daytemp, aes(x=Day, y=ratio, group=1)) +
  geom_line(color="steelblue") + 
  xlab("Days")+
  ylab('Decline Ratio')+ ggtitle('Decline Ratios for Each Days in a Month')
  #+ggrepel::geom_label_repel(aes(label = round(n_total/sum(n_total),3)), nudge_y = .002, size = 2)

plot1 <- ggplot(temp2, aes(Day,Hour, fill= ratio)) + 
  geom_tile() + 
  scale_fill_distiller(palette = 'GnBu') +
  scale_x_continuous(breaks = c(1:31))+
  scale_y_continuous(breaks = c(1:24))+
  xlab('Days in A Month') +
  ylab('Transaction Hour') +
  ggtitle('Decline Ratios for Hour/Day') +
  labs(fill = 'Decline Ratio')
plotly::ggplotly(plot1)

# Trend Analysis and fbprophet

temp1 <- df %>%
  mutate(Date = as.Date(format(Transaction_Timestamp,"%Y-%m-%d"))) %>% 
  group_by(Date) %>%
  summarise(Total_Transactions = n()) %>% 
  ungroup()

temp2 <- df %>% 
  mutate(Date = as.Date(format(Transaction_Timestamp,"%Y-%m-%d"))) %>% 
  filter(Transaction_Status=="Declined") %>% 
  group_by(Date) %>% 
  summarise(n_declined=n()) %>% 
  left_join(temp1,by='Date') %>% 
  mutate(Decline_Ratio = n_declined/Total_Transactions) 

temp3 <- df %>%
  distinct(Account_ID,.keep_all = TRUE)%>% 
  mutate(Date=as.Date(format(Transaction_Timestamp,"%Y-%m-%d"))) %>%
  group_by(Date) %>%
  summarise(Total_Unique_Users = n()) %>% 
  left_join(temp1,by='Date')

ggplot(temp2, aes(x=Date, y=Decline_Ratio)) +
  geom_line(color="steelblue") + 
  xlab("Days") +
  ylab('Decline Ratio') +
  geom_smooth(method = 'loess')

# Total Amount of Transactions

temp3_long <- data.table::melt(temp3, id="Date") 

ggplot(data=temp3_long,
       aes(x=Date, y=value, colour=variable)) +
  geom_line() +
  geom_smooth(method = "loess")

# Projection

## Total Transactions
transactionProj <- rename(temp2, y=Total_Transactions, ds=Date) %>% 
  select(ds,y)

prophetModel <- prophet(weekly.seasonality = TRUE,
            seasonality.mode = 'additive') %>% 
  prophet::add_country_holidays(country_name = 'US')%>% 
  prophet::fit.prophet(transactionProj)

future <- make_future_dataframe(prophetModel, periods = 30)
forecast <- predict(prophetModel, future)
plot(prophetModel, forecast)
prophet_plot_components(prophetModel, forecast)

##Total Unique Customers
customerProj <- rename(temp2, y=Total_Unique_Users, ds=Date) %>% select(ds,y)

prophetModel <- prophet(weekly.seasonality = TRUE,
            seasonality.mode = 'additive') %>% 
  prophet::add_country_holidays(country_name = 'US')%>% 
  prophet::fit.prophet(customerProj)

future <- make_future_dataframe(prophetModel, periods = 30)
forecast <- predict(prophetModel, future)
plot(prophetModel, forecast)
prophet_plot_components(prophetModel, forecast)


# Filter only billing series

dfBilling <- df %>% 
  mutate(Total_TGroup_App = tidyr::replace_na(Total_TGroup_App,0)) %>% 
  group_by(groups) %>% 
  filter(Total_TGroup != 1 & Total_TGroup != Total_TGroup_App) %>% 
  ungroup()

# Billing Series Plot

temp1 <- dfBilling %>% 
  filter(!is.na(timediff_sec)) 

plot1 <- ggplot() + 
  geom_histogram(data = temp1, aes(x=timediff_sec),binwidth = 3600)+
  scale_x_continuous(breaks=seq(0, 200000, 12*3600))+
  labs(title="Plot of Recycling Time", x ="Seconds", y = "Frequency")

plotly::ggplotly(plot1)

temp2 <- dfBilling %>% 
  filter(!is.na(timediff_sec),
         Total_TGroup_App == 1) %>% 
  group_by(groups) %>% 
  summarise(time_until_success = sum(timediff_sec)) %>% 
  ungroup() 

plot2 <- ggplot() + 
  geom_histogram(filter(t2,Response.y %in% c(1240,1320,1330,1100)), mapping = aes(x=time_until_success, fill=as.factor(Response.y)),binwidth = 3600)+
  scale_x_continuous(breaks=seq(0, 200000, 12*3600))+
  scale_fill_manual(values=c("#69b3a2", "#404080",'red', 'green')) + 
  facet_wrap( ~ as.factor(Response.y), ncol=1)
labs(title="Plot of Time to Success", x ="Seconds", y = "Frequency")

ggplotly(plot2)

temp1$Type <- 'Attempt Period'
temp2$Type <- 'Time to Success'
temp1 <- rename(temp1, time = timediff_sec) %>% select(Type, time)
temp2 <- rename(temp2, time = time_until_success) %>% select(Type,time)
tempAll<-rbind(temp1,temp2)


plot3 <- ggplot() + 
  geom_histogram(data = tempAll %>% filter(Type == "Attempt Period"), aes(x=time,fill=Type), binwidth = 3600)+
  scale_x_continuous(breaks=seq(0, 200000, 12*3600))+
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  geom_histogram(data = tempAll %>% filter(Type == "Time to Success"), aes(x=time,fill=Type), binwidth = 3600)+
  scale_x_continuous(breaks=seq(0, 200000, 12*3600))+
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(title="Plot of Billing Series Time Period", x ="Seconds", y = "Frequency")

plotly::ggplotly(plot3)


# Modelling

# Random Forest Model ---- NON-SEQUENTIAL

dfNonSeq <- df %>% 
  ungroup() %>% 
  select(Account_ID,
         Factor_A 
         ,Factor_B
         ,Factor_D 
         ,Factor_E
         ,Transaction_Status
         ,Day
         ,wDay
  ) %>%  
  mutate_all(.funs = as.factor) %>% 
  bind_cols(select(df,Hour)) %>% 
  mutate(Hour = sin(2*pi*Hour/24),
         Houry = cos(2*pi*Hour/24))

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dfNonSeq))

## set the seed to make your partition reproducible
#set.seed(123)
train_ind <- sample(seq_len(nrow(dfNonSeq)), size = smp_size)

train <- dfNonSeq[ train_ind,]
test  <- dfNonSeq[-train_ind,]

train <- train %>% filter(!(Account_ID==103413)) 
  
train<- train[sample(nrow(train)),]
test <- test[sample(nrow(test),replace=F),]

train <- splitstackshape::stratified(train, 'Transaction_Status', as.numeric(table(train$Transaction_Status)[2]))

train.acc <- train$Account_ID
train.status<- train$Transaction_Status

test2.acc <- test2All$Account_ID
test2.status<- test2All$Transaction_Status

train<-train %>% 
  mutate(Account_ID=NULL)

test2All<-test2All %>% 
  mutate(Account_ID=NULL,
         Transaction_Status = NULL)

rmModel <- randomForest::randomForest(
  formula = Transaction_Status ~ .,
  data    = train,
  mtry = 3,
  ntreeTry = 300,
  importance= TRUE
)

plot(rmModel)

pred <- predict(rmModel, newdata = test,  predict.all = FALSE, type='prob')

test.status<- as.numeric(as.factor(test.status))

test.status <- replace(test.status, test.status=='A', 1)

test.status <- replace(test.status, test.status==1, 0)

predROC <- prediction(pred[,1], test.status)
perf <- ROCR::performance(predROC,"tpr","fpr")
plot(perf)

## Alternative Prediction ##
single.test <- filter(df, Account_ID == 103413)
test2All <- distinct(single.test[c(1:57),],Day,.keep_all=TRUE)

prob <- rep(0, nrow(test2All))

test2 <- rbind(test[1, ] , test2All)
test2 <- test2[-1,]
test3<- test2 %>% mutate(Hour=test2$Hour[1],
                         Houry=test2$Hour[1])

for (i in c(1:nrow(test2))) {
  pred2 <- predict(rmModel, newdata = test2[i,],  predict.all = TRUE)
  prob[i] <- as.numeric(table(pred2$individual)[1])/(as.numeric(table(pred2$individual)[2])+as.numeric(table(pred2$individual)[1]))
}

dfProb2 <- cbind(as.data.frame(prob),test2$Day) %>% rename(Day = 'test2$Day') %>% mutate(Day = as.numeric(Day),
                                                                                        Status = test2.status)

ggplot(dfProb2, aes(x=Day, y=prob),) +
  geom_line(color="steelblue",stat = 'identity') + 
  xlab("Days")+
  ylab('Probability')+ ggtitle('Probability of Transactions of Account_ID = 3')+
  geom_point(aes(x=Day,y=prob),color = as.factor(as.numeric(dfProb$Status)+5))

####

table(pred$individual)
confusionMatrix(pred, test.status)

randomForest::importance(rmModel,type=1,scale=T)
create_rfplot(rmModel,type = 2)



# Random Forest Model ---- SEQUENTIAL CLASSIFICATION

t <- dfBilling %>% 
  ungroup() %>% 
  arrange(groups,Transaction_Timestamp) %>% 
  group_by(groups) %>% 
  summarise_all(first) %>% 
  select(groups,Response) %>% 
  ungroup()

t1 <- dfBilling %>% 
  group_by(groups,Response) %>% 
  summarise(Tot=n())

t2 <- t1 %>% ungroup() %>% 
  group_by(groups) %>%
  summarise(tot_t= n()) %>% 
  filter(tot_t>2) %>% 
  distinct(groups) %>%
  mutate(Flag_Response=1)

df1 <- left_join(dfBilling, t2, by = 'groups') %>% left_join(t, by='groups')

dfBillingSeq <- df1 %>% arrange(groups,Transaction_Timestamp) %>%
  mutate(Response=lag(Response.x),
         First_Response = Response.y,
         Flag_Response = tidyr::replace_na(Flag_Response,0)) %>% 
  filter(!is.na(timediff_sec)) 


dfSeqCl <- dfBillingSeq %>% 
  ungroup() %>% 
  select(Account_ID
         ,Factor_A
         ,Factor_B
         ,Factor_D 
         ,Factor_E
         ,Response
         ,Day
         ,wDay
         ,Transaction_Status
  ) %>%  
  mutate_all(.funs = as.factor) %>% 
  bind_cols(select(dfBillingSeq
                   ,Hour
                   ,timediff_sec)) %>% 
  mutate(Hour = sin(2*pi*Hour/24),
         Houry = cos(2*pi*Hour/24))

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dfSeqCl))

## set the seed to make your partition reproducible
#set.seed(123)
train_ind <- sample(seq_len(nrow(dfSeqCl)), size = smp_size)

train <- dfSeqCl[ train_ind,]
test  <- dfSeqCl[-train_ind,]


train<- train[sample(nrow(train)),]
test <- test[sample(nrow(test),replace=F),]

train <- stratified(train, 'Transaction_Status', as.numeric(table(train$Transaction_Status)[1]))

train.acc <- train$Account_ID
train.status<- train$Transaction_Status

test.acc <- test$Account_ID
test.status<- test$Transaction_Status

train<-train %>% 
  mutate(Account_ID=NULL)

test<-test %>% 
  mutate(Account_ID=NULL,
         Transaction_Status = NULL)

# names of features
features <- setdiff(names(train), "Transaction_Status")

#set.seed(123)

m2 <- randomForest::tuneRF(
  x          = train[features],
  y          = train$time_until_success,
  ntreeTry   = 500,
  mtryStart  = 2,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)


rmModel <- randomForest::randomForest(
  formula = Transaction_Status ~ .,
  data    = train,
  mtry = 3,
  ntreeTry = 300,
  importance= TRUE
)

plot(rmModel)

pred <- predict(rmModel, newdata = test,  predict.all = FALSE)
caret::confusionMatrix(factor(pred), factor(test.status))

## Alternative Prediction ##
single.test <- filter(dfSeqCl, Account_ID == 3394)

test2All <- distinct(single.test[c(1:57),], Day, .keep_all=TRUE) 
test2.status <- test2All$Transaction_Status
test2All <- mutate (test2All, Account_ID =NULL,Transaction_Status = NULL)

prob <- rep(0, nrow(test2All))

test2 <- rbind(test[1, ] , test2All)
test2 <- test2[-1,]
test3<- test2 %>% mutate(Hour=test2$Hour[1],
                         Houry=test2$Hour[1])

for (i in c(1:nrow(test2))) {
  pred2 <- predict(rmModel, newdata = test2[i,],  predict.all = TRUE)
  prob[i] <- as.numeric(table(pred2$individual)[1])/(as.numeric(table(pred2$individual)[2])+as.numeric(table(pred2$individual)[1]))
}

dfProb2 <- cbind(as.data.frame(prob),test2$Day) %>% rename(Day = 'test2$Day') %>% mutate(Day = as.numeric(Day),
                                                                                         Status = test2.status)

dfProb2$Day<- factor(dfProb2$Day, levels = c("30",'31','1','2','3','4','5','6','7','8','9'))
dfProb2$order <- c(1:nrow(dfProb2))

ggplot(dfProb2, aes(x=Day, y=prob)) +
  geom_line(color="steelblue",stat = 'identity') + 
  xlab("Days")+
  ylab('Probability')+ ggtitle('Probability of Transactions of Account_ID = 3394')+
  geom_point(aes(x=Day,y=prob),color = as.factor(as.numeric(dfProb2$Status)+5),size=3)+
  geom_text(aes(label=order),hjust=2, vjust=0)

####


randomForest::importance(rmModel,type=1,scale=T)
create_rfplot(rmModel, type = 1)

library (ROCR)

y <- as.factor(as.numeric(test.status)) # logical array of positive / negative cases
predictions <- as.factor(as.numeric(pred)) # array of predictions


precision <- posPredValue(predictions, y, positive="1")
recall <- sensitivity(predictions, y, positive="1")

F1N <- (2 * precision * recall) / (precision + recall)


pr <- prediction(predictions, y);

# Recall-Precision curve             
RP.perf <- performance(pr, "prec", "rec");

plot (RP.perf);

# ROC curve
ROC.perf <- performance(pr, "tpr", "fpr");
plot (ROC.perf);

# ROC area under the curve
auc.tmp <- performance(pr,"auc");
auc <- as.numeric(auc.tmp@y.values)

performance(pr,"f")



# Random Forest Model ---- SEQUENTIAL Regression

dfSeqReg <- df0 %>% filter(Total_TGroup_App != 0, Total_TGroup > 1) %>% 
  arrange(groups,desc(Transaction_Timestamp)) %>% 
  mutate(timediff_sec = lag(timediff_sec)) %>% 
  group_by(groups) %>% 
  mutate(time_to_approval = cumsum(tidyr::replace_na(timediff_sec, 0))) %>% 
  arrange(Transaction_Timestamp) %>% 
  mutate(Flag_A = ifelse(factorChange(Factor_A) > 0,1,0),
         Flag_B = ifelse(factorChange(Factor_B) > 0,1,0),
         Flag_D = ifelse(factorChange(Factor_D) > 0,1,0),
         Flag_E = ifelse(factorChange(Factor_E) > 0,1,0),
         Response_F = ifelse(factorChange(Response) > 0,1,0)) %>% 
  filter(!is.na(timediff_sec)) %>% 
  ungroup()

dfSeqReg <- dfSeqReg %>% 
  arrange(groups,Transaction_Timestamp) %>% 
  group_by(groups) %>% 
  summarise_all(first) %>% 
  bind_rows(filter(dfSeqReg, Response_F == 1))
 
dfSeqReg <- dfSeqReg %>%  
  select(Account_ID,
         groups,
         Factor_A 
         ,Factor_B
         ,Factor_D 
         ,Factor_E
         #,Flag_A
         #,Flag_B
         #,Flag_D
         #,Flag_E
         ,Response
         ,Response_F
  ) %>%  
  mutate_all(.funs = as.factor) %>% 
  mutate(time_to_approval = log(as.numeric(dfSeqReg$time_to_approval)))

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dfSeqReg))

## set the seed to make your partition reproducible
#set.seed(123)
train_ind <- sample(seq_len(nrow(dfSeqReg)), size = smp_size)

train <- dfSeqReg[ train_ind,]
test  <- dfSeqReg[-train_ind,]


train<- train[sample(nrow(train)),]
test <- test[sample(nrow(test),replace=F),]

train.acc <- train$Account_ID
train.groups <- train$groups
train.status<- train$time_to_approval

test.acc <- test$Account_ID
test.groups <- test$groups
test.status<- test$time_to_approval

train<-train %>% 
  mutate(Account_ID=NULL,
         groups = NULL)

test<-test %>% 
  mutate(Account_ID=NULL,
         time_to_approval = NULL,
         groups = NULL)

# names of features
features <- setdiff(names(train), "time_to_approval")

#set.seed(123)

m2 <- randomForest::tuneRF(
  x          = train[features],
  y          = train$time_to_approval,
  ntreeTry   = 500,
  mtryStart  = 2,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)


rmModel <- randomForest::randomForest(
  formula = time_to_approval ~ .,
  data    = train,
  mtry = 3,
  ntreeTry = 800,
  importance= TRUE
)

plot(rmModel)

pred = predict(rmModel, newdata = test[32,],  predict.all = TRUE)

## NonParametric Probability Distribution

single_pred <- as.data.frame(as.vector(unlist(exp(pred$individual)))) %>% rename(prediction = `as.vector(unlist(exp(pred$individual)))`)

ggplot(single_pred, aes(x=prediction)) + 
    geom_histogram(aes(y=..density..), binwidth = 3600,colour="black", fill="steelblue")+
  geom_density(alpha=.2, fill="#FF6666") +
  #xlim(0,50000)+
  geom_label(aes(x=exp(test.status[32])+3000, y= -0.000003), label="actual", fill = 'white', colour="blue")+
  geom_vline(xintercept = exp(test.status[32])) +
  labs(title="Non-Parametric Probability Density Estimation for a Single Transaction", x ="Seconds", y = "Frequency")

#plotly::ggplotly(plothist)

hist(exp(pred$individual),breaks = 100 ,xlim = c(0,100000))
summary(as.vector(unlist(exp(pred$individual))))
####

eval <- bind_cols(test, exp(pred), exp(test.status),test.acc,test.groups) %>% 
  mutate(diff = `...7`-`...8`) %>% mutate(poss = ifelse(diff <= 0, 1, 0)) %>% 
  rename(Account_ID=`...9`,pred=`...7`,Actual=`...8`,Groups=`...10`)


sqrt(mean((filter(eval,poss==1)$diff)^2))
mean(filter(eval,poss==1)$diff)

res <- filter(df0,Account_ID %in% eval$Account_ID) %>% 
  ungroup() %>% 
  arrange(groups,Transaction_Timestamp) %>% 
  group_by(groups) %>% 
  summarise_all(first) %>% 
  ungroup() %>% 
  distinct(Account_ID,.keep_all = TRUE)



eval <- distinct(eval,Account_ID,.keep_all = TRUE)

res <- mutate(res, newTime = interval(Transaction_Timestamp,(Transaction_Timestamp+lubridate::seconds(eval$diff)))) 

t <- dfSeqReg %>%  left_join(res,by = 'Account_ID') %>% filter(Total_TGroup.x > 2) #Run line-554 before running this code 
t <- t %>% group_by(groups.x) %>%  filter(Transaction_Timestamp.x %within% newTime) %>% summarise(tot = n()) #%>% filter(tot>2) 
sum(t$tot)-nrow(t) #total declined number of transactions avoided
sum(res$Total_TGroup) - nrow(res)*2

randomForest::importance(rmModel,type=1,scale=T)
create_rfplot(rmModel, type = 1)
