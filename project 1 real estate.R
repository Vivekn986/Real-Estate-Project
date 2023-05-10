#------------
#creating dummies method by logistic regression
 

library(dplyr)

train=read.csv(r"(C:\DATASETS\housing_train.csv)",stringsAsFactors = FALSE)
test=read.csv(r"(C:\DATASETS\housing_test.csv)", stringsAsFactors = FALSE)

train$Bedroom2[is.na(train$Bedroom2)]=median(train$Bedroom2,na.rm=T)

train$Bathroom[is.na(train$Bathroom)]=round(mean(train$Bathroom,na.rm=T),0)

train$Car[is.na(train$Car)]=round(mean(train$Car,na.rm=T),0)
train$Landsize[is.na(train$Landsize)]=round(mean(train$Landsize,na.rm=T),0)

train$BuildingArea[is.na(train$BuildingArea)]=round(mean(train$BuildingArea,na.rm=T),0)
train$YearBuilt[is.na(train$YearBuilt)]=round(mean(train$YearBuilt,na.rm=T),0)

test$Bedroom2[is.na(test$Bedroom2)]=median(test$Bedroom2,na.rm=T)
test$Bathroom[is.na(test$Bathroom)]=round(mean(test$Bathroom,na.rm=T),0)
test$Car[is.na(test$Car)]=round(mean(test$Car,na.rm=T),0)
test$Landsize[is.na(test$Landsize)]=round(mean(test$Landsize,na.rm=T),0)
test$BuildingArea[is.na(test$BuildingArea)]=round(mean(test$BuildingArea,na.rm=T),0)
test$YearBuilt[is.na(test$YearBuilt)]=round(median(test$YearBuilt,na.rm=T),0)


test$Price=NA
train$data='train'
test$data='test'
all_data=rbind(train,test)
apply(all_data,2,function(x)sum(is.na(x)))

t=table(all_data$Suburb)
View(t)
t1=round(tapply(all_data$Price,all_data$Suburb,mean,na.rm=T),0)
View(t1)
t1=sort(t1)

all_data=all_data %>% 
  mutate(
    sub_1=as.numeric(Suburb%in%c("Campbellfield","Jacana")),
    sub_2=as.numeric(Suburb%in%c("Kealba","Brooklyn","Albion","Sunshine West","Ripponlea","Fawkner")),
    sub_3=as.numeric(Suburb%in%c("Glenroy","Southbank","Sunshine North","Keilor Park","Heidelberg West","Reservoir","Braybrook","Kingsbury","Gowanbrae","Hadfield","Watsonia","Footscray","South Kingsville","Balaclava","Melbourne","Maidstone","Sunshine")),
    sub_4=as.numeric(Suburb%in%c("Airport West","Heidelberg Heights","Pascoe Vale","West Footscray","Altona North","Williamstown North","Brunswick West","Keilor East","Oak Park","Maribyrnong","Altona","Flemington","Coburg North","Yallambie","Avondale Heights","Bellfield")),
    sub_5=as.numeric(Suburb%in%c("Strathmore Heights","Glen Huntly","Kensington","Essendon North","St Kilda","Preston","North Melbourne","Coburg","Kingsville","Collingwood","Brunswick East","Gardenvale","Thornbury","Niddrie","West Melbourne","Viewbank")),
    sub_6=as.numeric(Suburb%in%c("Spotswood","Carnegie","Elwood","Heidelberg","Moorabbin","Oakleigh","Rosanna","Docklands","Yarraville","Cremorne","Seddon","Brunswick","Oakleigh South","Ascot Vale","Windsor","Caulfield","Essendon West","Newport")),
    sub_7=as.numeric(Suburb%in%c("Chadstone","South Yarra","Essendon","Bentleigh East","Murrumbeena","Hughesdale","Fairfield","Ashwood","Clifton Hill","Caulfield North","Abbotsford","Carlton","Prahran","Fitzroy","Ivanhoe","Hampton East","Caulfield East")),
    sub_8=as.numeric(Suburb%in%c("Richmond","Travancore","Templestowe Lower","Ormond","Caulfield South","Moonee Ponds","Hawthorn","Box Hill","Bulleen","Burnley","Burwood","Strathmore","Port Melbourne","Fitzroy North","Alphington")),
    sub_9=as.numeric(Suburb%in%c("Doncaster","South Melbourne","Northcote","Aberfeldie","Elsternwick","Bentleigh","Kooyong","Parkville")),
    sub_10=as.numeric(Suburb%in%c("Williamstown","East Melbourne","Seaholme")),
    sub_11=as.numeric(Suburb%in%c("Malvern East","Carlton North","Hawthorn East","Surrey Hills")),
    sub_12=as.numeric(Suburb%in%c("Princes Hill","Mont Albert","Armadale","Kew East","Glen Iris","Ashburton")),
    sub_13=as.numeric(Suburb%in%c("Brighton East","Eaglemont","Hampton")),
    sub_14=as.numeric(Suburb%in%c("Toorak","Ivanhoe East","Camberwell","Balwyn North","Kew")),
    sub_15=as.numeric(Suburb%in%c("Brighton","Middle Park")),
    sub_16=as.numeric(Suburb%in%c("Albert Park","Balwyn","Malvern"))
  ) %>% 
  select(-Suburb)

glimpse(all_data)

all_data=all_data %>% 
  select(-Address)

all_data=all_data %>%
  mutate(Type_t=as.numeric(Type=="t"),
         type_u=as.numeric(Type=="u"))
all_data=all_data %>% 
  select(-Type)

all_data=all_data %>%
  mutate(Method_PI=as.numeric(Method=="PI"),
         Method_SA=as.numeric(Method=="SA"),
         Method_SP=as.numeric(Method=="SP"),
         Method_VB=as.numeric(Method=="VB")) %>% 
  select(-Method)

all_data=all_data %>%
  mutate(Gnelson=as.numeric(SellerG=="Nelson"),
         GJellis=as.numeric(SellerG=="Jellis"),
         Ghstuart=as.numeric(SellerG=="hockingstuart"),
         Gbarry=as.numeric(SellerG=="Barry"),
         GMarshall=as.numeric(SellerG=="Marshall"),
         GWoodards=as.numeric(SellerG=="Woodards"),
         GBrad=as.numeric(SellerG=="Brad"),
         GBiggin=as.numeric(SellerG=="Biggin"),
         GRay=as.numeric(SellerG=="Ray"),
         GFletchers=as.numeric(SellerG=="Fletchers"),
         GRT=as.numeric(SellerG=="RT"),
         GSweeney=as.numeric(SellerG=="Sweeney"),
         GGreg=as.numeric(SellerG=="Greg"),
         GNoel=as.numeric(SellerG=="Noel"),
         GGary=as.numeric(SellerG=="Gary"),
         GJas=as.numeric(SellerG=="Jas"),
         GMiles=as.numeric(SellerG=="Miles"),
         GMcGrath=as.numeric(SellerG=="McGrath"),
         GHodges=as.numeric(SellerG=="Hodges"),
         GKay=as.numeric(SellerG=="Kay"),
         GStockdale=as.numeric(SellerG=="Stockdale"),
         GLove=as.numeric(SellerG=="Love"),
         GDouglas=as.numeric(SellerG=="Douglas"),
         GWilliams=as.numeric(SellerG=="Williams"),
         GVillage=as.numeric(SellerG=="Village"),
         GRaine=as.numeric(SellerG=="Raine"),
         GRendina=as.numeric(SellerG=="Rendina"),
         GChisholm=as.numeric(SellerG=="Chisholm"),
         GCollins=as.numeric(SellerG=="Collins"),
         GLITTLE=as.numeric(SellerG=="LITTLE"),
         GNick=as.numeric(SellerG=="Nick"),
         GHarcourts=as.numeric(SellerG=="Harcourts"),
         GCayzer=as.numeric(SellerG=="Cayzer"),
         GMoonee=as.numeric(SellerG=="Moonee"),
         GYPA=as.numeric(SellerG=="YPA")
  ) %>% 
  select(-SellerG)

all_data=all_data %>%
  mutate(CA_Banyule=as.numeric(CouncilArea=="Banyule"),
         CA_Bayside=as.numeric(CouncilArea=="Bayside"),
         CA_Boroondara=as.numeric(CouncilArea=="Boroondara"),
         CA_Brimbank=as.numeric(CouncilArea=="Brimbank"),
         CA_Darebin=as.numeric(CouncilArea=="Darebin"),
         CA_Glen_Eira=as.numeric(CouncilArea=="Glen Eira"),
         CA_Monash=as.numeric(CouncilArea=="Monash"),
         CA_Melbourne=as.numeric(CouncilArea=="Melbourne"),
         CA_Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"),
         CA_Manningham=as.numeric(CouncilArea=="Manningham"),
         CA_Kingston=as.numeric(CouncilArea=="Kingston"),
         CA_Hume=as.numeric(CouncilArea=="Hume"),
         CA_HobsonsB=as.numeric(CouncilArea=="Hobsons Bay"),
         CA_MoonValley=as.numeric(CouncilArea=="Moonee Valley"),
         CA_Moreland=as.numeric(CouncilArea=="Moreland"),
         CA_PortP=as.numeric(CouncilArea=="Port Phillip"),
         CA_Stonnington=as.numeric(CouncilArea=="Stonnington"),
         CA_Whitehorse=as.numeric(CouncilArea=="Whitehorse"),
         CA_Yarra=as.numeric(CouncilArea=="Yarra")) %>% 
  select(-CouncilArea)

train=all_data %>% 
  filter(data=='train') %>% 
  select(-data)
#thus train has total obs as 7536 and 70 variables (69+price)

test=all_data %>% 
  filter(data=='test') %>% 
  select(-data,-Price)

glimpse(train)
glimpse(test)

set.seed(123)
s=sample(1:nrow(train),0.75*nrow(train))
train_75=train[s,] #5652
test_25=train[-s,]
library(car)

LRf=lm(Price ~ .,data=train_75)
summary(LRf)

LRf=lm(Price ~ .-Landsize-GRaine-GMoonee-CA_Bayside-GLITTLE-Gnelson-GSweeney-Ghstuart-CA_Kingston-Gbarry-GRay-GStockdale-GNoel-GJas-GBiggin-GYPA-CA_PortP-CA_Whitehorse-GRendina-GFletchers-GBrad-GHodges-GVillage-GLove-sub_4-GGary-CA_Hume-CA_Boroondara-Method_SA-GWilliams-GHarcourts-GNick-GGreg-CA_Monash-GWoodards-CA_Stonnington-GCayzer-Postcode-sub_3,data=train_75)
summary(LRf)

PP_test_25=predict(LRf,newdata =test_25)
PP_test_25=round(PP_test_25,1)
class(PP_test_25)

res=test_25$Price-PP_test_25 #(real value-predicted value)
#root mean square error is as follows
RMSE_test_25=sqrt(mean(res^2))
RMSE_test_25

212467/RMSE_test_25

PP_test_final=predict(LRf,newdata =test)
write.csv(PP_test_final, "PP_test_final.csv")



##----------------- forest mapping


library(tidymodels)
library(visdat)
library(tidyr)
library(car)
library(tidymodels)

library(stringr)

hsg_train=read.csv(r"(C:\DATASETS\housing_train.csv)",stringsAsFactors = FALSE)
hsg_test=read.csv(r"(C:\DATASETS\housing_test.csv)", stringsAsFactors = FALSE)

glimpse(hsg_train)
setdiff(names(hsg_train), names(hsg_test))
head(hsg_train)



lapply(hsg_train,function(x)sum(is.na(x)))

year_func = function(x){
  x= ifelse(x==2010,NA,2010-x)
  return(x)
} 


hsg_train$add =stringr::str_extract(hsg_train$Address, '\\w+$')
hsg_test$add =stringr::str_extract(hsg_test$Address, '\\w+$')


dp_pipe=recipe(Price~., data=hsg_train) %>% 
  update_role(Address,new_role="drop_vars") %>% 
  update_role(Type,Suburb,Method,add,SellerG,CouncilArea,new_role="to_dummies") %>% 
  update_role(Rooms,Distance,Postcode,Bedroom2,Bathroom,Car,Landsize,BuildingArea,new_role="to_numeric") %>% 
  step_rm(has_role("drop_vars")) %>% 
  step_other(has_role("to_dummies"),threshold=0.01,other="__other__") %>% 
  step_unknown(has_role("to_dummies"),new_level="__missing__") %>% 
  step_dummy(has_role("to_dummies")) %>% 
  step_mutate_at(has_role("to_numeric"),fn=as.numeric) %>% 
  step_mutate_at(YearBuilt,fn=year_func) %>% 
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)
## final transformation -> from fit 
train=bake(dp_pipe,new_data=NULL)
test=bake(dp_pipe,new_data=hsg_test)


set.seed(2)
dim(train)
s=sample(1:nrow(train),0.8*nrow(train))
t1=train[s,] ## you create as model
t2=train[-s,]
colSums(is.na(test))

param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))


subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  return(subset_para)
}
num_trials=50
my_params=subset_paras(param,num_trials)

library(randomForest)
library(cvTools)
myerror=9999999

for(i in 1:num_trials){
  # print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  k=cvTuning(randomForest,Price~.,
             data =train,
             tuning =params,
             folds = cvFolds(nrow(train), K=10, type = "random"),
             seed =2
  )
  score.this=k$cv[,2]
  if(score.this<myerror){
    
    # print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
    # print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  # print('DONE')
  # uncomment the line above to keep track of progress
}

ld.rf.final=randomForest(Price~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=train)

t2.pred1=predict(ld.rf.final, newdata = t2)
errors=t2$Price-t2.pred1
rmse=myerror**2%>%mean()%>%sqrt()
212467/rmse
ld.rf.final
test.pred=predict(ld.rf.final,newdata = test)
plot(test.pred,which =3)

d=importance(ld.rf.final)
d=as.data.frame(d)
d$VariableName=rownames(d)
d %>% arrange(desc(IncNodePurity))
varImpPlot(ld.rf.final)

write.csv(test.pred,"mysubmission.csv",row.names = F)
write.csv(test.pred,"vivek_nayak_P1_part2.csv",row.names = F)
getwd()