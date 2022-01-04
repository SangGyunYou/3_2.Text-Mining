
Sys.setlocale("LC_ALL", "Korean")

rm(list=ls())
setwd("C:/Users/Yogurt/Google 드라이브/광고홍보학부｜AI MBA/AI MBA/1_2. 예측분석★/Asg_Forecasting Analysis/팀원배포 파일")
getwd()

# install.packages('car')
# install.packages('plm')

library(ggplot2)
library(dplyr)
library(car)
library(plm)


df = read.csv('Data_jjk2.csv',header = T); head(df)
str(df)
df$date  = as.Date(df$date)
df$region= as.factor(df$region); str(df);

help(summarise)


#ndf = as.data.frame(summarise(group_by(df, date, region), mosquito_index = max(mosquito_index))) ; head(ndf)
ndf = df %>%   group_by(date, region) %>%
  summarise(mosquito_index = max(mosquito_index)) %>% as.data.frame() ; head(ndf)

ndf = left_join(ndf,df ,by=c('date','region','mosquito_index')); head(ndf)

# 현재 데이터로 모기 지수의 그래프 
x11(); ndf %>% ggplot(aes(x=date, y=mosquito_index, group = region, col=region))+geom_line()

# 1) 2020년 02월 이전 데이터와, 이후 데이터는 모기지수 데이터 수요 조사의 공식이나, 방식이 변경 되어
#    데이터 속성이 달라지므로, 함께 사용하기가 힘듬
# 2) 2020년 02월 이후 데이터를 활용 할려고 하였으나, 분석 결과 11월부터 04월까지의 수집된 데이터의 정확성이 떨어졌고,
#    11월~ 04월까지의 데이터의 영향으로 04월 이후의 예측값에도 막대한 영향으로, TEST set 결과의 편차가 너무 많이 나타남
#    그리하여, 2020년 05월 부터 현재까지의 데이터를 활용 하여 모델을 만듬
ndf2_1 = ndf %>% filter(date>='2020-05-01'&date<='2020-10-31') #2020.5 ~ 2020.10 데이터
ndf2_2 = ndf %>% filter(date>='2021-05-01') #2021.5 데이터
ndf2 = bind_rows(ndf2_1,ndf2_2) # 데이터 합치기
str(ndf2)
summary(ndf2) #  데이터 summary

x11(); ndf2 %>% ggplot(aes(x=date, y=mosquito_index, group = region, col=region))+geom_line()


# 1. 변수간 correlation
corr_mat = data.frame(abs(cor(ndf2[,c(3:ncol(ndf2))]))) ; corr_mat
# write.csv(corr_mat,'corr_mat_full2.csv')
# correlation으로 변수 제거 0.8 이상
# 사용변수
# ugwater_level,ugwater_electconduction,region_kor_man,region_fore_man,region_dap,air_O3,air_SO2,air_finedust,air_Temp_min,air_precipitation,air_windspeed,air_humidity


# 2. Pooled Model 분석(굳이 안해도 되지만.... 레포트 분량이 적으면 넣기)
m1 = lm(mosquito_index ~ 0+ region+ugwater_level+ugwater_electconduction+region_kor_man+region_fore_man+region_dap+air_O3+air_SO2+air_finedust+air_Temp_min+air_precipitation+air_windspeed+air_humidity,data=ndf2)
summary(m1)

# 2.1. step aic(Pooled Model을 AIC기준으로 좋은 모델을 찾기)
step(m1)
m2 = lm(formula = mosquito_index ~ region + ugwater_level + ugwater_electconduction + 
          region_kor_man + air_O3 + air_Temp_min + air_precipitation + 
          air_windspeed + air_humidity - 1, data = ndf2)
summary(m2)


# 3. Fixed Effect Vs Random Effect Model
pdata <-  pdata.frame(ndf2, index=c("region", "date"));head(pdata)

# 3.1. Fixed Effect
# log 를 취한 이유는 스케일이 너무 큰 변수가 존재 하면 이상치가 발생 하므로 log로 분포를 줄인다.
# ugwater_level(한자리수), ugwater_electconduction(3자리수) 원본 데이터 확인
fixed <- plm(mosquito_index ~ugwater_level+ugwater_electconduction+log(region_kor_man)+log(region_fore_man)+log(region_dap)+air_O3+air_SO2+air_finedust+air_Temp_min+air_precipitation+air_windspeed+air_humidity
             , data=pdata, model="within")

summary(fixed)
# 3.1.1 유의하지 않은 변수 제거
fixed_1 <- plm(mosquito_index ~ugwater_level+ugwater_electconduction+log(region_kor_man)+air_O3+air_Temp_min+air_precipitation+air_windspeed+air_humidity
               , data=pdata, model="within")
summary(fixed_1)
fx_level <- fixef(fixed_1, type = "level"); fx_level
fx_dmean <- fixef(fixed_1, type = "dmean"); fx_dmean
fixef(fixed_1)
coef(fixed_1)

# 3.1.2 변수 예측
### 예측 방법은 "모기지수예측" = "각 변수"*coefficient(coef(fixed_1)) + "지역의 고정효과"(fixef(fixed_1))
ndf3 = ndf2[,c('date',"region",'mosquito_index','ugwater_level','ugwater_electconduction','region_kor_man','region_fore_man','region_dap','air_O3','air_SO2','air_finedust','air_Temp_min','air_precipitation','air_windspeed','air_humidity')]
ndf3
ndf3$tmp_pred1 = -4.3601947 *ndf3$ugwater_level  -0.1538435 *ndf3$ugwater_electconduction+ 1167.7985857*log(ndf3$region_kor_man) +590.0905501*ndf3$air_O3 +2.9868586 *ndf3$air_Temp_min+  +3.0468620*ndf3$air_windspeed+ 0.2917945*ndf3$air_humidity 
ndf3$tmp_pred2 = ifelse(ndf3$region=='공원',-16411 ,ifelse( ndf3$region=='수변부', -16797 ,-16431  ))
ndf3$pred = ndf3$tmp_pred1+ndf3$tmp_pred2
RMSE_fixed = sqrt(mean((ndf3$pred-ndf3$mosquito_index)^2));RMSE_fixed
ndf3[,c('mosquito_index', 'pred')]


# 3.2. Random Effect
random <- plm(mosquito_index ~ugwater_level+ugwater_electconduction+log(region_kor_man)+log(region_fore_man)+air_O3+air_SO2+air_finedust+air_Temp_min+air_precipitation+air_windspeed+air_humidity
              , data=pdata, model="random",random.method="amemiya")
summary(random)
# 3.2.1 유의하지 않은 변수 제거
random_1 <- plm(mosquito_index ~ugwater_level+ugwater_electconduction+log(region_kor_man)+air_O3+air_Temp_min+air_precipitation+air_windspeed+air_humidity
                , data=pdata, model="random",random.method="amemiya")
summary(random_1)
ranef(random_1)

# 3.2.2 변수 예측
### 예측 방법은 "모기지수예측" = "각 변수"*coefficient(coef(fixed_1)) + "지역의 랜덤효과"(ranef(random_1))
ndf4 = ndf2[,c('date',"region",'mosquito_index','ugwater_level','ugwater_electconduction','region_kor_man','region_fore_man','region_dap','air_O3','air_SO2','air_finedust','air_Temp_min','air_precipitation','air_windspeed','air_humidity')]
ndf4$tmp_pred1 = -1.5966e+04-4.3743e+00*ndf4$ugwater_level-1.5228e-01 *ndf4$ugwater_electconduction+ 1.1273e+03*log(ndf4$region_kor_man) +6.0059e+02*ndf4$air_O3 +3.0180e+00  *ndf4$air_Temp_min+  +2.6449e+00*ndf4$air_windspeed+2.3919e-01  *ndf4$air_humidity +7.0867e-02*ndf4$ air_precipitation 
ndf4$tmp_pred2 = ifelse(ndf4$region=='공원',130.1573 ,ifelse( ndf4$region=='수변부',  -240.8739  ,110.7167   ))
ndf4$pred = ndf4$tmp_pred1+ndf4$tmp_pred2
RMSE_random = sqrt(mean((ndf4$pred-ndf4$mosquito_index)^2));RMSE_random
ndf4[,c('mosquito_index', 'pred')]
ndf4
ndf4[,c('date', 'region', 'mosquito_index', 'pred')]


# 3.3. Fixed Effect Vs Random Effect 
# Ho : random  effect vs H1 : fixed effect 
# p-value 0.99 이므로 귀무가설을 기각하지 못한다 따라서 random effect 모델을 추천한다
phtest(fixed_1, random_1)

# 4. 예측그래프(Random Effect모델을 결정하면 굳이 Fixed model을 안그려도 상관없음음)
# 4.1 fixed effect
a = ndf3 %>% select('date','region','mosquito_index') %>% mutate(grp = 'real')
b = ndf3 %>% select('date','region','pred') %>% mutate(grp = 'prediction') %>% rename(mosquito_index = pred)
# fixed effect 공원
x11(); bind_rows(a,b) %>% filter(region == '공원') %>% ggplot(aes(x=date, y=mosquito_index, group = grp, col = grp))+geom_line()+ ggtitle("공원 예측")+theme(plot.title = element_text(hjust = 0.5))
# fixed effect 수변부
x11(); bind_rows(a,b) %>% filter(region == '수변부') %>% ggplot(aes(x=date, y=mosquito_index, group = grp, col = grp))+geom_line()+ ggtitle("수변부 예측")+theme(plot.title = element_text(hjust = 0.5))
# fixed effect 주거지 
x11(); bind_rows(a,b) %>% filter(region == '주거지') %>% ggplot(aes(x=date, y=mosquito_index, group = grp, col = grp))+geom_line()+ ggtitle("주거지 예측")+theme(plot.title = element_text(hjust = 0.5))


# 4.2 Random effect
c = ndf4 %>% select('date','region','mosquito_index') %>% mutate(grp = 'real')
d = ndf4 %>% select('date','region','pred') %>% mutate(grp = 'prediction') %>% rename(mosquito_index = pred)
# random effect 공원
x11(); bind_rows(c,d) %>% filter(region == '공원') %>% ggplot(aes(x=date, y=mosquito_index, group = grp, col = grp))+geom_line()+ ggtitle("공원 예측")+theme(plot.title = element_text(hjust = 0.5))
# random effect 수변부
x11(); bind_rows(c,d) %>% filter(region == '수변부') %>% ggplot(aes(x=date, y=mosquito_index, group = grp, col = grp))+geom_line()+ ggtitle("수변부 예측")+theme(plot.title = element_text(hjust = 0.5))
# random effect 주거지 
x11(); bind_rows(c,d) %>% filter(region == '주거지') %>% ggplot(aes(x=date, y=mosquito_index, group = grp, col = grp))+geom_line()+ ggtitle("주거지 예측")+theme(plot.title = element_text(hjust = 0.5))

