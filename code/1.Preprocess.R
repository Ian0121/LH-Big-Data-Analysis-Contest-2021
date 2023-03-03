
## 패키지 불러오기
pacotes = c("data.table", "dplyr", "mice", "ggGam",
            "glmnet", "mgcv", "mltools", "randomForest", "xgboost")

package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})


## 경로설정
CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(CURRENT_WORKING_DIR)


## 데이터 불러오기
train <- fread('train.csv', encoding = 'UTF-8', data.table=F)
test <- fread('test.csv', encoding = 'UTF-8', data.table=F)
age_gender_info <- fread('age_gender_info.csv', encoding = 'UTF-8', data.table=F)
sub <- fread("sample_submission.csv", encoding = 'UTF-8')


## 변수명 변경                                 
train <- dplyr::rename(train,
                       '버스' = '도보 10분거리 내 버스정류장 수',
                       '지하철' = '도보 10분거리 내 지하철역 수(환승노선 수 반영)')
test <- dplyr::rename(test,
                      '버스' = '도보 10분거리 내 버스정류장 수',
                      '지하철' = '도보 10분거리 내 지하철역 수(환승노선 수 반영)')


## 오류 1번 처리
train <- train %>%
  filter(단지코드 != c('C1490')) %>% filter(단지코드 != c('C2497')) %>% filter(단지코드 != c('C2620')) %>% 
  filter(단지코드 != c('C1344')) %>% filter(단지코드 != c('C1024')) %>% filter(단지코드 != c('C2470')) %>%
  filter(단지코드 != c('C1740')) %>% filter(단지코드 != c('C2405')) %>%
  filter(단지코드 != c('C1804')) 


## 오류 2번 처리
error2 <- train %>%
  filter(단지코드 == 'C1397' | 단지코드 == 'C2085')

train$총세대수[train$단지코드 == 'C1397'] <- sum(unique(error2['총세대수']))
train$총세대수[train$단지코드 == 'C2085'] <- sum(unique(error2['총세대수']))
train$공가수[train$단지코드 == 'C1397'] <- sum(unique(error2['공가수']))
train$공가수[train$단지코드 == 'C2085'] <- sum(unique(error2['공가수']))
train$단지코드[train$단지코드 == 'C2085'] <- 'C1397'

error2 <- train %>% 
  filter(단지코드 == 'C2431' | 단지코드 == 'C1649')

train$총세대수[train$단지코드 == 'C2431'] <- sum(unique(error2['총세대수']))
train$총세대수[train$단지코드 == 'C1649'] <- sum(unique(error2['총세대수']))
train$공가수[train$단지코드 == 'C2431'] <- sum(unique(error2['공가수']))
train$공가수[train$단지코드 == 'C1649'] <- sum(unique(error2['공가수']))
train$등록차량수[train$단지코드 == 'C2431'] <- sum(unique(error2['등록차량수']))
train$등록차량수[train$단지코드 == 'C1649'] <- sum(unique(error2['등록차량수']))
train$버스[train$단지코드 == 'C1649'] <- 2
train$단지코드[train$단지코드 == 'C1649'] <- 'C2431'

train <- train %>%
  filter(단지코드 != c('C1036'))


## 오류 3번 처리
train <- train %>%
  filter(단지코드 != c('C1095')) %>% filter(단지코드 != c('C2051')) %>% filter(단지코드 != c('C1218')) %>% 
  filter(단지코드 != c('C1894')) %>% filter(단지코드 != c('C2483')) %>% filter(단지코드 != c('C1502')) %>%
  filter(단지코드 != c('C1988'))


## 지하철 변수 제거
train <- train %>%
  select(-c('지하철'))

test <- test %>%
  select(-c('지하철'))



##  임대료, 임대보증금 data type 전처리
train$임대료[train$임대료 == '-'] <- -100
train$임대보증금[train$임대보증금 == '-'] <- -100
train$임대료[train$임대료 == ''] <- -100
train$임대보증금[train$임대보증금 == ''] <- -100
train$임대료[train$임대료 == -100] <- NA
train$임대보증금[train$임대보증금 == -100] <- NA
train$임대료 <- as.numeric(train$임대료)
train$임대보증금 <- as.numeric(train$임대보증금)

test$임대료[test$임대료 == '-'] <- -100
test$임대보증금[test$임대보증금 == '-'] <- -100
test$임대료[test$임대료 == ''] <- -100
test$임대보증금[test$임대보증금 == ''] <- -100
test$임대료[test$임대료 == -100] <- NA
test$임대보증금[test$임대보증금 == -100] <- NA
test$임대료 <- as.numeric(test$임대료)
test$임대보증금 <- as.numeric(test$임대보증금)



## '장기전세'의 임대료 -> 0
train$임대료[train$공급유형 == '장기전세'] <- 0


## test의 '자격유형' NA를 train의 '자격유형' 최빈값으로 바꿈
## test에서 자격유형이 NA인 것들은 train에서의 최빈값으로 대체 (핸드라벨링, data leakage 방지)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(train$자격유형)

test$자격유형[test$단지코드 == 'C2411'] <- getmode(train$자격유형)
test$자격유형[test$단지코드 == 'C2253'] <- getmode(train$자격유형)


## 범주형 변수들 factor 변환
train$단지코드 <- factor(train$단지코드)
train$임대건물구분 <- factor(train$임대건물구분)
train$지역 <- factor(train$지역)
train$공급유형 <- factor(train$공급유형)
train$자격유형 <- factor(train$자격유형)

test$단지코드 <- factor(test$단지코드)
test$임대건물구분 <- factor(test$임대건물구분)
test$지역 <- factor(test$지역)
test$공급유형 <- factor(test$공급유형)
test$자격유형 <- factor(test$자격유형)


## integer -> numeric
train$총세대수 <- as.numeric(train$총세대수)
train$전용면적별세대수 <- as.numeric(train$전용면적별세대수)

test$총세대수 <- as.numeric(test$총세대수)
test$전용면적별세대수 <- as.numeric(test$전용면적별세대수)


## 아파트, 상가 나누기
tr_apt <- train %>% 
  filter(임대건물구분 == '아파트')

tr_sto <- train %>% 
  filter(임대건물구분 == '상가') %>% 
  select(-c('임대료', '임대보증금','공급유형','자격유형','지역'))    
# 상가는 '임대료', '임대보증금','공급유형','자격유형' 지우기

te_apt <- test %>% 
  filter(임대건물구분 == '아파트')

te_sto <- test %>% 
  filter(임대건물구분 == '상가') %>% 
  select(-c('임대료', '임대보증금','공급유형','자격유형','지역'))   
# 상가는 '임대료', '임대보증금','공급유형','자격유형' 지우기



## 상가와 같이 있는 아파트 만들기
inter <- intersect(unique(tr_apt$단지코드), unique(tr_sto$단지코드)) %>% as.vector
tr_apt_sto = data.frame(matrix(ncol = 14))
names(tr_apt_sto) <- names(tr_apt)

for (i in seq(1,length(inter))) {
  tr_apt_sto <- rbind(tr_apt_sto,tr_apt %>% filter(단지코드 == inter[i]))
}
tr_apt_sto <- tr_apt_sto[-1,]
tr_apt_sto <- tr_apt_sto %>% select(-임대료,-임대보증금,-공급유형,-자격유형,-지역)


inter1 <- intersect(unique(te_apt$단지코드), unique(te_sto$단지코드)) %>% as.vector
te_apt_sto = data.frame(matrix(ncol = 13))
names(te_apt_sto) <- names(te_apt)

for (i in seq(1,length(inter1))) {
  te_apt_sto <- rbind(te_apt_sto,te_apt %>% filter(단지코드 == inter1[i]))
}
te_apt_sto <- te_apt_sto[-1,]
te_apt_sto <- te_apt_sto %>% select(-임대료,-임대보증금,-공급유형,-자격유형,-지역)



## 순수 아파트 만들기 (주상복합 아파트 제외)
for (i in seq(1,length(inter))) {
  tr_apt <- tr_apt %>% 
    filter(단지코드 != inter[i])
}


for (i in seq(1,length(inter1))) {
  te_apt <- te_apt %>% 
    filter(단지코드 != inter1[i])
}


##  tr_apt 임대료, 임대보증금 NA 채우기
## mice 이용하여 imputation 하기
set.seed(44)
tr_apt_imp <- mice(tr_apt, m=1, method=c('', '', '', '', '', '', '', '', '', 'cart', 'cart', '', '', ''), print=F)

tr_apt_imp$predictorMatrix
pred = tr_apt_imp$predictorMatrix
pred[,'단지코드'] <-  0           # 임대료, 임대보증금 imputation에 필요 없는 변수들 제거 
pred[,'총세대수'] <-  0
pred[,'임대건물구분'] <-  0
pred[,'전용면적별세대수'] <-  0
pred[,'공가수'] <-  0
pred[,'버스'] <-  0
pred[,'단지내주차면수'] <-  0
pred[,'등록차량수'] <-  0
pred

set.seed(44)
tr_apt_imp = mice(tr_apt, m=5, method=c('', '', '', '', '', '', '', '', '', 'cart', 'cart', '', '', ''),
              predictorMatrix = pred, print=F)

tr_apt_imp$imp$임대보증금
tr_apt_imp$imp$임대료

tr_imp_deposit <- tr_apt_imp$imp$임대보증금
tr_imp_fee <- tr_apt_imp$imp$임대료


plot(tr_apt_imp, c('임대보증금','임대료'))      # mice convergence 확인 
stripplot(tr_apt_imp, pch=20, cex=1.2)          # blue point: observed, red point: imputed
densityplot(tr_apt_imp, scales=list(relation='free'),layout=c(2,1))  # The first plot: original complete set.

tr_apt[as.numeric(names(apply(tr_apt_imp$imp$임대보증금, 1, mean))),'임대보증금'] <- apply(tr_apt_imp$imp$임대보증금, 1, mean)
tr_apt[as.numeric(names(apply(tr_apt_imp$imp$임대료, 1, mean))),'임대료'] <- apply(tr_apt_imp$imp$임대료, 1, mean)



## te_apt 임대료, 임대보증금 NA 채우기를 위한 데이터 형태 확인
## imputation 시 data leakage 주의하기 / 핸드라벨링 주의
## 어느 값이 NA이 인지 확인 후, 비슷한 형태를 띄는 것을 train에서 찾기
rent_na <- te_apt %>% select('임대보증금')
te_apt %>% dplyr::slice(which(is.na(rent_na)))


fee_na <- te_apt %>% select('임대료')
te_apt %>% dplyr::slice(which(is.na(fee_na)))    # 임대료와 임대보증금이 NA인 단지코드들이 똑같은 것을 확인


te_apt %>% filter(단지코드 == 'C2152')  # 지역:강원도 / 공급유형:영구임대 / 자격유형:C
te_apt %>% filter(단지코드 == 'C1267')  # 지역:경상남도 / 공급유형:행복주택 / 자격유형:L



## te_apt의 'C2152' 단지코드 '임대료', '임대보증금' NA 값 채우기
tr_apt %>% filter(지역=='강원도') %>% filter(자격유형=='C')          # 이런 데이터는 없음
tr_apt %>% filter(지역=='강원도') %>% filter(공급유형=='영구임대')   # 이 데이터의 임대보증금,임대료로 그대로 채움
dat2 <- tr_apt %>% filter(지역=='강원도') %>% filter(공급유형=='영구임대')
c2152_rent_na <- rep(dat2$임대보증금,2)
c2152_fee_na <- rep(dat2$임대료,2)



##  te_apt의 'C1267' 단지코드 '임대료', '임대보증금' NA 값 채우기
## tr_apt에서 해당유형이 같은 obs들 찾음.
## 한 단지코드 내의 전용면적과 임대료, 임대보증금들은 거의 선형을 띄기 때문에 이들을 가지고 선형회귀를 해서 te_apt의 NA를 채울 예정.
dat <- tr_apt %>% filter(지역=='경상남도') %>% filter(공급유형=='행복주택') %>% filter(자격유형=='L')
fit1 <- lm(임대보증금~전용면적, data=dat)
fit2 <- lm(임대료~전용면적, data=dat)

c1267_na <- te_apt %>% dplyr::slice(which(is.na(rent_na))) %>% filter(단지코드 == 'C1267')

c1267_rent_na <- predict(fit1, c1267_na)
c1267_fee_na <- predict(fit2, c1267_na)



## te_apt 임대료, 임대보증금 NA 채우기
na1 <- c(c2152_rent_na, c1267_rent_na)
na2 <- c(c2152_fee_na, c1267_fee_na)
te_apt[which(is.na(rent_na)),'임대보증금'] <- na1
te_apt[which(is.na(rent_na)),'임대료'] <- na2

rent_na <- te_apt %>% select('임대보증금')  # NA값 제대로 들어간 것 확인
te_apt %>% dplyr::slice(which(is.na(rent_na)))
fee_na <- te_apt %>% select('임대료')
te_apt %>% dplyr::slice(which(is.na(fee_na))) 



## '단지내전체면적' 변수를 위한 기초변수 '전체전용면적' 변수 생성
tr_apt$전체전용면적 <- tr_apt$전용면적 * tr_apt$전용면적별세대수
te_apt$전체전용면적 <- te_apt$전용면적 * te_apt$전용면적별세대수



## tr_apt, te_apt 에서 '공급유형', '자격유형' 변수 단일화(범주화)
sq_type <- function(df){
  df$공급자격유형 <- NA
  for (i in seq(1, nrow(df) )) {
    if (df[i,]$자격유형 == 'A' & df[i,]$공급유형 == '국민임대') {
      df[i,]$공급자격유형 <- 'A국민임대'
    } else if (df[i,]$자격유형 == 'A' & df[i,]$공급유형 != '국민임대'){
      df[i,]$공급자격유형 <- 'A나머지'
    } else if ((df[i,]$자격유형 == 'C' | df[i,]$자격유형 == 'F' | df[i,]$자격유형 == 'I') & df[i,]$공급유형 == '영구임대') {
      df[i,]$공급자격유형 <- 'CFI영구임대' 
    } else if ((df[i,]$자격유형 == 'J' | df[i,]$자격유형 == 'K' | df[i,]$자격유형 == 'L' | df[i,]$자격유형 == 'M' |
                df[i,]$자격유형 == 'N' | df[i,]$자격유형 == 'O') & df[i,]$공급유형 == '행복주택') {
      df[i,]$공급자격유형 <- 'JKLMNO행복주택'
    } else if ((df[i,]$자격유형 == 'B' | df[i,]$자격유형 == 'G') & df[i,]$공급유형 == '국민임대') {
      df[i,]$공급자격유형 <- 'BEG국민임대'
    } else if (df[i,]$자격유형 == 'H' & df[i,]$공급유형 == '국민임대'){
      df[i,]$공급자격유형 <- 'H국민임대'
    } else if (df[i,]$자격유형 == 'E') {
      df[i,]$공급자격유형 <- 'BEG국민임대'
    } else {
      df[i,]$공급자격유형 <- 'D공공분양'
    }
  }
  df <- df %>% select(-c('공급유형', '자격유형'))
  
}

tr_apt <- sq_type(tr_apt)
te_apt <- sq_type(te_apt)
tr_apt$공급자격유형 <- factor(tr_apt$공급자격유형)
te_apt$공급자격유형 <- factor(te_apt$공급자격유형)



## 단지코드별로 하나로 합치기
## 단지내 '평균임대보증금', '평균임대료'  /  '전체전용면적' -> '단지내전체면적' 
## 단지내 전용면적 구간화  /  공급자격유형은 최빈값 / 단지내 '한세대평균전용면적' 변수 생성
agg_apt <- function(df){
  result <- data.frame()
  code <- as.vector(unique(df$단지코드))
  for (i in seq(1, length(code))) {
    cnt_0030 <-  0 ;  cnt_3040 <-  0 ; cnt_4050 <-  0 ; cnt_5060 <- 0 ; cnt_60 <-  0
    mean_deposit <-  0 ; mean_fee <-  0
    sum_area <-  0 ; mean_area <- 0
    mode_sqtype <- NA
    
    same_code <- df %>% filter(단지코드 == code[i])
    
    # '평균임대보증금', '평균임대료'
    summary1 <- same_code %>% group_by(임대보증금) %>% summarise(SUM = sum(전용면적별세대수))
    summary2 <- same_code %>% group_by(임대료) %>% summarise(SUM = sum(전용면적별세대수))
    summary1$weight <- summary1[,2]/sum(summary1[,2])
    summary2$weight <- summary2[,2]/sum(summary2[,2])
    mean_deposit <- as.numeric(sum(summary1$임대보증금 * summary1$weight))
    mean_fee <- as.numeric(sum(summary2$임대료 * summary2$weight))
    
    # '전체전용면적' -> '단지내전체면적' / '한세대평균전용면적' 변수 생성
    sum_area <- sum(same_code$전체전용면적)
    mean_area <- sum_area / sum(summary1[,2])
    
    # '공급자격유형'은 최빈값 
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    mode_sqtype <- getmode(same_code$공급자격유형)
    
    # 전용면적 구간화 
    for (j in seq(1, nrow(same_code))) {
      if (same_code[j, ]$전용면적 < 30) {
        cnt_0030 <- cnt_0030 + same_code[j, ]$전용면적별세대수
      } else if (30 <= same_code[j, ]$전용면적 &  same_code[j, ]$전용면적 < 40) {
        cnt_3040 <- cnt_3040 + same_code[j, ]$전용면적별세대수
      } else if (40 <= same_code[j, ]$전용면적 &  same_code[j, ]$전용면적 < 50) {
        cnt_4050 <- cnt_4050 + same_code[j, ]$전용면적별세대수
      } else if (50 <= same_code[j, ]$전용면적 &  same_code[j, ]$전용면적 < 60) {
        cnt_5060 <- cnt_5060 + same_code[j, ]$전용면적별세대수
      } else {
        cnt_60 <-  cnt_60 + same_code[j, ]$전용면적별세대수
      }
    }
    subresult <- same_code[1,]
    subresult$평균임대보증금 <- mean_deposit ; subresult$평균임대료 <- mean_fee
    subresult$전용면적0030 <-  cnt_0030 ; subresult$전용면적3040 <-  cnt_3040
    subresult$전용면적4050 <-  cnt_4050 ; subresult$전용면적5060 <-  cnt_5060
    subresult$전용면적60 <-  cnt_60
    subresult$단지내전체면적 <-  sum_area ; subresult$한세대평균전용면적 <-  mean_area
    subresult$공급자격유형 <- mode_sqtype
    
    result <- rbind(result, subresult)
    
  }
  rownames(result) <- NULL
  result <- result %>% select(-c('전용면적', '전용면적별세대수', '임대보증금', '임대료', '전체전용면적'))
  return(result)
}

tr_apt_agg <- agg_apt(tr_apt)
te_apt_agg <- agg_apt(te_apt)

tr_apt_agg <- tr_apt_agg %>% select(-c('임대건물구분'))
te_apt_agg <- te_apt_agg %>% select(-c('임대건물구분'))



## '등록차량수', '총세대수', '단지내주차면수' log 씌우기
tr_apt_log <- tr_apt_agg
tr_apt_log$등록차량수 <- log(tr_apt_log$등록차량수)
tr_apt_log$총세대수 <- log(tr_apt_log$총세대수)
tr_apt_log$단지내주차면수 <- log(tr_apt_log$단지내주차면수)


te_apt_log <- te_apt_agg
te_apt_log$총세대수 <- log(te_apt_log$총세대수)
te_apt_log$단지내주차면수 <- log(te_apt_log$단지내주차면수)




## '의도' 변수 만들기
# 단지별로 주차장을 만들때 '단지내주차면수'를 '총세대수'와 다르게 만든 이유가 있을것이라고 보고, 새로운 변수 창출.
tr_apt_log$의도 <- tr_apt_log$총세대수 - tr_apt_log$단지내주차면수
te_apt_log$의도 <- te_apt_log$총세대수 - te_apt_log$단지내주차면수
