## 상가세대수 총합 구하기
agg_sto <- function(df){
  result <- data.frame()
  code <- as.vector(unique(df$단지코드))
  
  for (i in seq(1,length(code))) {
    cnt_sto <- 0
    
    same_code <- df %>% filter(단지코드 == code[i])
    
    summary <- sum(same_code$전용면적별세대수)
    subresult <- same_code[1,]
    subresult$상가세대수합 = summary
    result <- rbind(result, subresult)
  }
  return(result)
}

tr_sto_agg <- agg_sto(tr_sto)
te_sto_agg <- agg_sto(te_sto)


## 상가있는 아파트 전용면적 구간화/ 단지내전체면적 / 평균전용면적 구하기
agg_sto_apt <- function(df){
  result <- data.frame()
  code <- as.vector(unique(df$단지코드))
  
  for (i in seq(1,length(code))) {
    cnt_20 <-  0 ;  cnt_30 <-  0 ; cnt_40 <-  0 ; sum_area <-  0
    
    same_code <- df %>% filter(단지코드 == code[i])
    
    # 전용면적 구간화 + 전체면적
    for (j in 1:nrow(same_code)) {
      if (same_code[j, ]$전용면적 < 30) {
        cnt_20 <- cnt_20 + same_code[j, ]$전용면적별세대수
      } else if (30 <= same_code[j, ]$전용면적 &  same_code[j, ]$전용면적 < 40) {
        cnt_30 <- cnt_30 + same_code[j, ]$전용면적별세대수
      } else {
        cnt_40 <-  cnt_40 + same_code[j, ]$전용면적별세대수
      }
      sum_subarea <- same_code[j, ]$전용면적별세대수*same_code[j, ]$전용면적
      sum_area <- sum_area + sum_subarea
    }
    
    # 평균면적
    mean_area = sum_area/(cnt_20+cnt_30+cnt_40)
    
    subresult <- same_code[1,]
    subresult$면적20 <- cnt_20 ; subresult$면적30 <- cnt_30 ;  subresult$면적40 <- cnt_40
    subresult$단지내전체면적 <- sum_area ; subresult$한세대평균전용면적 <- mean_area
    
    result <- rbind(result, subresult)
  }
  return(result)
}

tr_stoapt_agg <- agg_sto_apt(tr_apt_sto)
te_stoapt_agg <- agg_sto_apt(te_apt_sto)


## 데이터 정리
tr_stoapt_agg <- tr_stoapt_agg %>% select(-임대건물구분, -전용면적, -전용면적별세대수)
te_stoapt_agg <- te_stoapt_agg %>% select(-임대건물구분, -전용면적, -전용면적별세대수)

tr_sto_agg <- tr_sto_agg %>% select(단지코드,상가세대수합)
te_sto_agg <- te_sto_agg %>% select(단지코드,상가세대수합)

## final dataset
ftr_sto <- merge(tr_stoapt_agg, tr_sto_agg, by='단지코드')
fte_sto <- merge(te_stoapt_agg, te_sto_agg, by='단지코드')



## '등록차량수', '총세대수', '단지내주차면수', '단지내전체면적','버스' log 변환
xtr_sto = ftr_sto %>% select(-단지코드)
xte_sto = fte_sto %>% select(-단지코드)

xtr_sto_log <- xtr_sto
xtr_sto_log$총세대수 <- log(xtr_sto_log$총세대수)
xtr_sto_log$단지내주차면수 <- log(xtr_sto_log$단지내주차면수)
xtr_sto_log$단지내전체면적 <- log(xtr_sto_log$단지내전체면적)
xtr_sto_log$버스 <- log(xtr_sto_log$버스)

xtr_sto_log$등록차량수 <- log(xtr_sto_log$등록차량수)

xte_sto_log <- xte_sto
xte_sto_log$총세대수 <- log(xte_sto_log$총세대수)
xte_sto_log$단지내주차면수 <- log(xte_sto_log$단지내주차면수)
xte_sto_log$단지내전체면적 <- log(xte_sto_log$단지내전체면적)
xte_sto_log$버스 <- log(xte_sto_log$버스)

xtr_sto_log <- xtr_sto_log[,c('공가수','단지내주차면수','등록차량수','버스','총세대수','면적20','면적30','면적40','단지내전체면적',
                              '한세대평균전용면적','상가세대수합')]
xte_sto_log <- xte_sto_log[,c('공가수','단지내주차면수','버스','총세대수','면적20','면적30','면적40','단지내전체면적',
                              '한세대평균전용면적','상가세대수합')]

## Elastic Net

x_sto <- xtr_sto_log %>% select(-등록차량수) %>% as.matrix()
y_sto <- xtr_sto_log$등록차량수 %>% as.numeric()

set.seed(44)
sto_elastic <- glmnet(x_sto,y_sto,alpha=0.5, lambda = 0.01)

xte_sto_log2 <- xte_sto_log %>% as.matrix()

pred_sto <- predict(sto_elastic, xte_sto_log2)
pred_sto <- exp(pred_sto)

## linear model

sto_lm <- lm(등록차량수~면적20+면적30+면적40,data = xtr_sto_log)

pred1_sto = predict(sto_lm, xte_sto_log)
pred1_sto <- exp(pred1_sto)

## 최종 예측
fte_sto$등록차량수 = (pred_sto*0.2+pred1_sto*0.8)
fte_sto <- data.table(fte_sto)
final_sto <- fte_sto[, sum(등록차량수), by="단지코드"]
final_sto = data.frame(final_sto)
names(final_sto) <- c("code",'num')

store = left_join(sub,final_sto,by='code')
store <- store[,-c('num.x')]
names(store) <- c("code", "num")


## 순수 아파트 + 상가있는 아파트 예측값 병합
apartment[is.na(apartment)] <- 0
store[is.na(store)] <- 0

store$num = apartment$num + store$num

store = data.frame(store)

real_final = left_join(sub,store,by='code')
real_final <- real_final[,-c('num.x')]
names(real_final) <- c("code", "num")

write.csv(real_final,'final_submission_team_stat.csv',row.names = FALSE)
