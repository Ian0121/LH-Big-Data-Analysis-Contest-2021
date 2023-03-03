## gam
gam1 = gam(등록차량수 ~  s(총세대수) + s(공가수) + s(버스) + s(단지내주차면수) +
                  s(전용면적3040) + s(전용면적3040) + s(전용면적4050) + s(전용면적5060) +  s(전용면적60) + 
                  s(단지내전체면적) + s(한세대평균전용면적) + 공급자격유형 + 지역, data=tr_apt_log)


ggGam(gam1)     # 거의 모든 변수가 선형관계를 보여줌. 라쏘 or 엘라스틱넷을 사용하여 변수 줄일 가능성 보임.

par(mfrow=c(2,2))
gam.check(gam1)

x <- tr_apt_log %>% select(-c('등록차량수','단지코드','단지내전체면적', '공급자격유형', '지역')) %>% as.matrix()
y <- cbind(tr_apt_log$등록차량수)
lambdas <- seq(0, 0.05, by = .0005)

set.seed(44)
cv_fit <- cv.glmnet(x, y, alpha = 0.5, lambda = lambdas)
plot(cv_fit)

opt_lambda <- cv_fit$lambda.min
opt_lambda


m_elastic <- glmnet(x,y,alpha=0.5, lambda = opt_lambda)
coef(m_elastic)  



gam_elastic = gam(등록차량수 ~ s(단지내주차면수) + s(공가수) + s(평균임대료)  + s(전용면적3040) + s(전용면적4050) + 
                         s(전용면적5060) + s(총세대수) + s(평균임대보증금) + s(한세대평균전용면적) + s(의도) + 
                         공급자격유형 + 지역, data=tr_apt_log)
ggGam(gam_elastic)


pred <- predict(gam_elastic, te_apt_log, se.fit = TRUE)
pred <- pred$fit
pred <- exp(pred)


## linear regression
lm_fit <- lm(등록차량수 ~ 0 + 단지내주차면수 + 공가수 + 평균임대료 + 전용면적3040 + 전용면적4050 + 
                    전용면적5060 + 총세대수 + 평균임대보증금 + 한세대평균전용면적 + 공급자격유형 + 지역, data=tr_apt_log)

summary(lm_fit)

pred2 <- predict(lm_fit, te_apt_log)
pred2 <- exp(pred2)



## random forest
tr_apt_log_y <- tr_apt_log %>% select(c('등록차량수'))
tr_apt_log <- tr_apt_log %>% select(-c('등록차량수'))

nrow(tr_apt_log) 
nrow(te_apt_log) 

apt_log <- rbind(tr_apt_log, te_apt_log)
apt_log_code <- apt_log %>% select(c('단지코드'))
apt_log <- apt_log %>% select(-c('단지코드'))
nrow(apt_log) 

apt_log <- data.frame(one_hot(data.table(apt_log)))
apt_log <- cbind(apt_log_code, apt_log)

tr_apt_log <- apt_log[1:372,]
te_apt_log <- apt_log[373:515,]

tr_apt_log <- cbind(tr_apt_log, tr_apt_log_y)


set.seed(44)
rf.fit = randomForest(등록차량수 ~. -단지코드 ,data=tr_apt_log, ntree = 5000, importance = T)

importance(rf.fit)
varImpPlot(rf.fit)

set.seed(44)
rf.fit = randomForest(등록차량수 ~ 단지내전체면적 + 단지내주차면수 + 총세대수 + 의도 + 한세대평균전용면적 + 
                             평균임대료 + 평균임대보증금 + 전용면적4050 + 전용면적3040 + 전용면적5060 + 
                             공가수 + 전용면적0030 ,data=tr_apt_log, ntree = 5000, importance = T)


pred3 <- predict(rf.fit, te_apt_log)
pred3 <- exp(pred3)


## xgboost
x <- tr_apt_log[,2:38] %>% as.matrix
y <- tr_apt_log[,39] %>% as.matrix

set.seed(44)
xgb.fit <- xgboost(data = x, label = y, nrounds = 1000, objective = "reg:squarederror",
            early_stopping_rounds = 50, max_depth = 6, eta = .05, verbose = 1000)  

target <- te_apt_log[,2:38] %>% as.matrix
pred4 <- predict(xgb.fit, target)
pred4 <- exp(pred4)


result <- pred*0.5 + pred2*0.3 + pred3*0.1 + pred4*0.1

te_apt_agg$등록차량수 = result
te_apt_agg <- data.table(te_apt_agg)
a<-te_apt_agg[, sum(등록차량수), by="단지코드"]
b = data.frame(a)
names(b) <- c("code",'num')

apartment = left_join(sub,b,by='code')
apartment <- apartment[,-c('num.x')]
names(apartment) <- c("code", "num")
