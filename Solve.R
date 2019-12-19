library(ROCR) #пакет для построения ROC-кривых
adult <- read.csv("D:/1_Vladimir_Fuji/Образование/Projects/Tests/adult.csv")

#смотрю структуру данных
str(adult) 
summary(adult)

#строю модель логистической регрессии и смотрю на значимость предикторов
fit  <- glm(income ~ ., adult, family = "binomial")
summary(fit)

#строю альтернативные модели с уменьшением количества предикторов
fit1 = glm(income ~ age + occupation + education + marital.status + relationship + race + sex + native.country + capital.gain + capital.loss + 
             hours.per.week, adult, family = "binomial")
fit2 = glm(income ~ age + education + marital.status + relationship + race + sex + native.country + capital.gain + capital.loss + 
             hours.per.week, adult, family = "binomial")
fit3 = glm(income ~ age + occupation + education + marital.status + relationship + race + sex + capital.gain + capital.loss + 
             hours.per.week, adult, family = "binomial")

#сравниваю альтернативные модели с исходной
anova(fit, fit3, test = 'Chisq')

#добавляю колонку с предсказанием вероятности значений колонки "income"
adult$prob  <- predict(object = fit, type = "response")

#строю ROC - кривую и смотрю на AUC (довольно не плохая)
pred_fit <- prediction(adult$prob, adult$income)
perf_fit <- performance(pred_fit,"tpr","fpr")
plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.1))
auc  <- performance(pred_fit, measure = "auc")
str(auc)

#специфичность (TNR)
perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
#чувствительность (TPR)
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
#точность правильных классификаций
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

#строю графики
plot(perf3, col = "red", lwd =2)
plot(add=T, perf4 , col = "green", lwd =2)
plot(add=T, perf5, lwd =2)
legend(x = 0.6,y = 0.3, c("spec", "sens", "accur"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)

#и подбираю точку пересечения
abline(v= 0.27, lwd = 2)

#добавляю новую переменную с предсказанным значением
adult$pred_resp  <- factor(ifelse(adult$prob > 0.27, 1, 0), labels = c("<=50K", ">50K"))

#сравниваю предсказанное значение с истинным в выборке
adult$correct  <- ifelse(adult$pred_resp == adult$income, 1, 0)

#% правильно предсказанных значений
mean(adult$correct)

#количество правильно предсказанных значение
sum(adult$correct)