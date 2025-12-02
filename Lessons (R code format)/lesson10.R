library(shipunov) 

df <- mtcars

plot(df$hp, df$mpg)

# Для того, чтобы провести регрессионный анализ необходимо воспользоваться
# функцией lm()

lmod <- lm(mpg ~ hp, df)
summary(lmod)
coeff <- lmod$coefficients  # 1й - Intercept (b), 2й - Slope (k) для уравнения y = k*x + b


x1 <- seq(50, 400, 10)
y1 <- coeff[2]*x1+coeff[1]

lines(x1, y1)


plot(lmod)

# Для оценки состоятельности модели можно проверить нормальность распределения остатков

shapiro.test(lmod$residuals)

plot(df$hp, df$mpg)
abline(lmod, col='red')
Cladd(lmod, df)

plot(df$hp, df$mpg, col='red', pch = 19)
abline(lmod)
with(df, segments(hp, fitted(lmod), hp, mpg, col='red'))

library(ggplot2)
ggplot(df, aes(x = hp, y = mpg))+
  geom_point()+
  geom_smooth(method = 'lm')



lmod2 <- lm(mpg ~ hp + disp + wt + qsec, df)
summary(lmod2)
coeff2 <- lmod2$coefficients

lmod2 <- lm(mpg ~  wt, df)
summary(lmod2)
coeff2 <- lmod2$coefficients

x2 <- seq(min(df$wt), max(df$wt), 0.2)
y2 <- coeff2[2]*x2+coeff2[1]

plot(mpg ~ wt, df)
lines(x2, y2)

plot(lmod2)

shapiro.test(lmod2$residuals)

# Информационные критерии AIC и BIC необходимы для того, чтобы выяснить, какая из моделей
# лучше описывает данные

AIC(lmod, lmod2)
BIC(lmod, lmod2)

ggplot(df, aes(x = hp, y = mpg))+
  geom_point()+
  geom_smooth(method = 'loess')

df$cyl <- as.factor(df$cyl)

lmod3 <- lm(mpg ~ wt*cyl, df)
summary(lmod3)


with(df, plot(wt, mpg, pch=as.numeric(cyl)))
abline(lm(mpg ~ wt, data=subset(df, cyl=="4")))
abline(lm(mpg ~ wt, data=subset(df, cyl=="6")),lty=2)
abline(lm(mpg ~ wt, data=subset(df, cyl=="8")),lty=4)
legend("topright", lty=c(1,2,4), legend=c("4","6", '8'))

