# В зависимости от того, какие данные у вас имеются на входе (имеется в виду характер
# распределения), вам, как исследователям необходимо пользоваться подходящими 
# статистическими критериями.
#
# Изучите схему выбора релевантных критериев на главной странице курса в GitHub
# Ниже будет представлено то, как реализовать конкретный критерий из схемы в R.
# Данное занятие будет посвящено непрерывным данным
#
#
# Любой статистический тест всегда проверяет справедливость нулевой гипотезы.
# Как вы можете помнить, нулевая гипотеза (H0) в общем виде говорит о том, что
# никаких различий между сравниваемыми показателями нет статистически значимой
# разницы. Альтернативная гипотеза (H1) постулирует наличие статистически значимых 
различий

#### NORMALLY DISTRIBUTED
### 1 group
## n > 30
# z-test - mean is equal to given constant

library(BSDA)

z.test(df$height, mu = median(df$height),
       sigma.x = sd(df$height))


## n < 30
# t-test - mean is equal to given constant
t.test(df$height, mu = median(df$height))

### 2 groups
## non-paired
# t-test - means are equal
t.test(df$height[df$smoke == 1], df$height[df$smoke == 0])


## paired
# paired t-test - mean differences are equal
t.test(df$height[df$smoke == 1][1:100], df$height[df$smoke == 0][1:100],
       paired = T)


### 3 and more groups
# ANOVA - means are equal
df$gestation_f <- cut(df$gestation, breaks = c(220, 270, 290, 350),
                      labels = c('Few','Normal','A lot'))
table(df$gestation_f)

fligner.test(height ~ gestation_f, df)

aov_res <- aov(height ~ gestation_f, df)
summary(aov_res)

TukeyHSD(aov_res) # Данный критерий (критерий Тьюки) показывает, между какими
                  # сравниваемыми группами действительно существуют различия

plot(TukeyHSD(aov_res))

#### SKEWED
### 2 groups
## non-paired
# Wilcoxon Rank Sum test - medians are equal - критерий суммы рангов Вилкоксона
# Также можно запомнить как критерий Манна-Уитни

wilcox.test(height ~ smoke, df)
wilcox.test(x = df$height[df$smoke == 1],
            y = df$height[df$smoke == 0])


## paired
# Wilcoxon Signed Rank Test - median differences are equal - критерий знаковых рангов
# Вилкоксона

wilcox.test(x = df$height[df$smoke == 1][1:100],
            y = df$height[df$smoke == 0][1:100],
            paired = T)

### 3 and more groups
# non-parametric ANOVA - Kruskal-Wallis criterion - medians are equal
kruskal.test(height ~ gestation_f, df)

# Непараметрический аналог критерия Тьюки - критерий Данна
library(dunn.test)
dunn.test(df$height, df$gestation_f, method = 'holm') # method = 'holm' - 
                                                      # поправка на размер выборки
                                                      # можете самостоятельно разобрать
                                                      # для чего нужны и чем различаются
                                                      # разные поправки, но, в общем и целом,
                                                      # можете пользоваться поправкой Холма


#### HOMEWORK ####
Провести анализ непрерывной переменной (x) по группам.
1) Взять групповую переменную с двумя группами (y1, y2)
1.1) Провести проверку на нормальность переменной x и равенство дисперсий между группами y1 и y2 по переменной x
1.2) В зависимости от результатов в п. 1.1 использовать релевантные методы сравнения выборок с двумя группами

2) Взять групповую переменную с тремя или более группами (z1, z2, z3...)
2.1) Провести проверку на нормальность переменной x и равенство дисперсий между группами (z) по переменной x
2.2) В зависимости от результатов в п. 2.1 использовать релевантные методы сравнения выборок с тремя и более группами




