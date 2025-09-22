df <- read.csv('babies.csv')


## Преобразование данных

hist(df$gestation)

#       logarithmic
# log() делает данные более линейным, может смещать распределение влево
# Уравнивает дисперсии выборок
# "Боится" нулей, поэтому рекомендуется прибавлять единицу 

df$log_gestation <- log(df$gestation + 1)#
hist(df$log_gestation)

#       square root

df$sqrt_gestation <- sqrt(df$gestation)
hist(df$gestation)
hist(df$sqrt_gestation)


#       обратное преобразование
df$rev_gestation <- 1/(df$gestation + 1)
hist(df$gestation)
hist(df$rev_gestation)

#       возведение в степень (в квадрат)
df$sq_gestation <- df$gestation**2
hist(df$gestation)
hist(df$sq_gestation)

#       logistic


#### Центральные моменты

# Пожалуйста, изучите 4 главу (параграф 4.1) основного учебника, чтобы
# облегчить понимание следующих строк. Либо, если тяжело с английским
# можете ознакомиться с материалом по ссылке: 
# https://olegtalks.ru/tpost/td8yoxf0r1-kak-ponyat-yaschik-s-usami-boxplot-polno?ysclid=mfv2g532kw150143326
# Там примеры приведены на Python, но теоретическая часть неплохая

# mean()         - среднее значение
# median()       - медианное значение
# modeest::mlv() - модальное значение

	# modeest - не встроенная библиотека. Попробуйте установить
	# её с помощью install.packages
	# 
	# Если с установкой библиотеки возникают проблемы (что скорее
	# будет с вероятностью 99%, то можно попробовать "вытащить" 
	# модальное значение из функции density() (см. ниже)

mean(df$weight, na.rm = T)  # 119.5769
median(df$weight, na.rm = T) # 120
modeest::mlv(df$weight, na.rm = T) # 115

hist(df$weight, freq = F) # Параметр freq переводит ось Y из частоты встречаемости
			  # в "вероятность"
		
dx <- density(df$weight)             # Расчитывает плотность вероятности каждого случайного числа
lines(dx$x, dx$y, col = 2, lwd = 2)  # "Наслаивает" график плотности вероятности
				     # на исходную гистограмму


abline(v = mean(df$weight, na.rm = T), col = 'red', lwd = 2)
abline(v = median(df$weight, na.rm = T), col = 'green', lwd = 2)
abline(v = mlv(df$weight, na.rm = T),
       col = 'blue', lwd = 2)


var(df$weight, na.rm = T)
sd(df$weight, na.rm = T)
sqrt(var(df$weight, na.rm = T))

boxplot(df$weight, horizontal = T)     # horizontal = T - для облегчения восприятия (на первых этапах)
abline(м = median(df$weight, na.rm = T), col = 'green', lwd = 2)
abline(h = mean(df$weight, na.rm = T), col = 'red', lwd = 2)
abline(h = mlv(df$weight, na.rm = T), col = 'blue', lwd = 2)

boxplot.stats(df$weight)
# boxplot(boxplot.stats(df$weight)$stats)

abline(h = median(df$weight, na.rm = T), col = 'green', lwd = 2)
abline(h = mean(df$weight, na.rm = T), col = 'red', lwd = 2)
abline(h = mlv(df$weight, na.rm = T),
       col = 'blue', lwd = 2)


weight <- df$weight[!df$weight %in% boxplot.stats(df$weight)$out] # Попробуем отфиьтровать аутлаеры (выбросы)
								  # Не всегда требуется, но, почему бы и нет

boxplot(weight)


try <- quantile(na.omit(df$weight), 
         probs = c(0, 0.25, 0.5,0.75,1))  # Расчёт квартилей. Для понимания, попробуйте нанести их
					  # на boxplot


#### HOMEWORK ####

1. Придумать, как посчитать моду (если пакет modeest не установился)
2. Посчитать моду по 3 столбцам
3. Нарисовать boxplotы по 3 столбцам
4. Линиями обозначить mean, median, mode
5. Гистограммы с графиком плотности (+ пункт 4)