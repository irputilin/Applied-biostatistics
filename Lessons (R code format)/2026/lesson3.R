df <- mtcars

df$disp[5]
df$disp[5:10]
df$disp[c(5, 7, 8, 11)]

df[20,]
df[,5]
df[20,5]

df[1,1] <- 100

hvosty <- c(seq(20,25, 0.25))
usy <- c(seq(5, 15, 0.5))

usy[17] <- NA

usy

df[17,11] <- NA

df <- na.omit(df)

df[17,11] <- NA


mean(df[,1])
mean(df[,11])

mean(df[,11], na.rm = T)

var(df[,11], na.rm = T)
sqrt(var(df[,11], na.rm = T))
sd(df[,11], na.rm = T)

sqrt(var(df[,11], na.rm = T)) == sd(df[,11], na.rm = T)


hist(df$hp)
hist(df$hp, probability = T,
     nclass = 5,
     col = 'yellow',
     border = 'red',
     xlab = 'Лошадиные силы',
     ylab = 'Плотность',
     main = 'Распределение л.с.')

df$cyl <- as.factor(df$cyl)


plot(df$mpg, df$hp, col = df$cyl)

##### HOMEWORK #####
# mtcars
# Построить точечный график: mpg VS drat
#             цветом указать: vs + легенда!
# Построить гистограмму: mpg, hp, disp
#             цветные!! 
#             Ось Х - обозвать по-русски
#             Ось Y - "Плотность вероятности"
# * Сделать так, чтобы было два графика
# 
# LESSON_1_ФАМ1_ФАМ2_ФАМ3.R

