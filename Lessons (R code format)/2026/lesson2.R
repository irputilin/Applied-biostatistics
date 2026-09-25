# ОБщий вид создания переменной
# переменная <- что-либо 

x <- 7
peremennaya <- c(5, 22, 104, 11, 53, 67, 52)

peremennaya + x
peremennaya - x
peremennaya * x
peremennaya / x
peremennaya ** x

perem_2 <- c(1, 2, 3, 4, 5, 6, 'slovo') 
perem_2 / 2


# Общий вид взаимодействия с функциями
#
# название_функции(аргумент1, аргумент2.... аргументN)
# название_функции(арг1 = знач1, арг2 = знач2,...)


is.numeric(peremennaya)


y <- 1:100
y2 <- seq(from = 1, to = 100, by = 7)
y3 <- seq(1, 100, 2.5)
y4 <- seq(100, 1, -8)

y5 <- c(rep('Б', 5), rep('Ц', 5), rep('Ы', 5))

plot(y2, col = 'yellow')

plot(y2, col = 'red', pch = 101)

y5 <- as.factor(y5)

plot(y2, col = y5)
legend('bottomright',
       col = 1:3,
       pch = 1,
       legend = levels(y5))

