#1.  Реализовать AR(2)ARCH(3) процесс с заданным кол-вом наблюдений и значениями параметров θ и А
set.seed(123)
library("tseries")
#кол-во наблюдений - 2100
AR2ARCH3 = function(A, θ) {
  n = 2100 
  x = numeric(n)
  σ = numeric(n)
  e = rnorm(n, 0, 1)
  
  σ[1] = A[1]
  x[1] = sqrt(σ[1]) * e[1]
  σ[2] = A[1] + A[2] * x[1]^2 
  x[2] = θ[1] * x[1] + sqrt(σ[2]) * e[2]
  σ[3] = A[1] + A[2] * x[2]^2 + A[3] * x[1]^2
  x[3] = θ[1] * x[2] + θ[2] * x[1] + sqrt(σ[3]) * e[3]
  
  for (i in 4:n) {
    σ[i] = A[1] + A[2] * x[i - 1]^2 + A[3] * x[i - 2]^2 + A[4] * x[i - 3]^2
    x[i] = θ[1] * x[i - 1] + θ[2] * x[i - 2] + sqrt(σ[i]) * e[i]
  }
  
  plot(x, type = 'l', main = "AR(2)ARCH(3)")
  return(x)
}

#заданные параметры
θ = c(-0.3, 0.4)
A = c(1, 0.2, 0.1, 0.2)

#генерация AR(2)ARCH(3) и построение графика
x_n = AR2ARCH3(A, θ)

#2. Разделить последовательность {xn}, в отношении 20:1 на обучающую и тестовую выборки.
n_train = 2000
n_test = 100
x_train = x_n[1:n_train] #обучающая, от 1 до 2000
x_test = x_n[(n_train + 1):(n_train + n_test)] #тестовая, от 2001 до 2100

#3. На основе обучающей выборки получить оценки параметров θ и а

#оценка параметров θ с помощью ф-ии arima() 
ar_model = arima(x_train, order = c(2, 0, 0), include.mean = FALSE)
θ_estimates = c(ar_model$coef[1], ar_model$coef[2])

#оценка параметров А с помощью ф-ии garch()
h_residuals = numeric(n_train)
h_residuals[1] = x_train[1]
h_residuals[2] = x_train[2] - θ_estimates[1] * x_train[1]

#расчет остатков для GARCH модели
for (i in 3:n_train) {
  h_residuals[i] = x_train[i] - θ_estimates[1] * x_train[i - 1] - θ_estimates[2] * x_train[i - 2]
}

#оценка параметров GARCH модели
garch_model = garch(h_residuals, order = c(3, 0), start = A)
A_estimates = c(garch_model$coef[1], garch_model$coef[2], garch_model$coef[3], garch_model$coef[4]) 

θ_estimates #оценки параметров θ
A_estimates #оценки параметров А

#4. Построить последовательность прогнозов на один шаг на тестовой выборке

plot(x_test, type = 'l', col = 'blue')

predict_AR2ARCH3 = function(x_test, θ, A) {
  n_test = length(x_test)
  x_predict = numeric(n_test)
  σ = numeric(n_test)
  upper_bound = numeric(n_test)
  lower_bound = numeric(n_test)
  
  x_predict[1] = 0
  x_predict[2] = θ[1] * x_test[1]
  x_predict[3] = θ[1] * x_test[2] + θ[2] * x_test[1]
  σ[1] = A[1]
  σ[2] = A[1] + A[2] * x_test[1]^2
  σ[3] = A[1] + A[2] * x_test[2]^2 + A[3] * x_test[1]^2
  
  #моделирование последовательности прогнозов
  for (i in 4:n_test) {
    x_predict[i] = θ[1] * x_test[i - 1] + θ[2] * x_test[i - 2]
    σ[i] = A[1] + A[2] * x_test[i - 1]^2 + A[3] * x_test[i - 2]^2 + A[4] * x_test[i - 3]^2
  } 
  
  #расчет верхней и нижней границ прогноза
  for (i in 1:n_test) {
    upper_bound[i] = x_predict[i] + sqrt(σ[i])
    lower_bound[i] = x_predict[i] - sqrt(σ[i])
  }
  
  #наложение последовательности прогнозов на последовательность наблюдений процесса
  lines(x_predict, type = 'p', col = 'black')
  lines(upper_bound, lty = 2, col = 'red', lwd = 2)
  lines(lower_bound, lty = 2, col = 'red', lwd = 2)
  legend("bottomright", legend = c("Прогноз", "Границы прогноза", "Наблюдения"), col = c("black", "red", "blue"), lty = c(1, 2), lwd = c(2, 2))
}

predict_AR2ARCH3(x_test, θ_estimates, A_estimates)

#5-7. Реальные данные
data = read.csv2("C:/Users/anyxi/Desktop/AAPL_200401_240413.csv") #котировки компании Apple на бирже США за 2020-2024гг

#график динамики, взяла значение HIGHT - наивысшую цену, достигнутую акцией в течение торгового дня

plot(data$X.HIGH.,type="l",col="seagreen", main = "Динамика изменения HIGHT")

#8-9. Привести данные к стационарному виду
#данные в столбце типа character, поэтому преобразуем к числовому
data_type = class(data$X.HIGH)
print(data_type)

P = as.numeric(data$X.HIGH)

data_type = class(P) #теперь данные числовые
print(data_type)

#к стационарному виду:
n = length(P)
z = numeric(n)
for(k in 2:n){
  z[k] = (P[k] - P[k-1]) / P[k-1] 
}

plot(z, type="l",col="red", main = "График доходностей")

#10. Шаги 2-4 для {zn} при предположении, что процесс {zn} описывается моделью AR(2)ARCH(3)

#2. Разделить последовательность {zn}, в отношении 20:1 на обучающую и тестовую выборки.
n_data = length(z)
n_train = round(length(z) * 20 / 21)  #размер обучающей выборки

x_train = z[1:n_train]  #обучающая - первые 966 значений
x_test = z[(n_train + 1):length(z)]  #тестовая - от 967 до 1014

length(x_train)
length(x_test)
length(x_train)+length(x_test)

#3. На основе обучающей выборки получить оценки параметров θ и а

#оценка параметров θ с помощью ф-ии arima() 
ar_model = arima(x_train, order = c(2, 0, 0), include.mean = FALSE)
θ_estimates = c(ar_model$coef[1], ar_model$coef[2])

#оценка параметров А с помощью ф-ии garch()
h_residuals = numeric(n_train)
h_residuals[1] = x_train[1]
h_residuals[2] = x_train[2] - θ_estimates[1] * x_train[1]

#расчет остатков для GARCH модели
for (i in 3:n_train) {
  h_residuals[i] = x_train[i] - θ_estimates[1] * x_train[i - 1] - θ_estimates[2] * x_train[i - 2]
}

#оценка параметров GARCH модели
garch_model = garch(h_residuals, order = c(3, 0))
A_estimates = c(garch_model$coef[1], garch_model$coef[2], garch_model$coef[3], garch_model$coef[4]) 

θ_estimates #оценки параметров θ
A_estimates #оценки параметров А

plot(x_test, type = 'l', col = 'blue')
predict_AR2ARCH3(x_test, θ_estimates, A_estimates)
