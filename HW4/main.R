  #Введём N
 
 N=10000
  #Построим выборку N>=10^4 с помощью генератора случайных чисел

  a = -1  # z=[a,b] - диагональ квадрата
  b = 1.5 #

  gamma =  function(x){
   y = (-abs(x - (a+b)/2) + (b-a)/2 ) # Задаем половину квадрата функцией модуля
   return(y)
  }
  
  deriv=function(x){return(-sign(x - (a+b)/2))} 
  int = function(x){
    y = (abs(gamma(x))*sqrt(1+deriv(x)^2)) # Подынтегральная функция М
    return(y)
  }
  
  g = Vectorize(function(t){
    y = integrate(int,a,t)$value
    return(y)
  })
  
  x=seq(a, b, 0.01)
  q = Vectorize(function(x){                                # Задаем квантильную функцию, исходя из соотношения g(t)-x=0, где g - функция распределения
    y = uniroot(function(t) g(t)-x, c(a, b))     # t - необходимое нам значение квантиля, а x - вероятность
    return(y$root)
  })
        generate=function(t){
          y=runif(t,0,1)
          return(q(y))
        }

        generate_on=function(t){
          z=generate(t)
          alpha=runif(t,0,2*pi)
          x=sin(alpha)*gamma(z)
          y=cos(alpha)*gamma(z)
          matrice=data.frame(x,y,z)
          return(matrice)
        }

        data=generate_on(N)
        
          mod_data=function(v1,v2){ #Возвращает модуль (длину) расстояния между точками v2 и v1
            mod=sqrt(sum((v2-v1)^2))
            return(mod)
          }
        
          r = c()                                        
          mean = c()
          disp = c()
          sum = 0
          n = (1:N)
          for (i in n){
            d = mod_data(c(data$x[i],data$y[i],data$z[i]),
                         c(data$x[N-i],data$y[N-i],data$z[N-i]))
            r[i] = d
            sum = sum + d
            
            mean[i] = sum/i                # Среднее зн-е.
            disp[i] = sum( (r - mean)^2 )/i  # Выб. дисперсия.
          }
          

          
  #Построим график зависимости среднего арифметического первых n элементов от n.
          setwd("D:\\вуз\\R\\programs\\HW4")
          pdf("plots.pdf")
          plot(n, mean, type='l', pch = '.', ylim = c(0.5,2))
          
  #Доверительные интервалы
          
          conf_level = 0.95
          signif_level = 1 - conf_level
          
          tmp = 0
          for(i in 1:N) {
            j = i * 50
            low = mean[j] - sqrt(disp[j]/j/signif_level) # Дов. интервал с использованием неравенства Чебышева
            upp = mean[j] + sqrt(disp[j]/j/signif_level)
            if (upp - low < 0.1 && j == 0) {
              tmp = j
            }
            lines(rep(j, 2), c(low, upp), col = "red")
          }
          
          plot(n, disp, type = 'p', pch = '.', ylim = c(0, 0.5))
          for(i in 1:N) {
            j = i * 50
           low = (j - 1) * disp[j] / qchisq(0.975, j - 1)
           upp = (j - 1) * disp[j] / qchisq(0.025, j - 1)
            lines(rep(j, 2), c(low, upp), col = "red")
          }
          
  # --------------------------------------------------------------------
  # Критерий честности монеты
  # --------------------------------------------------------------------
          
  # Для честной монеты вероятность "успеха" - 50%.  
  # Далее нас интересует распределения количества успехов в 
  # последовательности длины N независимых случайных экспериментов (подбрасываний монетки)
  # с постоянной вероятностью p=0.5,
  # то есть нас интересует биномиальное распределение.
          
          a = qbinom(0.005, N, 0.5)
          b = qbinom(0.995, N, 0.5)
          
  # Выбрав уровень значимости 0.01, установим левую (нижнюю) границу a и правую (верхнюю) границу b - значения,
  # за которые количество успехов не выйдут в случае, если критерий верен,
  # с вероятностью ошибки, равной 0.01 (по 0.005 на каждую из сторон).
          
  # Проверим честность монеты.
          
          coin = function(n) sample(0:10, n, replace = TRUE) %% 2
          c = coin(n)
          s = sum(c)
          res = 0
          if (s >= a && s <= b) res = 1
          res
          
  # Полученный результат - 0, следовательно, монету нельзя считать честной.
          
    dev.off()