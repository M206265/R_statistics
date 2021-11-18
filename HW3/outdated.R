a = -1  # z=[a,b] - диагональ квадрата
b = 1.5 #

gamma =  function(x){
  y = (-abs(x - (a+b)/2) + (b-a)/2 ) # Задаем половину квадрата функцией модуля
  return(y)
}
x=seq(a, b, 0.01) # Задаем границы промежутка
plot(x, gamma(x), type="l", ylim=c(a,b)) # Проверяем, что функция действительно задает необходимую нам кривую

deriv=function(x){return(-sign(x - (a+b)/2))} 
int = function(x){
  y = (abs(gamma(x))*sqrt(1+deriv(x)^2)) # Подынтегральная функция М
  return 
}
  M = integrate(int,a,b)$value 

f = Vectorize(function(x){
  y =int(x)/M
  return(y)
})
            x=seq(a,b,0.01)
            plot(x, f(x), type="l", ylim=c(a,b) ) # График плотности распределения

g = Vectorize(function(t){
  y = integrate(int,a,t)$value
  return(y)
})


quant = Vectorize(function(x){                                # Задаем квантильную функцию, исходя из соотношения g(t)-x=0, где g - функция распределения
 y = uniroot(function(t) g(t)-x, c(a, b))     # t - необходимое нам значение квантиля, а x - вероятность
 return(y$root)
})

q13 = quant(c(0.25, 0.75)) # q13 - массив из значений нижнего и верхнего квантилей
#q2 = q(c(0.5)) # q2 - медиана
q13          
quantile(g(x))
          x=seq(-2,2.5,0.01)

          p=c(0.25,0.75)
          q=quantile(g(x),p)
          plot(x, f(x), type="l") # График плотности распределения
          lines(q, f(q), type="h", col="blue", lty="dashed") # Линии квартилей
          lines(median(g(x)), y = f(median(g(x))), type = "h", col = "red", lty = "dashed") # Линия медианы
          t=seq(quantile(g(x),p=c(0.25)), quantile(g(x),p=c(0.75)), 0.01)
          polygon(c(t, rev(t)), c(f(t), rep(0,length(t))), 
                  border = FALSE, density = 10, lwd = 0.7, col = 'blue')
          
          plot(x,g(x),type="l",ylim=c(0,3)) # График интегральной функции распределения
          lines(q, g(q), type="h", col="blue", lty="dashed") # Линии квартилей
          lines(median(g(x)), y = g(median(g(x))), type = "h", col = "red", lty = "dashed") # Линия медианы
          t=seq(quantile(g(x),p=c(0.25)), quantile(g(x),p=c(0.75)), 0.01)
          polygon(c(t, rev(t)), c(g(t), rep(0,length(t))), 
                  border = FALSE, density = 10, lwd = 0.7, col = 'blue')
          
          # Вероятность того, что значение случайной величины находится между нижним и верхним квартилями (в интерквартильном размахе) равно 50%
          # и не зависит от функции распределения.
          
          x=seq(0,1,0.01)
          plot(x, quant(x), type="l") # Квантильная функция     
          
          table_of_quantiles = data.frame(x, quantile(g(x),seq(0,1,0.1))) # Таблица квантилей
          View(table_of_quantiles)

          
  generate=function(t){
    y=runif(t,0,1)
    return(quant(y))
  }
  generate_on=function(t){
    z=generate(t)
    alpha=runif(t,0,2*pi)
    x=sin(alpha)*gamma(z)
    y=cos(alpha)*gamma(z)
    matrice=data.frame(x,y,z)
  return(matrice)
  }

            data=generate_on(1000)
          plot(data$x, data$z, pch='.')
          plot(data$x, data$y, pch='.')
          
          
          smoothScatter(data$x, data$z,
                        nrpoints = 1000, nbin = 256,
                        colramp =  colorRampPalette(c("blue", "orange", "red"), space = "Lab"),
                        transformation = function(x) (x) )
          
          smoothScatter(data$x, data$y,
                        nrpoints = 1000, nbin = 252,
                        colramp =  colorRampPalette(c("blue", "orange", "red"), space = "Lab"),
                        transformation = function(x) (x) )
          
          