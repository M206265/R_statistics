  {n=3
G=matrix(nrow=2^n, ncol=n)
G[1,n]=1
G[2,n]=-1
p=2
  for(i in 2:n){ 
    t=p
    p=p*2
      for(k in (p/2+1):p) {  # отзеркаливание
        for(x in 0:(n-1)){   # кода
        G[k,n-x]=G[t,n-x]}   #
        
      G[t,n+1-i]=1   # добавление 1 впереди кода
      G[k,n+1-i]=-1  # добавление -1 впереди кода
      t=t-1
      }
  }
G
}

  answer=0
  q=1
                          # расчёт углов
   for(l in 1:(2^n-1)){
    for(m in (l+1):(2^n)){ 
        a=sum((G[l,])^2)
        b=sum((G[m,])^2)
        modules = sqrt(a)*sqrt(b)
        scal=G[l,]%*%G[m,]
        if (Mod(scal/modules) > 1) {answer[q]=180} else {
        angle = function(scal, modules) acos(scal/modules) 
        answer[q]=180*angle(scal,modules)/pi }
        q=q+1 }}  

  print(answer)
  mean(answer)  
  var(answer)  
  unique(answer)
  ans=hist(answer) 
  pdf( file="Hist.pdf")
  ans
  dev.off()
    
  