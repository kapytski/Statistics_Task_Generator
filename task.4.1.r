task.4.1<-function(){
  
  cond<-paste0("Вероятность того, что пациенту в палате понадобится помощь медсестры равна \\(p\\=##slot_1##\\).",
               " Построить закон распределения случайной величины \\(X\\) – количества пациентов в 4-х местной палате,",
               " которым понадобится помощь медсестры. Считать, что помощь каждому пациенту может понадобится",
               " независимо от остальных. По полученному закону распределения найти математическое ожидание \\(\\mu\\)",
               " и среднее квадратическое отклонение \\(\\sigma\\) данной случайной величины.")
  rnd<-function(){
    p<-round(runif(1,0.6,0.9),2)
  }
  
  sol<-function(x){
    round(c(m=x*4,s=4*x*(1-x)),3)
  }
  
  x<-rnd()
  
  right.answer<-sol(x)
  
  task<-function(){
    # task.numbers<-c("i"=4,"j"=7)
    foo<-function(i,cond){sub(paste0("##slot_",i,"##"),x[i],cond)}
    res<-foo(1,cond)
    if(length(x)>1){
      for(j in 2:length(x)) res<-foo(j,res)
    }
   res 
  }
  
  
  get.bad.anwers<-function(x){
    foo<-function(){
      r<-runif(10,min=0, max=1)*2*x
      r<-sort(r[which(abs(r-x)/x>0.15)])
      r
    }
    temp<-foo()
    while(length(temp)<6) temp<-foo()
    sort(sample(temp,6))
  }
  
 
  answers<-function(){
    
    bad.anwer_m<-get.bad.anwers(right.answer['m'])
    bad.anwer_s<-get.bad.anwers(right.answer['s'])
    
    digits=3
    right.answer<-round(right.answer,digits)
    foo<-function(x,l){
      paste0("\\(",l,"\\=",x,"\\)")
    }
    right.answer<-paste0(foo(right.answer['m'],"M"),", ",foo(right.answer['s'],"\\sigma"))
    
    bad.anwer_m<-sapply(bad.anwer_m,round,digits=digits)
    bad.anwer_s<-sapply(bad.anwer_s,round,digits=digits)
    bad.anwer_m<-sapply(bad.anwer_m,foo,l="m")
    bad.anwer_s<-sapply(bad.anwer_s,foo,l="\\sigma")
    
    bad.anwers<-paste0(bad.anwer_m,", ",bad.anwer_s)
    res1<-paste0("=",right.answer)
    res2<-paste0("~",bad.anwers)
    res<-paste0(collapse="\n",c(res1,res2))
    res<-paste("{",res,"}")
    res
    # cat(res) 
  }
  
  list(condition=task(),answers=answers(),right.answer=right.answer)
  
}