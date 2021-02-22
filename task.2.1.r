task.2.1<-function(){
  
  cond<-paste0("Вероятность того, что начинающая медсестра при проведении инъекции попадёт в вену с первого раза составляет ##slot_1##.",
               "Найти вероятность того, что при проведении аттестации данной медсестры удачными будут ##slot_2## из ##slot_3## инъекций.")
  rnd<-function(){
    p<-round(runif(1,0.6,0.9),2)
    m<-sample(5:9,1)
    n<-m+sample(2:4,1)
    c(p=p,m=m,n=n)
  }
  
  sol<-function(x){
    round(dbinom(x['m'],x['n'],x['p']),4)
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
      r<-runif(10,min=0, max=1)
      r<-sort(r[which(abs(r-x)/x>0.15)])
      r
    }
    temp<-foo()
    while(length(temp)<6) temp<-foo()
    sort(sample(temp,6))
  }
  
 
  answers<-function(){
    
    bad.anwers<-get.bad.anwers(right.answer)
    
    digits=4
    right.answer<-round(right.answer,digits)
    bad.anwers<-sapply(bad.anwers,round,digits=digits)
    right.answer<-paste0("\\(",right.answer,"\\)")
    bad.anwers<-paste0("\\(",bad.anwers,"\\)")
    res1<-paste0("=",right.answer)
    res2<-paste0("~",bad.anwers)
    res<-paste0(collapse="\n",c(res1,res2))
    res<-paste("{",res,"}")
    res
    # cat(res) 
  }
  
  list(condition=task(),answers=answers(),right.answer=right.answer)
  
}