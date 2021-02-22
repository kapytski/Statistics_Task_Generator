# tasks<-dir(pattern="task\\.[0-9]\\.[0-9]\\.r")
tasks<-dir(pattern="task\\.4\\.[0-9]\\.r")

# task<-tasks[1]
generate.task<-function(task,n=2){
  source(task,encoding = "utf-8")  
  f<-sub("\\.r$","",task)
  # res<-paste0(res,"()")
  out<-""
  for(i in 1:n){
    res<-eval(call(f))
    res<-paste0(res$condition,"\n",res$answers,"\n")
    out<-paste0(out,res,"\n")
  }
  # 
  cat(out,file=paste0(f,".txt"))
  
}

sapply(tasks,generate.task,n=300)