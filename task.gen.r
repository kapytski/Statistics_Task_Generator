# возможны проблемы с кодировкой по Windows, поэтому устанавливаем принудительно utf-8
options(encoding="utf-8")

# собираем из рабочей директории адреса файлов скриптов по паттерну (здесь отбираются задания типа "task.4.*.r", где "*"-числа от 0 до 9)
tasks<-dir(pattern="task\\.4\\.[0-9]\\.r")

# вызыватель скриптов задач: на вход подаётся адрес скрипта "task"
generate.task<-function(task,n=2){
  
  # вызываем скрипт задачи
  source(task,encoding = "utf-8")
  
  # готовим имя файла, куда будет сохраняться результат работы
  f<-sub("\\.r$","",task)
  
  # помещаем результаты n-кратного вызова скрипта в выходную строку "out" 
  out<-""
  for(i in 1:n){
    res<-eval(call(f))
    res<-paste0(res$condition,"\n",res$answers,"\n")
    out<-paste0(out,res,"\n")
  }
  
  #   записываем полученную строку "out" в файл с именем, совпадающим с названием
  # адреса файла-скрипта, но с расширением txt
  cat(out,file=paste0(f,".txt"))
  
}

# неявный цикл n-кратного вызова каждого файла-скрипта из вектора "tasks" 
sapply(tasks,generate.task,n=300)
