
createGroupTable=function(data,classwidth){
  a=(min(data)%/%10)*10
  b=((max(data)%/%10)+1)*10
  print(a)
  print(b)
  d=b+classwidth
  lowerclass=seq(a,b,classwidth)
  upperclass=lowerclass+classwidth-1
  
  classInterval=paste(lowerclass,'-',upperclass)
  alldata=table(cut(data,seq(a-1,d-1,classwidth),labels=classInterval))
  lowerclassBoundary=lowerclass-0.5
  upperclassBoundary=upperclass-0.5
  classBoundary=paste(lowerclassBoundary,'-',upperclassBoundary)
  classmark=(upperclassBoundary+lowerclassBoundary)/2
  DataTable=data.frame(alldata,classBoundary,classmark)
  colnames(DataTable)=c('ClassInterval','frequency','ClassBoundary','ClassMark(x)')
  print(DataTable)
  frequency=DataTable$frequency
  Fx=classmark*frequency
  DataTable$Fx=Fx
  print(DataTable)
  mean=sum(Fx)/sum(frequency)
  print(paste('the mean =',expression(lambda),'fx/N'))
  print(paste('mean=',sum(fx),'/',sum(frequency)))
  themean=paste('mean=',sum(Fx)/sum(frequency))
  print(themean)
  
} 

score=c(21, 13, 16, 23, 14, 12, 14, 13 ,22,13,20, 11, 20 ,16, 28, 21,14,14,18,22,15,20,29,31,34,35,37,35)
createGroupTable(score,10)

wages = c(88,82,96,102,104,106,104,24,26,29,86,36,60,23,24,39,48,46,
          + 33,36,39,78,67,82,32,67,27,24,26,27,30,36,37,49,50,56,83,
          + 99,68,28,55,54,26,29,30,40,46,44,99,84,36,51,86,88,87,29,
          + 40,40,40,66,45,23,26,46,46,96,99,100,100,101,103,106,107,
          + 46,48,49,48,94,55,56,59,60,70,72,76,79,80,50,49,93,86,54,
          + 83,89,90,94,96,99,102,46)
createGroupTable(wages,10)

