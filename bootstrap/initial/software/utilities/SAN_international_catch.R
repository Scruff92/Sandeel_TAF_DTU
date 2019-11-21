# script to read total international catches and make a tables for markdown and output folder 


a<-read.table(file.path(root,'Data','officialcatches_IV_2017.txt'),stringsAsFactors=F, header=TRUE)
a[,-1]<-a[,-1]/1000 #change scale to 1000s of tonnes
a[a==0]<-"-"
print(knitr::kable(a,align=c('l',rep('r',9)), digits=1))


