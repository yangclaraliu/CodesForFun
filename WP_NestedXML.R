require(XML)
setwd("~/Documents/NSF")
years <- list.files()
nsf <- list()

#testing with NSF year of 1976
setwd("~/Documents/NSF/2018")
all <- list.files()
current <- xmlInternalTreeParse(all[1], useInternalNodes = T)
node_list <- xmlToList(xmlInternalTreeParse(all[1]))
labels <- paste("//",gsub("\\.","/",substring(names(unlist(node_list)),7)),sep="")
target <- substring(names(unlist(node_list)),7) [c(1:5,7:8,10,13,19:21,25,28,30)]

for (y in 6:43){
  path <- paste("~/Documents/NSF/",years[y],sep="")
  setwd(path)
  all <- list.files()
  nsf[[y]] <- data.frame(matrix(NA,ncol=length(target),nrow=length(all)))
  colnames(nsf[[y]]) <- target
  for(i in 1:length(all)){
    current <- xmlInternalTreeParse(all[i], useInternalNodes = T)
    node_list <- xmlToList(xmlInternalTreeParse(all[i]))
    labels <- paste("//",gsub("\\.","/",substring(names(unlist(node_list)),7)),sep="")
    xml.res <- data.frame(cbind(sapply(1:length(labels),function(l) xpathSApply(current,labels[l],function(x) xmlSApply(x,xmlValue))),substring(names(unlist(node_list)),7)))
    colnames(xml.res) <- c("Value","Label")
    xml.res <- apply(xml.res,2,as.character)
    for (c in 1:length(target)){
      if (length(which(xml.res[,2]==colnames(nsf[[y]])[c]))==1) {
        nsf[[y]][i,c] <- paste(xml.res[which(xml.res[,2]==colnames(nsf[[y]])[c]),1],collapse="|")
      }
    }
  }
}

