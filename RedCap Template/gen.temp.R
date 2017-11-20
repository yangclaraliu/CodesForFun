#' this is the function used to generate the template for RedCap entries with more than 20 effect estimates
#' @param PMID is the PubMed ID of the paper that you are working with, it sould be a number
#' @param no_estimates captures the number of estimates
#' @initial should be your initial as a character string


gen.temp <- function(PMID,no_estimates,initial){
  if(!require(RISmed)) install.packages("RISmed")
  if(grepl('^[A-Za-z]+$', PMID)) stop("This PubMed ID does not look right. :(\n")
  if(!(initial %in% c("YL","MS","AH","YS","ZK","JS"))) stop("This is an initial of somebody we don't know. :(\n")
  cat(paste("Please check if the title matches with what you are working with:\n",ArticleTitle(EUtilsGet(PMID)),"\n"))
  if(no_estimates<=20) stop("Too Few Effect Estimates to Access this Template. :(\n")
  temp_head <- read.csv("RedCap_Template.csv")
  temp_head[,paste("ee",1:no_estimates,sep="")] <- data.frame(matrix(nrow=nrow(temp_head),ncol=no_estimates))
  file_name <- paste(PMID,"_",initial,".csv",sep="")
  cat(paste("\nYour template named ", file_name, "has been saved to ",getwd(),".",sep=""))
  write.csv(temp_head,file = file_name)
}


