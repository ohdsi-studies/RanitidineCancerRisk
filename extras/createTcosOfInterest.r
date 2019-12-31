createTcosOfInterest<-function(workFolder,
                               excludedTargetComparatorConceptIds = c(43009003),
                               outcomeIds=c(549,550),
                               includedCovariateConceptIds="",
                               updateNegativeControls=T,
                               negativeControlConceptIds=NULL){
  conceptSets<-read.csv(file.path(workFolder,"conceptSetExpression.csv"),
                        stringsAsFactors = FALSE)
  colnames(conceptSets)<-gsub("\\.","_",colnames(conceptSets))
  colnames(conceptSets)<-SqlRender::snakeCaseToCamelCase(colnames(conceptSets))
  conceptSets <- conceptSets[!conceptSets$conceptId %in% excludedTargetComparatorConceptIds,]
  
  drugConceptIds<-conceptSets$conceptId[conceptSets$name=="H2blocker"]
  tcs<-as.data.frame(t(combn(drugConceptIds,2)))
  colnames(tcs)<-c("targetId","comparatorId")
  
  outcomeIds<-paste0(outcomeIds,collapse=";")
  excludedCovariateConceptIds<-paste0(drugConceptIds,collapse=";")
  includedCovariateConceptIds<-""
  
  tcosOfInterest<-cbind(tcs,outcomeIds,excludedCovariateConceptIds,includedCovariateConceptIds)
  
  if(updateNegativeControls){
    negativeControls <- read.csv(file.path(workFolder,"NegativeControls.csv"),
                                 stringsAsFactors = FALSE)
    uniqueNegativeControls<-unique(negativeControls[,c("outcomeId", "outcomeName")])
    for(i in 1:nrow(tcosOfInterest)){
      if(i==1){negativeControls=data.frame()}
      negativeControl<-uniqueNegativeControls
      negativeControl$targetId<-tcosOfInterest$targetId[i]
      negativeControl$comparatorId<-tcosOfInterest$comparatorId[i]
      negativeControl$type<-"outcome"
      
      negativeControls<-rbind(negativeControls,negativeControl)
      
    }
    write.csv(negativeControls,file.path(workFolder,"NegativeControls.csv"), row.names=FALSE)
  }
  
  write.csv(tcosOfInterest,file.path(workFolder,"TcosOfInterest.csv"), row.names=FALSE)
  
}

