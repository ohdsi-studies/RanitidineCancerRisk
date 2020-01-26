createTcosOfInterest<-function(workFolder,
                               excludedTargetComparatorConceptIds = NULL,
                               secondaryComparison = "2",
                               outcomeIds = 1:20,
                               includedCovariateConceptIds = "",
                               updateNegativeControls = T,
                               negativeControlConceptIds = NULL){
  conceptSets<-read.csv(file.path(workFolder,"conceptSetExpression.csv"),
                        stringsAsFactors = FALSE)
  colnames(conceptSets)<-gsub("\\.","_",colnames(conceptSets))
  colnames(conceptSets)<-SqlRender::snakeCaseToCamelCase(colnames(conceptSets))
  conceptSets <- conceptSets[!conceptSets$conceptId %in% excludedTargetComparatorConceptIds,]
  
  drugConceptIds<-conceptSets$conceptId[conceptSets$name=="H2blocker"]
  
  #tcs<-as.data.frame(t(combn(drugConceptIds,2))) #combination
  tcs<-expand.grid(rep(list(drugConceptIds),2)) #permutation without repetition
  tcs<-tcs[tcs[,1]!=tcs[,2],] #remove identical target and comparator
  if(as.numeric(secondaryComparison)){
    tcsSen <- tcs
    tcsSen[,1] <- paste0(tcsSen[,1],secondaryComparison)
    tcsSen[,2] <- paste0(tcsSen[,2],secondaryComparison)
    tcs <- rbind(tcs,tcsSen)
    
  }
  colnames(tcs)<-c("targetId","comparatorId")
  
  outcomeIds<-paste0(outcomeIds,collapse=";")
  excludedCovariateConceptIds<-paste0(drugConceptIds,collapse=";")
  
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

