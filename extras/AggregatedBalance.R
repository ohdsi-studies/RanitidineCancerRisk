####Target covariate Ids####
#Medical conditions
targetCovIds1 <- c(4212540210,255573210,201606210,4182210210,440383210,201820210,439727210,432867210,316866210,80180210,4030518210,80809210,4279309210,81893210,197494210)
covOverlap %>% filter(covariateId %in% targetCovIds)
#GI
targetCovIds2 <- c(318800210, 4027663210, 201340210)
covOverlap %>% filter(covariateId %in% targetCovIds2)
#Drug
targetCovIds3 <- c(21604686410, 21603890410, 21600960410, 21601853410, 21604489410, 21603932410, 21602723410, 21602472410)

covIdsCategorical <- covOverlap %>%
  filter(
    (covariateId %in% c(targetCovIds1,targetCovIds2,targetCovIds3))|
    covariateAnalysisId %in% c(1,3,4,5,6)
    ) %>%
  pull(covariateId)

categoricalCovs %>% pull(covariateId)

balance %>% filter(analysisId == 1)
balance %>% filter(analysisId == 3)
balance %>% filter(analysisId == 4)
balance %>% filter(analysisId == 5)
balance %>% filter(analysisId == 6)

balance %>% filter(analysisId == 2) #age in number
balance %>% filter(analysisId == 7) #index month
unique(balance$analysisId)

####SMD####
##SMD = Difference in mean outcome between groups / standard deviation of outcome among participants
beforeMatchingSeq <- 4
afterMatchingSeq <- 5

##Categorical values
# i = 1
for(i in seq(length(dbsPrime))){
  if(i==1) aggBals <- data.frame()
  print(i)
  singleBal <- 
    getBalance(databaseId = dbsPrime[i],
                          dataFolder = dataFolder,
                          targetId = targetIdPrime,
                          comparatorId = comparatorIdPrime,
                          analysisId = analysisIdPrime,
                          outcomeId = outcomeIdPrime) %>%
    filter(covariateId %in% covIdsCategorical)
  
  singleAttr <- attrition %>% filter(databaseId == dbsPrime[i],
                       targetId == targetIdPrime,
                       comparatorId == comparatorIdPrime,
                       analysisId == analysisIdPrime,
                       outcomeId == outcomeIdPrime)
  
  singleBal <- singleBal %>% mutate(
    databaseId = dbsPrime[i], 
    beforeTargetPop = singleAttr %>% 
      filter(sequenceNumber == beforeMatchingSeq, exposureId == unique(targetId)) %>%
      pull(subjects),
    beforeComparatorPop = singleAttr %>% 
      filter(sequenceNumber == beforeMatchingSeq, exposureId == unique(comparatorId)) %>%
      pull(subjects),
    afterTargetPop = singleAttr %>% 
      filter(sequenceNumber == afterMatchingSeq, exposureId == unique(targetId)) %>%
      pull(subjects),
    afterComparatorPop = singleAttr %>% 
      filter(sequenceNumber == afterMatchingSeq, exposureId == unique(comparatorId)) %>%
      pull(subjects)
    )
  aggBals <- rbind(aggBals, singleBal)
}

undebug(aggregateCategoricalBalance)
df <- aggBals %>%
  mutate(group = covariateId) %>%
  group_by(group) %>%
  group_map(~aggregateCategoricalBalance(.x))
do.call(rbind.data.frame, df)


aggBals <- aggBals %>% filter(covariateId == 80180210)

aggregateCategoricalBalance(aggBals)
aggregate(aggBals, by = list(as.numeric(factor(aggBals$covariateId))), FUN = aggregateCategoricalBalance)

df<-sapply(split(aggBals,factor(aggBals$covariateId)), aggregateCategoricalBalance)
do.call(rbind.data.frame, df)

ParallelLogger::clusterApply()