#Heirarchical nested example where you should really be using a glmm

pkgbuild::compile_dll()
roxygen2::roxygenize()

RegionPrevs <- c(A = 0.5, B = 2, C = 4)/100 #Prevalence in each region
NumRegions <- length(RegionPrevs)
NumVillages <- 10 #Villages per Region
NumSites <- 15 #Sites per village
MeanCatch <- 100; SDCatch <- 30 #Mean and SD of mosquito catch sizes
DispersionCatch <- 1.5
MaxPoolSize <- 25

Data <- data.frame()
TruePrev <- data.frame()
for(R in names(RegionPrevs)){
  for(V in 1:NumVillages){

    VillageTruePrev <- plogis(qlogis(RegionPrevs[[R]]) + rnorm(1,mean = 0, sd = 0.5))

    for(S in 1:NumSites){

      SiteTruePrev <- plogis(qlogis(VillageTruePrev) + rnorm(1,mean = 0, sd = 0.25))


      TruePrev <- rbind(TruePrev,
                        data.frame(Region = R,
                                   Village = paste(R,V,sep = "-"),
                                   Site = paste(R,V,S,sep = "-"),
                                   PrevalenceRegion = RegionPrevs[[R]],
                                   PrevalenceVillage = VillageTruePrev,
                                   PrevalenceSite = SiteTruePrev))

      #Generate catch sizes from zero-truncated negative binomial distribution. 're-roll' sizes <= 0 to guarantee at least one mossie
      CatchSizeMult <- SiteTruePrev/RegionPrevs[[R]]
      Catch <- 0
      while(Catch<=0){
        Catch <- rnbinom(1,mu = MeanCatch * CatchSizeMult,DispersionCatch)
      }
      NumBigPools <- Catch %/% MaxPoolSize
      if(NumBigPools){
        Data <- rbind(Data,
                      data.frame(Region = R,
                                 Village = paste(R,V,sep = "-"),
                                 Site = paste(R,V,S,sep = "-"),
                                 PoolSize = rep(MaxPoolSize,NumBigPools),
                                 PrevalenceSite = SiteTruePrev,
                                 PrevalenceVillage = VillageTruePrev,
                                 PrevalenceRegion = RegionPrevs[[R]]))
      }
      SizeSmallPool <- Catch %% MaxPoolSize
      if(SizeSmallPool){
        Data <- rbind(Data,
                      data.frame(Region = R,
                                 Village = paste(R,V,sep = "-"),
                                 Site = paste(R,V,S,sep = "-"),
                                 PoolSize = SizeSmallPool,
                                 PrevalenceSite = SiteTruePrev,
                                 PrevalenceVillage = VillageTruePrev,
                                 PrevalenceRegion = RegionPrevs[[R]]))
      }

    }
  }
}
Data$Result <- with(Data,as.numeric(runif(nrow(Data)) < 1-(1-PrevalenceSite)^PoolSize))

#Calculating the mean prevalence in the whole population and each Region,
#without adjusting for the psuedoreplicaiton introduced by the heiracrchical sampling frame
UnadjustedMean <-  PoolLogitReg(data = Data,
                                formula = Result ~ 1,
                                PoolSize)
plogis(coef(UnadjustedMean))
plogis(confint(UnadjustedMean))

UnadjustedMean.Region <- PoolLogitReg(data = Data,
                                      formula = Result ~ Region,
                                      PoolSize)
plogis(coef(UnadjustedMean.Region) + c(0,rep(coef(UnadjustedMean.Region)['(Intercept)'],2)))
confint(UnadjustedMean.Region)

#Adjusting now for sampling frame
AdjustedMean <- PoolLogitRegMixed(Data,
                                  Result ~ 1 + (1|Region/Village/Site), #Adding site sometimes causes convergence issues
                                  PoolSize)
summary(AdjustedMean)
plogis(coef(AdjustedMean)$Region$`(Intercept)`)
confint(AdjustedMean) ## THIS SHOULD WORK BUT DOESN'T

#This shoudl be the same as above, but for some reason confint doesn't work on the above
memPool <- glmer(data = Data,
                 formula = Result ~ Region + (1|Village/Site),
                 family = binomial(PoolLink(Data$PoolSize)))
summary(memPool)
confint(memPool)


Data %>%
  select(Region,Village,Site) %>%
  mutate(PoolSize = 1) %>%
  unique %>% cbind(.,Prev = predict(memPool,
                                    type = 'link',
                                    re.form = ~ (1|Village/Site),
                                    newdata = .) %>% plogis)


## Can I write a general case for the above which calculates the prevalence at every level?

Formula <- attr(memPool,'call')$formula


GroupTermNames <- (as.character(Formula)[[3]] %>%
                     str_match_all("\\|\\s*(.*?)\\s*\\)"))[[1]][,2] %>%
  strsplit(split = "\\/") %>%
  unlist

GroupTerms <- findbars(Formula)
NGroupTerms <- length(GroupTerms)
PredData <- memPool %>%
  attr('frame') %>%
  select_at(-c(1)) %>%
  mutate(PoolSize = 1) %>%
  unique
pred <- PredData
for(n in 1:NGroupTerms){
  GroupEffectForm <- reformulate(paste0("(",as.character(GroupTerms[(NGroupTerms-n+1):NGroupTerms]),")"))

  Prev = predict(memPool,
                 type = 'link',
                 re.form = GroupEffectForm,
                 newdata = PredData) %>% plogis
  pred <- cbind(pred, Prev =  Prev)
  colnames(pred)[ncol(pred)] <- paste(GroupTermNames[n],"Prev")
}
ColNames <- colnames(pred)
ColNames[NTerms+ 1 +(1:NTerms)] <- paste0(ColNames[1:NTerms],"Prev")
colnames(pred) <- ColNames
pred <- pred %>% select(-PoolSize)
pred


library(stringr)
a <- "(1|abcd/efgh) + (1|ajajaj)"
res <- str_match_all(a, "\\|\\s*(.*?)\\s*\\)")
res
res[[1]][,2]



##Bayesian version with brms
library(brms)

bform <- bf(Result ~ 1 - (1-inv_logit(eta))^PoolSize,
            eta ~ Region + (1|Village/Site),
            nl = TRUE)

nlprior <- c(prior(normal(0,2), nlpar = "eta")) #check that this is infact a sensible prior!
fit <- brm(bform, family = bernoulli("identity"),
           data = Data,
           prior = nlprior,
           cores = 4)


fitted(fit,
       scale = 'response',
       newdata = Data %>% select(-Result) %>% mutate(PoolSize = 1) %>% unique)

fitted(fit,
       scale = 'response',
       re_formula = NA,
       newdata = Data %>% select(Region) %>% mutate(PoolSize = 1) %>% unique)

PredictedVillage <- Data %>% select(Region,Village) %>% mutate(PoolSize = 1) %>% unique %>%
  cbind(.,fitted(fit,scale = 'response',
                 re_formula = ~ (1|Village),
                 newdata = .)) %>% select(-PoolSize)

PredictedSite <- Data %>% select(Region,Village,Site) %>% mutate(PoolSize = 1) %>% unique %>%
  cbind(.,fitted(fit,scale = 'response',
                 re_formula = ~  (1|Village/Site),
                 newdata = .)) %>% select(-PoolSize)
PredictedSite



a <- PoolRegBayes(Data, Result ~ Region + (1|Village/Site), PoolSize)

GetPrevs <- function(fit,data, formula, PoolSize){
  PoolSize <- enquo(PoolSize)
  data <- rename(data, PoolSize = !! PoolSize)

  GroupTermNames <- (as.character(formula)[[3]] %>%
                       str_match_all("\\|\\s*(.*?)\\s*\\)"))[[1]][,2] %>%
    strsplit(split = "\\/") %>%
    unlist
  NGroupTerms <- length(GroupTermNames)

  AllTerms <- intersect(colnames(data),unique(c(GroupTermNames, attr(terms(formula),"term.labels"))))
  PopTerms <- setdiff(AllTerms,GroupTermNames)

  PredData <- data[,AllTerms] %>% unique %>% mutate(PoolSize = 1)


  pred <- PredData

  Prev = fitted(fit,scale = 'response',re_formula = NA,newdata = PredData) %>%
    as.data.frame %>%
    select(-Est.Error)

  colnames(Prev) = paste('FixedEffects',c("Estimate", "CrILow","CrIHigh"),sep = ".")

  pred <- cbind(pred, Prev)

  predlist <- list(cbind(PredData[,PopTerms,drop =F],Prev) %>% unique)

  for(n in 1:NGroupTerms){

    GroupEffectForm <- paste("(1|",GroupTermNames[1:n] %>% paste(collapse = "/"),")",sep = "") %>% reformulate()
    Prev = fitted(fit,
                  scale = 'response',
                  re_formula = GroupEffectForm,
                  newdata = PredData) %>%
      as.data.frame %>%
      select(-Est.Error)
    colnames(Prev) = paste(GroupTermNames[n],c("Estimate", "CrILow","CrIHigh"),sep = ".")
    pred <- cbind(pred, Prev)
    predlist <- c(predlist, list(cbind(PredData[,c(PopTerms,GroupTermNames[1:n])],Prev) %>% unique))
  }
  pred <- pred %>% select(-PoolSize)

  names(predlist) <- c("FixedEffects", GroupTermNames)

  return(list(pred,predlist))
}
p <- GetPrevs(a,Data,Result ~ Region + (1|Village/Site), PoolSize)
p

PredictedRegion <- Data %>% select(Region) %>% mutate(PoolSize = 1) %>% unique %>%
  cbind(.,fitted(a,scale = 'response',
                 re_formula = ~ NULL,
                 newdata = .)) %>% select(-PoolSize)
PredictedRegion

PredictedVillage <- Data %>% select(Region,Village) %>% mutate(PoolSize = 1) %>% unique %>%
  cbind(.,fitted(a,scale = 'response',
                 re_formula = ~ (1|Village),
                 newdata = .)) %>% select(-PoolSize)
PredictedVillage

PredictedSite <- Data %>% select(Region,Village,Site) %>% mutate(PoolSize = 1) %>% unique %>%
  cbind(.,fitted(a,scale = 'response',
                 re_formula = ~ (1|Village/Site),
                 newdata = .)) %>% select(-PoolSize)





