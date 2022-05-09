
set.seed(123)
mydf <- data.table(renda = rnorm(1000,1000,100)
                   ,pessoas = round(rnorm(n = 1000,mean = 2,sd = 1)))
mydf[pessoas <= 0,pessoas := 1]
table(mydf$pessoas)

mydf[, decile:= as.numeric( cut(renda
                                , breaks=quantile(x = renda,probs = seq(0, 1, by=0.1), na.rm=T)
                                ,include.lowest= TRUE, labels=1:10))]
mydf[, decile_wtd :=  as.numeric( cut(renda
                                      ,breaks = Hmisc::wtd.quantile(x = renda
                                          , weights = pessoas
                                          , probs=seq(0, 1, by=0.1)
                                          , na.rm=T),include.lowest= TRUE, labels=1:10))]


mydf[,.N,by = decile]
mydf[,.N,by = decile_wtd]
