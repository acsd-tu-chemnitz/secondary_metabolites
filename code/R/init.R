source("./code/R/libload.R")
source("./code/R/util_functions.R")


totalCores = detectCores()
cluster <- makeCluster(totalCores[1]-1)
registerDoParallel(cluster)                                        # read data
metabolites <- read_excel("data/raw/metabolites_summary_2019_.xlsx",sheet = "Tabelle1")
climadata <- read_excel("data/raw/climadata_collector_2019_jun-nov.xlsx",sheet = "Klimadaten kollektor_2019_jun-n")
# select dates
dates_of_interest <- unique(na.omit(metabolites$Date))
nsplit <- length(dates_of_interest) -1
lidxx <- foreach (iter = 1:nsplit, .combine=rbind) %dopar% {climadata$Zeit >dates_of_interest[iter]& climadata$Zeit<=dates_of_interest[iter+1]}
stopCluster(cluster)
ltempxx <- queue()
ltranspxx <- queue() # transpiration
lphotoxx <- queue()  # photosynthesis
lhumxx <- queue()    # humidity
lco2xx <- queue()    # co2
lradxx <- queue()    # radiation
metabolites <- metabolites[-c(1),]
metabolites_ <- metabolites[metabolites$Substrat == "rock wool",]
metabolites_ <- head(metabolites_,-3)
lutein <- metabolites_$"Lutein [%]"
beta_caotin <- metabolites_$"ß-Caotin [%]"
lycopen <- metabolites_$"Lycopen [%]"
lutein <- as.numeric(metabolites_$Lutein...9)
beta_caotin <- as.numeric(metabolites_$"ß-Caotin...10")
lycopen <- as.numeric(metabolites_$Lycopen)
bind_full <- rbind(lutein,beta_caotin,lycopen)
bind_fulln <- normalize_input(bind_full)
coumaric <- as.numeric(metabolites_$"coumaric acid hexosid...31")
ferulic <- as.numeric(metabolites_$"ferulic acid hexoside...32")
caffeic <- as.numeric(metabolites_$"caffeic acid derivates sum...33")
caffeoyl <- as.numeric(metabolites_$"caffeoyl quinic acid derivates sum...34")
coumaryl <- as.numeric(metabolites_$"coumaryl quinic acid sum...35")
naringenin <- as.numeric(metabolites_$"naringenin...37")
quercetin <- as.numeric(metabolites_$"quercetin...38")
phloretin <- as.numeric(metabolites_$"phloretin diglucoside...39")
total_carotenoids <- as.numeric(metabolites_$"total carotenoids...12")
total_phenolic <- as.numeric(metabolites_$"total phenolic acids...36")
total_flavonoids <- as.numeric(metabolites_$"total flavonoids...40")



set.seed(52)
proj_bind <- Rtsne(t(bind_fulln), perplexity = 4)
                                        # kmeans on proj_bind
km <- kmeans(proj_bind$Y,2,25)
group <- metabolites_$Date
proj_bind_full <- cbind(proj_bind$Y,metabolites_$Date)
svglite("tsne_results.svg",width = 10, height = 10)
plot(proj_bind$Y[,1],proj_bind$Y[,2],pch=19,col=factor(group))
points(km$centers, pch = 60)
legend("topleft",
       legend = levels(factor(group)),
       pch = 19,
       col = factor(levels(factor(group))))
dev.off()
png("tsne_results.png", width = 10, height = 10, units = 'in', res = 300)
plot(proj_bind$Y[,1],proj_bind$Y[,2],pch=19,col=factor(group))
points(km$centers, pch = 60)
legend("topleft",
       legend = levels(factor(group)),
       pch = 19,
       col = factor(levels(factor(group))))
dev.off()


set.seed(52)
bind_full_carotenoids <- rbind(lutein,beta_caotin,lycopen)
bind_fulln_carotenoids<- normalize_input(bind_full_carotenoids)
proj_bind <- Rtsne(t(bind_fulln_carotenoids), perplexity = 4)
proj_bind_full <- cbind(proj_bind$Y,metabolites_$Date)
svglite("tsne_results_carotenoids.svg",width = 10, height = 10)
plot(proj_bind$Y[,1],proj_bind$Y[,2],pch=19,col=factor(group))
points(km$centers, pch = 60)
legend("topleft",
       legend = levels(factor(group)),
       pch = 19,
       col = factor(levels(factor(group))))
dev.off()

png("tsne_results_carotenoids.png", width = 10, height = 10, units = 'in', res = 300)
plot(proj_bind$Y[,1],proj_bind$Y[,2],pch=19,col=factor(group))
points(km$centers, pch = 60)
legend("topleft",
       legend = levels(factor(group)),
       pch = 19,
       col = factor(levels(factor(group))))
dev.off()

bind_full_phenolic <- rbind(coumaric,ferulic,caffeic,coumaryl)
bind_full_phenolic_tmp <- bind_full_phenolic[,!duplicated(t(bind_full_phenolic))] # remove duplicates
bind_fulln_phenolic <- normalize_input(bind_full_phenolic_tmp)
proj_bind <- Rtsne(t(bind_fulln_phenolic), perplexity = 4)
proj_bind_full <- cbind(proj_bind$Y,metabolites_$Date[!duplicated(t(bind_full_phenolic))])
svglite("tsne_results_phenolic.svg",width = 10, height = 10)
plot(proj_bind$Y[,1],proj_bind$Y[,2],pch=19,col=factor(group))
points(km$centers, pch = 60)
legend("topleft",
       legend = levels(factor(group)),
       pch = 19,
       col = factor(levels(factor(group))))
dev.off()

png("tsne_results_phenolic.png", width = 10, height = 10, units = 'in', res = 300)
plot(proj_bind$Y[,1],proj_bind$Y[,2],pch=19,col=factor(group))
points(km$centers, pch = 60)
legend("topleft",
       legend = levels(factor(group)),
       pch = 19,
       col = factor(levels(factor(group))))
dev.off()


bind_full_flavonoids <- rbind(naringenin,quercetin,phloretin)
bind_full_flavonoids_tmp <- bind_full_flavonoids[,!duplicated(t(bind_full_flavonoids))] # remove duplicates
bind_fulln_flavonoids <- normalize_input(bind_full_flavonoids_tmp)
proj_bind <- Rtsne(t(bind_fulln_flavonoids), perplexity = 4)
proj_bind_full <- cbind(proj_bind$Y,metabolites_$Date[!duplicated(t(bind_full_flavonoids))])
svglite("tsne_results_flavonoids.svg",width = 10, height = 10)
plot(proj_bind$Y[,1],proj_bind$Y[,2],pch=19,col=factor(group))
points(km$centers, pch = 60)
legend("topleft",
       legend = levels(factor(group)),
       pch = 19,
       col = factor(levels(factor(group))))
dev.off()

png("tsne_results_flavonoids.png", width = 10, height = 10, units = 'in', res = 300)
plot(proj_bind$Y[,1],proj_bind$Y[,2],pch=19,col=factor(group))
points(km$centers, pch = 60)
legend("topleft",
       legend = levels(factor(group)),
       pch = 19,
       col = factor(levels(factor(group))))
dev.off()

lmtsxx <- queue()

for (iter in 1:nsplit){
  idx <- lidxx[iter, ]
  transpiration <-  climadata$"transpiration (mg H2O/qm s))"[idx]
  photosynthesis <- climadata$"photosynthesis (µmol CO2/qm s)"[idx]
  humidity <- climadata$"relative humidity greenhouse (%)"[idx]
  co2 <- climadata$"CO2 concentration (ppm)"[idx]
  radiation <- climadata$"Global radiation ambient (W/qm)"[idx]
  radiation <- radiation*2.3 # convert from W/m2 to PPFD
  temperature <- climadata$"tempertaure greenhouse (°C)"[idx]

  ltranspxx$push(na_remove(transpiration))
  lphotoxx$push(na_remove(photosynthesis))
  lhumxx$push(na_remove(humidity))
  lco2xx$push(na_remove(co2))
  lradxx$push(na_remove(radiation))
  ltempxx$push(na_remove(temperature))


  lmtsxx$push(data.matrix(as.data.frame(ts.intersect(as.ts(photosynthesis), as.ts(transpiration), as.ts(humidity), as.ts(co2), as.ts(radiation), as.ts(temperature))))) # as matrix

}

llmtsxx <- lmtsxx$as_list()
llmtsxx[[7]] <- na_ma(llmtsxx[[7]])

llmtsxx_smooth <- queue()

for (iter in llmtsxx){
       llmtsxx_smooth$push(filterx(iter,120,10, ro=FALSE,zeroing = TRUE))
}

llmtsxx_smooth <- llmtsxx_smooth$as_list()



plan(multisession, workers = 8)
df_boot <- map(llmtsxx,tsbootstrapover_onemts,nb=500)
saveRDS(df_boot, "dataset.Rds",compress = FALSE)


feat1aslist <- queue()

meandf <- queue()

for (idx in 1:length(df_boot)){
  feataslist <- queue()
  for (channel in 1:6){
    feataspart <- colMeans(df_boot[[idx]][[channel]])
    feataslist$push(feataspart)
  }
  meandf$push(feataslist$as_list())
}

saveRDS(meandf$as_list(), "./data/processed/final_meandf.rds", compress = FALSE)



saveRDS(llmtsxx_smooth,"./data/processed/llmtsxx_smooth.rds",compress = FALSE)
saveRDS(llmtsxx,"./data/processed/llmtsxx.rds",compress = FALSE)