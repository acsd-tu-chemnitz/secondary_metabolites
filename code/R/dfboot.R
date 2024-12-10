source("./code/R/libload.R")
source("./code/R/util_functions.R")

llmtsxx <- readRDS(file = "./data/processed/llmtsxx.rds")

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


#meandf <- readRDS("./data/processed/final_meandf.rds")





