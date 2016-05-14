if (!exists('competitors')){
  source('./competitors.R')
}

mergedBrandsets <- list()
i <- 0
for (brandset in allBrandSets) {
  i <- i + 1
  brandset$brandsetId <- i
  brandset$time <- 1:130
  mergedBrandsets <- rbind(brandset, mergedBrandsets)
}

