if (!exists('competitors')){
  source('./5_competitors.R')
}

mergedBrandsets <- list()
i <- 0
for (brandset in allBrandSets) {
  i <- i + 1
  brandset$brandsetId <- i
  brandset$levelId <- paste(brandset$pair, i, brandset$time, sep = '_')

  mergedBrandsets <- rbind(brandset, mergedBrandsets)
}