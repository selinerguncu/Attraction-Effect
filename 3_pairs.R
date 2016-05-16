if (!exists('brandsets')){
  source('./2_brandset.R')
}
allBrandSets <- list()
i <- 0
for (focal in brandsets) {
  for (comp in focal$comps){
    for (decoy in focal$decoys){
      i <- i + 1
      # nr <- str_pad(i, width = 4, side = 'left', pad = '0')
      # name <- (paste(nr, focal$name, comp$comp_brandId[1], decoy$decoy_brandId[1], sep = '_'))
      brandset <- cbind(brands[[focal$name]], comp, decoy)
      if (decoy$decoy_price >= comp$comp_price && decoy$decoy_quality <= comp$comp_quality) {
        brandset$compDecoy <- 1
      } else {
        brandset$compDecoy <- 0
      }
      brandset$focalName <- focal$name
      brandsetName <- paste(focal$name, comp$comp_brandId[1], decoy$decoy_brandId[1], sep = '__')
      allBrandSets[[brandsetName]] <- brandset
    }
  }
}