if (!exists('allBrandSets')){
  source('./4_distance.R')
}

if (exists('sales')){
  remove(sales)
}

competitors = list()

for (brandset in allBrandSets) {

  compName = brandset$comp_brandId[1]
  decoyName = brandset$decoy_brandId[1]
  focalName = brandset$focalName[1]

  name = paste(focalName, compName, decoyName, sep = '__')

  competitors[[name]] <- list()
  i <- 0
  for (brandName in allBrands) {
    if (brandName == compName || brandName == decoyName || brandName == focalName){
      next()
    }
    i <- i + 1
    tmp = brands[[brandName]]

    if (exists('sales')){
      sales <- sales + tmp$sales
    } else {
      sales <- tmp$sales
    }

    colnames(tmp) <- paste(i, colnames(tmp), sep = '_')

    if (length(competitors[[name]]) == 0) {
      competitors[[name]] <- tmp
    } else {
      competitors[[name]] <- cbind(competitors[[name]], tmp)
    }

  }

  competitors[[name]]$totalSales <- sales

  for (j in 1:13) {
    otherPrice <- competitors[[name]][[paste(j, 'price', sep = '_')]] * competitors[[name]][[paste(j, 'sales', sep = '_')]] / competitors[[name]]$totalSales
    otherPromo <- competitors[[name]][[paste(j, 'promo', sep = '_')]] * competitors[[name]][[paste(j, 'sales', sep = '_')]] / competitors[[name]]$totalSales
    otherAdv <- competitors[[name]][[paste(j, 'adv', sep = '_')]] * competitors[[name]][[paste(j, 'sales', sep = '_')]] / competitors[[name]]$totalSales
    otherDist <- competitors[[name]][[paste(j, 'dist', sep = '_')]] * competitors[[name]][[paste(j, 'sales', sep = '_')]] / competitors[[name]]$totalSales
    otherAwar <- competitors[[name]][[paste(j, 'awar', sep = '_')]] * competitors[[name]][[paste(j, 'sales', sep = '_')]] / competitors[[name]]$totalSales
    otherCons <- competitors[[name]][[paste(j, 'cons', sep = '_')]] * competitors[[name]][[paste(j, 'sales', sep = '_')]] / competitors[[name]]$totalSales
    otherLiking <- competitors[[name]][[paste(j, 'liking', sep = '_')]] * competitors[[name]][[paste(j, 'sales', sep = '_')]] / competitors[[name]]$totalSales
    otherBrand <- competitors[[name]][[paste(j, 'brand', sep = '_')]] * competitors[[name]][[paste(j, 'sales', sep = '_')]] / competitors[[name]]$totalSales
    otherQuality <- competitors[[name]][[paste(j, 'quality', sep = '_')]] * competitors[[name]][[paste(j, 'sales', sep = '_')]] / competitors[[name]]$totalSales
  }

  allBrandSets[[name]]$other_price <- otherPrice
  allBrandSets[[name]]$other_promo <- otherPromo
  allBrandSets[[name]]$other_adv <- otherAdv
  allBrandSets[[name]]$other_dist <- otherDist
  allBrandSets[[name]]$other_awar <- otherAwar
  allBrandSets[[name]]$other_cons <- otherCons
  allBrandSets[[name]]$other_liking <- otherLiking
  allBrandSets[[name]]$other_brand <- otherBrand
  allBrandSets[[name]]$other_quality <- otherQuality

  newColsToStandardize = c("other_price", "other_promo", "other_adv", "other_dist", "other_awar", "other_cons", "other_liking", "other_brand", "other_quality", "dominance")
  
  for (col in newColsToStandardize) {
    newColName <- paste('z', col, sep = '_')
    tmp <- allBrandSets[[name]]
    tmp[, newColName] <- 0

    for (row in 1:NROW(tmp[, col])) {
      tmp[row, newColName] <- (tmp[row, col] - mean(tmp[, col])) / 2 * sd(tmp[, col])
    }

    allBrandSets[[name]] <- tmp
  
  }

}