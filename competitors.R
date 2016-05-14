if (!exists('allBrandSets')){
  source('./distance.R')
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

  allBrandSets[[name]]$otherPrice <- otherPrice
  allBrandSets[[name]]$otherPromo <- otherPromo
  allBrandSets[[name]]$otherAdv <- otherAdv
  allBrandSets[[name]]$otherDist <- otherDist
  allBrandSets[[name]]$otherAwar <- otherAwar
  allBrandSets[[name]]$otherCons <- otherCons
  allBrandSets[[name]]$otherLiking <- otherLiking
  allBrandSets[[name]]$otherBrand <- otherBrand
  allBrandSets[[name]]$otherQuality <- otherQuality

}