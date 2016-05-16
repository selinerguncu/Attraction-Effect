if (!exists('brandsets')){
  source('./3_pairs.R')
}

for (brandset in allBrandSets) {
  priceDifference <- brandset$price - brandset$decoy_price
  qualityDifference <- brandset$quality - brandset$decoy_quality
  brandset$dominance <- sqrt(priceDifference*priceDifference + qualityDifference*qualityDifference)
  if (max(brandset$dominance)/3 > brandset$dominance) {
    brandset$cld <- 'small'
  } else if (max(brandset$dominance)/3 < brandset$dominance && 2*max(brandset$dominance)/3 > brandset$dominance){
    brandset$cld <- 'medium'
  } else {
    brandset$cld <- 'large'
  }
  focalName <- brandset$focalName[1]
  brandsetName <- paste(focalName, brandset$comp_brandId[1], brandset$decoy_brandId[1], sep = '__')
  allBrandSets[[brandsetName]] <- brandset
}