if (!exists('allBrands')){
  source('./cleaning.R')
}

i <- 0
k <- 0
brandsets <- list()

prepareDecoyBrand <- function(brand, price, quality, both){
  brand$priceDecoy <- price
  brand$qualityDecoy <- quality
  brand$priceQualityDecoy <- both
  return(brand)
}

prepareCompBrand <- function(brand, type){
  brand$relativeQuadrant <- type
  return(brand)
}

for (brand in allBrands) {
  i <- i + 1
  focal = brands[[brand]]
  focal$brandId <- brand

  brandsets[[brand]] <- list(name = brand, decoys = list(), comps = list())

  j <- 0
  for (brand1 in allBrands) {
    j <- j + 1
    comparedBrand = brands[[brand1]]
    comparedBrand$brandId <- brand1

    if (comparedBrand == focal) {
      next()
    }

    if ( comparedBrand$price > focal$price && comparedBrand$quality < focal$quality ) {
      k <- k + 1
      colnames(comparedBrand) <- paste('decoy', colnames(comparedBrand), sep = '_')
      comparedBrand <- prepareDecoyBrand(comparedBrand, 0, 0, 1)

      # brandset <- cbind(focal, comparedBrand)
      brandsets[[brand]]$decoys[[brand1]] <- comparedBrand
      # assign(paste(brand, "decoy", brand1, sep = '_'), brandset)
      next()

    } else if ( comparedBrand$price == focal$price && comparedBrand$quality < focal$quality ) {
      k <- k + 1
      colnames(comparedBrand) <- paste('decoy', colnames(comparedBrand), sep = '_')
      comparedBrand <- prepareDecoyBrand(comparedBrand, 0, 1, 0)

      # brandset <- cbind(focal, comparedBrand)
      brandsets[[brand]]$decoys[[brand1]] <- comparedBrand
      # assign(paste(brand, "decoy", brand1, sep = '_'), brandset)
      next()

    } else if ( comparedBrand$price < focal$price && comparedBrand$quality == focal$quality ) {
      k <- k + 1
      colnames(comparedBrand) <- paste('decoy', colnames(comparedBrand), sep = '_')
      comparedBrand <- prepareDecoyBrand(comparedBrand, 1, 0, 0)

      # brandset <- cbind(focal, comparedBrand)
      brandsets[[brand]]$decoys[[brand1]] <- comparedBrand
      # assign(paste(brand, "decoy", brand1, sep = '_'), brandset)
      next()

    }

    if ( comparedBrand$price < focal$price && comparedBrand$quality < focal$quality ) {
      k <- k + 1

      colnames(comparedBrand) <- paste('comp', colnames(comparedBrand), sep = '_')
      comparedBrand$relativeQuadrant <- 'LP-LQ'

      # BOTH IN QUADRANT A (LP-LQ)
      if (focal$quadrant == 'LP-LQ'  && comparedBrand$comp_quadrant == 'LP-LQ') {
        comparedBrand$pair <- 2

      # BOTH IN QUADRANT B (HP-HQ)
      } else if (focal$quadrant == 'HP-HQ' && comparedBrand$comp_quadrant == 'HP-HQ') {
        comparedBrand$pair <- 4

      # FOCAL IN QUADRANT A and COMPETITOR IN QUADRANT B
      } else if (focal$quadrant == 'LP-LQ' && comparedBrand$comp_quadrant == 'HP-HQ') {
        comparedBrand$pair <- 6

      # FOCAL IN QUADRANT B and COMPETITOR IN QUADRANT A
      } else if (focal$quadrant == 'HP-HQ' && comparedBrand$comp_quadrant == 'LP-LQ') {
        comparedBrand$pair <- 8

      } else {
        comparedBrand$pair <- 0
        
      }

      # brandset <- cbind(focal, comparedBrand)
      brandsets[[brand]]$comps[[brand1]] <- comparedBrand
      # assign(paste(brand, "comp", brand1, sep = '_'), brandset)

    } else if ( comparedBrand$price > focal$price && comparedBrand$quality > focal$quality ) {
      k <- k + 1
      colnames(comparedBrand) <- paste('comp', colnames(comparedBrand), sep = '_')
      comparedBrand$relativeQuadrant <- 'HP-HQ'

      # BOTH IN QUADRANT A (LP-LQ)
      if (focal$quadrant == 'LP-LQ'  && comparedBrand$comp_quadrant == 'LP-LQ') {
        comparedBrand$pair <- 1

      # BOTH IN QUADRANT B (HP-HQ)
      } else if (focal$quadrant == 'HP-HQ' && comparedBrand$comp_quadrant == 'HP-HQ') {
        comparedBrand$pair <- 3

      # FOCAL IN QUADRANT A and COMPETITOR IN QUADRANT B
      } else if (focal$quadrant == 'LP-LQ' && comparedBrand$comp_quadrant == 'HP-HQ') {
        comparedBrand$pair <- 5

      # FOCAL IN QUADRANT B and COMPETITOR IN QUADRANT A
      } else if (focal$quadrant == 'HP-HQ' && comparedBrand$comp_quadrant == 'LP-LQ') {
        comparedBrand$pair <- 7

      } else {
        comparedBrand$pair <- 0
        
      }
      
      # brandset <- cbind(focal, comparedBrand)
      brandsets[[brand]]$comps[[brand1]] <- comparedBrand
      # assign(paste(brand, "comp", brand1, sep = '_'), brandset)

    }
  }
}
