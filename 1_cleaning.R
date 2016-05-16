# read files from csv folder
files <- list.files(path = './csv/')
# read quality scores
quality <- read.csv('./quality_detergent.csv', header=TRUE, stringsAsFactors=FALSE)


# columns we need to keep
keeps <- c("awar", "cons", "liking", "adv", "price", "promo", "dist", "sales", "brand")

brands <- list()
allBrands <- character()
allPrices <- numeric()

minPrice <- 1e6
maxPrice <- 0
minQuality <- min(quality[1,])
maxQuality <- max(quality[1,])
averageQuality <- (minQuality + maxQuality) / 2
medianQuality <- median(t(quality)[,1])

# iterate over csv files
for (file in files) {
  # create the filepath to import
  filepath = paste("./csv/", file, sep='')

  # import individual csv
  tmp = read.csv(filepath, header=TRUE)

  # filter unwanted columns
  tmp = tmp[keeps]

  # strip .csv extension and to have a nice name
  name = strsplit(file, '.csv')[[1]]

  # find the quality for brand and add it as a column
  qlt  = quality[, name]
  tmp$quality <- qlt
  # create reverse price variable
  tmp$reversePrice <- 1/tmp$price

  # find min price
  tmpMinPrice = min(tmp[, "price"])
  if (minPrice > tmpMinPrice && tmpMinPrice > 1e-04) {
    minPrice = tmpMinPrice
  }

  # find max price
  tmpMaxPrice = max(tmp[, "price"])
  if (maxPrice < tmpMaxPrice) {
    maxPrice = tmpMaxPrice
  }

  # append brand data to allbrands
  allBrands[name] <- name

  allPrices <- c(allPrices, tmp$price)

  # store brand in brands list
  brands[[name]] <- tmp
}

averagePrice <- ( minPrice + maxPrice ) / 2
medianPrice <- median(allPrices)

for (brand in allBrands) {
  tmp = brands[[brand]]
  if (tmp$price < medianPrice && tmp$quality < medianQuality) {
    tmp$quadrant <- "LP-LQ"
  } else if (tmp$price < medianPrice && tmp$quality > medianQuality) {
    tmp$quadrant <- "LP-HQ"
  } else if (tmp$price > medianPrice && tmp$quality < medianQuality) {
    tmp$quadrant <- "HP-LQ"
  } else {
    tmp$quadrant <- "HP-HQ"
  }

  tmp$normPrice <- (tmp$price - minPrice) / (maxPrice - minPrice)
  tmp$normQuality <- (tmp$quality - minQuality) / (maxQuality - minQuality)
  tmp$pqScore <- tmp$normPrice + tmp$normQuality
  
  for (col in keeps) {
    newColName <- paste('z', col, sep = '_')
    tmp[, newColName] <- 0

    for (row in 1:NROW(tmp[, col])) {
      tmp[row, newColName] <- (tmp[row, col] - mean(tmp[, col])) / 2 * sd(tmp[, col])
    }
  
  }
  
  lagged <- tmp
  real <- tmp
  
  lagged <- lagged[-1, ]
  colnames(lagged) <- paste('lag', colnames(lagged), sep = '_')
  
  real <- real[-130, ]

  tmp <- cbind(lagged, real)
  tmp$time <- 1:NROW(tmp[, 1])

  brands[[brand]] <- tmp
}

