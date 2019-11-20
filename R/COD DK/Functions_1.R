
Age.labels <- c('Total', '0', '1-4', '5-9', '10-14','15-19','20-24','25-29',
  '30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69',
  '70-74','75-79','80-84')

Category.labels <- c(
  'Smoking related cancer',
  'Non-Smoking related cancer',
  'Cardiovascular',
  'Respiratory-Infectious',
  'Respiratory-Non-infectious',
  'External',
  'Other')


e0frommxc <- function(mxcvec,sex){
  dim(mxcvec) <- c(111,length(mxcvec)/111)
  mx          <- rowSums(mxcvec)
  LifeExpectancy(mx,sex)
}

sd.frommx <- function(mx,sex='f'){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  age           <- 0:(i.openage-1)
  vx <- sum(dx*(age+ax-ex[1L])^2)
  sd <- sqrt(vx)
  return(sd)
}

sdfrommxc <- function(mxcvec = c(m1),sex){
  dim(mxcvec) <- c(111,length(mxcvec)/111)
  mx          <- rowSums(mxcvec)
  sd.frommx(mx,sex)
}


ed.frommx <- function(mx,sex='f'){
  i.openage <- l <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  v <- (sum(dx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])
  
  return(v)
}

edfrommxc <- function(mxcvec = c(m1),sex){
  dim(mxcvec) <- c(111,length(mxcvec)/111)
  mx          <- rowSums(mxcvec)
  ed.frommx(mx,sex)
}

COD.decomp <- function(.SD){
  sex <- .SD$Sex[1]
  
  
}


AKm02a0        <- function(m0, sex = "m"){
  sex <- rep(sex, length(m0))
  ifelse(sex == "m", 
         ifelse(m0 < .0230, {0.14929 - 1.99545 * m0},
                ifelse(m0 < 0.08307, {0.02832 + 3.26201 * m0},.29915)),
         # f
         ifelse(m0 < 0.01724, {0.14903 - 2.05527 * m0},
                ifelse(m0 < 0.06891, {0.04667 + 3.88089 * m0}, 0.31411))
  )
}

LifeExpectancy <- compiler::cmpfun(function(mx,sex = "f"){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[1]
})

LifeTable      <- function(mx,sex = "f"){
  mx <- as.matrix(mx)
  i.openage <- nrow(mx)
  ax        <- mx * 0 + .5
  ax[1, ]   <- AKm02a0(m0 = mx[1, ], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)        
  qx[i.openage, ]       <- ifelse(is.na(qx[i.openage, ]), NA, 1)
  ax[i.openage, ]       <- 1 / mx[i.openage, ]                   
  px 				      <- 1 - qx 																				
  px[is.nan(px)]  <- 0 
  lx 			        <- apply(px, 2, function(px., RADIX, OPENAGE){ 		
    if (all(is.na(px.))) {
      px.
    } else {
      c(RADIX, RADIX * cumprod(px.[1:OPENAGE]))
    }
  }, RADIX = 1, OPENAGE = i.openage - 1
  )
  rownames(lx)    <- 0:(i.openage - 1) 
  dx 				      <- lx * qx 																				
  Lx 				      <- lx - (1 - ax) * dx 														
  Lx[i.openage, ]	<- lx[i.openage, ] * ax[i.openage, ]
  Tx 				      <- apply(Lx, 2, function(Lx., i.openage, OPENAGE){
    c(rev(cumsum(rev(Lx.[1:OPENAGE]))),0) + Lx.[i.openage]	
  }, OPENAGE = i.openage - 1, i.openage = i.openage
  )
  rownames(Tx)    <- rownames(lx)
  ex 				      <- Tx / lx 	                              
  list(e0=ex[1,],ex=ex,lx=lx,mx=mx)
}





# Decomposition function based on Horiuchi etal 2008, 
# better to check my last program with van Raalte 2016

Decomp <-function (func, rates1, rates2, N, ...) {
  y1 <- func(rates1, ...)
  y2 <- func(rates2, ...)
  d <- rates2 - rates1
  n <- length(rates1)
  delta <- d/N
  x <- rates1 + d * matrix(rep(0.5:(N - 0.5)/N, length(rates1)), 
                           byrow = TRUE, ncol = N)
  cc <- matrix(0, nrow = n, ncol = N)
  for (j in 1:N) {
    for (i in 1:n) {
      z <- rep(0, n)
      z[i] <- delta[i]/2
      cc[i, j] <- func((x[, j] + z), ...) - func((x[, j] - 
                                                    z), ...)
    }
  }
  return(rowSums(cc))
}


my_reshape.function <- function(i=names(x)[1],DecompIn=x){
  
  Z        <- DecompIn[[i]]
  Z.names  <- names(Z)
  ZZ        <- lapply(Z.names, function(ii,Z,i){
    Z2      <- Z[[as.character(ii)]]
    XX      <- cbind(Country=as.integer(i),year=as.integer(ii),age=0:110,Z2)
    XX
  }, Z = Z,i=i)
  # now stick it together
  D        <- as.data.frame(do.call(rbind, ZZ))
  D        <- data.table(D)
  #DT       <- as.data.table(melt(D, 
  #                               id.vars = list("state","year","age"),
  #                               variable.name = "Cause"))
  D
}

