library("jpeg")
library("png")
library("abind")

offset <- function(image,x=0,y=0)
{
  x.min <- max(1, 1+x)
  x.max <- min(ncol(image), ncol(image)+x)
  y.min <- max(1, 1+y)
  y.max <- min(nrow(image), nrow(image)+y)
  
  x2.min <- max(1, 1-x)
  x2.max <- min(ncol(image), ncol(image)-x)
  y2.min <- max(1, 1-y)
  y2.max <- min(nrow(image), nrow(image)-y)
  
  new.image <- image
  new.image[y.min:y.max, x.min:x.max] <- new.image[y2.min:y2.max, x2.min:x2.max]
  
  return(new.image) 
}

offsets.x <- c(-1,-1,-1,0,0,1,1,1)
offsets.y <- c(-1,0,1,1,-1,-1,0,1)

offsets.x <- c(-1)
offsets.y <- c(-1)

offsets.x <- round(rnorm(25,0,1))
offsets.y <- round(rnorm(25,0,1))

offsets <- cbind(offsets.x,offsets.y)
offsets <- as.list(as.data.frame(t(offsets)))

image <- readJPEG("adventure_time.jpg")
image.bw <- (0.2126*image[,,1] + 0.7152*image[,,2] + 0.0722*image[,,3])

plot(0,1, type="n", xlim=c(0,1), ylim=c(0,1))
rasterImage(image.bw, 0, 0, 1, 1)

image.list <- lapply(offsets, FUN=function(x){offset(image.bw, x[1], x[2])})
image.list <- lapply(image.list, FUN=function(x){1-sqrt((x-image.bw)^2)})
image.matrix <- do.call(abind,c(image.list,list(along=3)))
n <- 1
m <- 1
image.new <- apply(image.matrix, c(1,2), FUN=function(x){minn(x^n)^m})
rasterImage(image.new,0,0,1,1)

test.r <- image[,,1]+image.new
test.g <- image[,,2]+image.new
test.b <- image[,,3]+image.new
test.r <- test.r-min(test.r)
test.g <- test.g-min(test.g)
test.b <- test.b-min(test.b)
test.r <- test.r/max(test.r)
test.g <- test.g/max(test.g)
test.b <- test.b/max(test.b)
test <- matrix(rgb(test.r,test.g,test.b),nrow=nrow(test.r),ncol=ncol(test.r))
dim(test)
rasterImage(test,0,0,1,1)
6
