library(ggplot2)
library(dplyr)
library(data.table)
library(raster)

#read data in as data.table
DT <- fread('tile_placements.csv', col.names = c('ts','hash','x','y','colorval'))[, 3:5]

#colors as described by reddit, index - 1 corresponds to colorval
colors <- c('#FFFFFF', '#E4E4E4', '#888888', '#222222', 
            '#FFA7D1', '#E50000', '#E59500', '#A06A42', 
            '#E5D900', '#94E044', '#02BE01', '#00E5F0',
            '#0083C7', '#0000EA', '#E04AFF', '#820080')

#matrix of the colors represented in RGB
scalarRGB <- function() {
    hex <- substr(colors,2,7)
    r <- paste0('0x',substr(hex, 1, 2)) %>% as.numeric
    g <- paste0('0x',substr(hex, 3, 4)) %>% as.numeric
    b <- paste0('0x',substr(hex, 5, 6)) %>% as.numeric
    
    return(cbind(r, g, b))
}

rgb <- scalarRGB()

#counts the number of times each colorval shows for each (x, y)
grid <- DT[, .N, by = names(DT)]

#throw away values where x or y are above 999 (which were created by a bug in the r/place API)
grid <- filter(grid, x < 1000, y < 1000)

#convert from long to wide and set NA values to 0
grid <- dcast(grid, x + y ~ colorval, value.var = 'N')
grid[is.na(grid)] <- 0


#create a grid of 1000 x 1000 cells, to add pixels where there were no colors added
coords <- expand.grid(seq(0, 999), seq(0, 999))
colnames(coords) <- c('x', 'y')

#merge the grid with the original data.table and set NA values to 0
grid <- merge(grid, coords, all.y = T)
grid[is.na(grid)] <- 0

#convert values into relative frequencies
grid <- cbind(grid[, 1:2], grid[, 3:18]/rowSums(grid[, 3:18]))

#if there were no colors added, make there be 1 value of #FFFFFF
# this sets the pixel's average color to white
grid[rowSums(grid[,3:18]) == 0,][,3] <- 1

#convert data.table to matrix
m <- as.matrix(grid[, 3:18])

#compute the root mean square of each pixel's RGB value
avgColors <- sqrt(m %*% (rgb^2)) %>% floor

#create data.table (x, y, r, g, b)
output <- cbind(grid[, 1:2], avgColors) %>% as.data.table

#reorder so that it's in ascending x,y values
output <- output[with(output, order(x, y)),]

#create raster object from matrix and flip it so that (0, 0) is the bottom left corner
rasterObj <- rasterFromXYZ(output) %>% flip(direction = 'y')

#plot raster object
plotRGB(rasterObj)
