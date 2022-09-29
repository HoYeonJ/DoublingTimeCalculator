# Import the data--note: it can have any number of columns but "Time" needs to 
# be in the first and the growth of each well comes is in the columns after
cellGrowth = read.csv(file = 'exampleFile.csv')
col = ncol(cellGrowth)
row = nrow(cellGrowth)
times = cellGrowth[,(1:1)]

# Log transform the growth columns
#cellGrowthLog = log(cellGrowth[,(2:col)])
cellGrowthLog = cellGrowth[,(2:col)]

# Find the increments instead of the raw magnitude (to calculate the steepest
# part of the curve)
newcol = col-1
cellGrowthLog[(2:row),(1:newcol)] = cellGrowthLog[(2:row),(1:newcol)]-
  cellGrowthLog[(1:row-1),(1:newcol)]
cellGrowthLog[(1),(1:newcol)]=0

# Take increments of 30 and find the slope of the secant line
newcol = col-1 
cellGrowthThirty = as.data.frame(matrix(0, row, newcol))
for (y in (1:newcol)){
  for (x in (1:row-30)){
    cellGrowthThirty[x,y] = (cellGrowth[x+30,y+1]-cellGrowth[x,y+1])/30
}
}

# Find the max of each column
maxes = sapply(cellGrowthThirty[,1:newcol], which.max)
print(maxes)
# Take the 30 point window around the max
mat = as.data.frame(matrix(0, 3, newcol))
arr = vector(mode = "double", length = newcol)
timesarr = vector(mode = "double", length = newcol)
for (y in (1:newcol)){
    center= maxes[y]
    windows = cellGrowth[(center-50: center+50), y+1]
    x = cellGrowth[(center-50: center+50),1]
    model = lm(log(windows)~ x)
    print(model$r.squared)
    timesarr[y] = times[center]
    #mat[,y ]= c(log(2)/coefficients(model)[2], times[center], model$r.squared)
}
print(mat)

# Run linear regressions for each array

# Find the doubling time

