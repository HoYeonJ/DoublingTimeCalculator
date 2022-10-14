# NEXT STEP: Make setting the parameters for the model its own method entirely

# Import the data--note: it can have any number of columns but "Time" needs to 
# be in the first and the growth of each well comes is in the columns after
data = read.csv(file = 'exampleFile3.csv')
data[is.na(data)]=0

# Metadata inputs: "window" is the window size for calculating the brute force
# R^2 and slope values
window = 100

col = ncol(data)
row = nrow(data)

# Time series of the data
times = data[,(1:1)]

# Growth of every well
cellGrowth = data[,(2:col)]
newcol = col-1

# Takes a time series and growth data to find the growth rate (b from y = a*b^x) 
# and the r^2 for a linear regression of the log transformed growth data
rSquaredSlopeHeuristic = function(tlist, glist){
  leng = length(glist)
  lent = length(tlist)
  
  # Validation
  if (leng!=lent)
    print("Incorrect input to heuristic")
  
  # Running the exponential regression
  model = lm(log(glist)~ tlist)
  
  return (coefficients(model)[2], summary(model)$r.squared/100)
}

# # Find the increments instead of the raw magnitude (to calculate the steepest
# # part of the curve)
# # REMOVED because slope of r^2 line is better metric
# cellGrowthLog[(2:row),(1:newcol)] = cellGrowthLog[(2:row),(1:newcol)]-
#   cellGrowthLog[(1:row-1),(1:newcol)]
# cellGrowthLog[(1),(1:newcol)]=0

# Take increments of 30 and find the slope of the secant line
# newcol = col-1 
# cellGrowthThirty = as.data.frame(matrix(0, row, newcol))
# for (y in (1:newcol)){
#   for (x in (1:row-granularity)){
#     cellGrowthThirty[x,y] = (cellGrowth[x+granularity,y+1]-cellGrowth[x,y+1])/
#       granularity
# }
# }

# # Find the max steepest slope of each column
# maxes = sapply(cellGrowthThirty[,1:newcol], which.max)
# print(maxes)

# Take the window around the max to find every possible continuous
# growth rate and r^2. Heuristic is r^2 + slope standardized to maximum slope
rmat = matrix(0, row-window, newcol)
smat = matrix(0, row-window, newcol)
hmat = matrix(0, row-window, newcol)
maxslope = -10

for (y in (1:newcol)){
  for (x in (1:(row-window))){
    # print(y)
    # Center point of regression
    center= x+(window/2)
    # Absorbance
    windows = cellGrowth[(center-window/2: center+window/2), y]
    # Time
    t = times[(center-window/2: center+window/2)]
    # Linear regression after log transformation
    model = lm((windows)~ t)
    slope = coefficients(model)[1]
    r = summary(model)$r.squared
    if (slope>maxslope)
      maxslope = slope
    rmat[x,y]=r
    smat[x,y]=slope
  }
}
rmat[is.nan(rmat)]=0
smat[is.nan(rmat)]=0
hmat = smat/maxslope + rmat

# Finding the time point with greatest slope and best r^2 value for each well
# NOTE: instead of each column representing 1 well, 
# each well is reprsented as a row in this matrix
retmat = matrix(0, 3, newcol)
print(max(rmat[,4]))
for (y in (1:newcol)){
  maxind = which.max(rmat[,y])
  # Rerun the regression because it was not saved
  windows = cellGrowth[(maxind-window/2: maxind+window/2), y]
  t = times[(maxind-window/2: maxind+window/2)]
  model = lm((windows)~ t)
  slope = coefficients(model)[1]
  r = summary(model)$r.squared
  retmat[,y]= c(log(2)/slope, times[maxind], r)
}

# Result formatted as: Doubling time, center of steepest slope, R^2 value
print(retmat)

