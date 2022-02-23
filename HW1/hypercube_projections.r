# Plot projections of hypercube {0,1}^4 on a plane consisting 3 points: x0 = c(0,0,0,0),
# y0 = c(1.5,0,0,-0.5), z0 = c(-1.5, 1.5, 0, 0)

# --------------------------------------------------------------
# Functions 

getPlaneCoords = function(x, y, z, A) {
    a = y - x
    b = z - x
    systemMatrix = matrix(nrow=2, c(a%*%a, a%*%b, a%*%b, b%*%b))
    systemVector = matrix(c(a%*%A - a%*%x, b%*%A - b%*%x))
    planeCoordinates = solve(systemMatrix, systemVector)

    k = as.numeric(as.numeric(a%*%b)/as.numeric(b%*%b))  # Orthonormal
    a = a - k * b
    planeCoordinates[2] = planeCoordinates[2] + k * planeCoordinates[1]
    
    planeCoordinates[1] = planeCoordinates[1] * sqrt(as.numeric(a %*% a)) #Normalizing
    planeCoordinates[2] = planeCoordinates[2] * sqrt(as.numeric(b %*% b))
    return(planeCoordinates)
}
#Function from the first part of HW
getCubeCoords = function() {
  n=4 # Dimension of cube
  G=matrix(nrow=2^n, ncol=n)
  G[1,n]=1
  G[2,n]=-1
  p=2
  for(i in 2:n){ 
    t=p
    p=p*2
    for(k in (p/2+1):p) {  # отзеркаливание
      for(x in 0:(n-1)){   # кода
        G[k,n-x]=G[t,n-x]}   #
      
      G[t,n+1-i]=1   # добавление 1 впереди кода
      G[k,n+1-i]=-1  # добавление -1 впереди кода
      t=t-1
    }
  }
  G = G*0.5 + 0.5
}

getCubeEdges = function(CubeCoords) {
    N = length(CubeCoords[,1])
    edges = matrix(ncol=2*length(CubeCoords[1,]),0)
    for(i in 1:(N-1)) {
        vec1 = CubeCoords[i,]
        for(j in (i+1):N) {
            vec2 = CubeCoords[j,]
            vec0 = vec2 - vec1
            if(as.numeric(vec0 %*% vec0) == 1.0) {
                edges <- rbind(edges, c(vec1, vec2))
            }
        }
    }
    edges <- edges[-1,]
    return(edges)
}

getProjections = function(edges, x, y, z) {
    projections = matrix(ncol=4,0)
    for(i in 1:length(edges[,2])) {
        projections <- rbind(projections, c(getPlaneCoords(x,y,z,edges[i,1:4]),
                                            getPlaneCoords(x,y,z,edges[i,5:8])))
    }
    projections <- projections[-1,]
    return(projections)
}

plotProjectionHypercube = function(x, y, z)
{
    cube = getCubeCoords();
    edges = getCubeEdges(cube);
    projections = getProjections(edges, x, y, z);
    X = c(projections[,1],projections[,3]);
    Y = c(projections[,2],projections[,4]);
    plot(X, Y, main = "projection", xlab = "x", ylab="y", pch=20)
    for(i in 1:length(projections[,2])) {
        lines(c(projections[i,1],projections[i,3]),c(projections[i,2],projections[i,4]))
    }
}

# ------------------------------------------------------------
# Plotting projections

x0 = c(0,0,0,0)
y0 = c(1.5,0,0,-0.5)
z0 = c(-1.5, 1.5, 0, 0)

setwd("D:\\вуз\\R\\programs\\HW1")
pdf("projection.pdf")

plotProjectionHypercube(x0, y0, z0)

#---------------------------------------------------------------
# Plotting projections for 10 another points

for(i in 1:10) {
    planeVectors = runif(4 * 3, min = -3, max = +3)
    plotProjectionHypercube(planeVectors[1:4],planeVectors[5:8],planeVectors[9:12])
}

dev.off()
