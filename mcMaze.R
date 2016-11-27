
addBlock <- function(x, y, z, b, conn) {
   writeLines(paste0("world.setBlock(",round(x),",",round(y),",",round(z),",",round(b),")"), conn)
}

addBlocks <- function(x1, y1, z1, x2, y2, z2,
                      type, conn) {
   cmd <- paste0("world.setBlocks(", x1, ",", y1, ",", z1, ",",
                 x2, ",", y2, ",", z2, ",", type, ")")
   writeLines(cmd, conn)
}

mcMaze <- function(n=5, baseline=70,
                   width=3,
                           # corridor width
                   height=4,
                           # corridor height
                   host="localhost"
                   ) {
   library(Rmaze)
   library(igraph)
   AIR <- 0
   ROCK <- 1
   SOIL <- 2
   WOOD <- 5
   GLASS <- 20
   LAPIZ <- 22
   GOLD <- 41
   GLOWSTONE <- 89
   ##
   buffer <- 10L
                           # how wide buffer around the maze
   W <- n*(width + 1) + 1
                           # width of the complete maze
   conn <- socketConnection(host=host, port = 4711,
                            blocking=TRUE, server=FALSE, open="r+")
   x1 <- z1 <- -buffer
   x2 <- z2 <- W + buffer
   addBlocks(x1,baseline,z1,  x2,baseline+height+20,z2, AIR, conn)
                           # on top of it a lot of air
   addBlocks(x1,baseline-1,z1, x2,baseline-1,z2, SOIL, conn)
                           # one layer of soil
   addBlocks(1,baseline+height,1, W,baseline+height,W, GLOWSTONE, conn)
                           # rock roof
   g <- makeGraph(n, n)
   maze <- makeMaze_dfs(g)
   ## create border of the maze
   addBlocks(1,baseline, 1,  W, baseline + height - 1, 1,  ROCK, conn)
   addBlocks(1,baseline, W,  W, baseline + height - 1, W,  ROCK, conn)
   addBlocks(1,baseline, 1,  1, baseline + height - 1, W,  ROCK, conn)
   addBlocks(W,baseline, 1,  W, baseline + height - 1, W,  ROCK, conn)
   ## exits
   addBlocks(1,baseline,1, 1+width,baseline+height-1,1,  0, conn)
   addBlocks(W,baseline,W, W-width,baseline+height-1,W,  0, conn)
   addBlocks(1,baseline,1, 1,baseline+height,1, WOOD, conn)
   addBlocks(W,baseline,W, W,baseline+height,W, GOLD, conn)
   for(ie in seq(length=ecount(maze))) {
      edge <- E(maze)[ie]
      if(edge$wall == "ON") {
         n1 <- head_of(maze, edge) %>% names 
         n2 <- tail_of(maze, edge) %>% names
         i1 <- n1 %>% sub(".*_([[:digit:]]+)_.*", "\\1", .) %>% as.integer
         j1 <- n1 %>% sub(".*_([[:digit:]]+)$", "\\1", .) %>% as.integer
         i2 <- n2 %>% sub(".*_([[:digit:]]+)_.*", "\\1", .) %>% as.integer
         j2 <- n2 %>% sub(".*_([[:digit:]]+)$", "\\1", .) %>% as.integer
         if(i1 != i2) {
                           # horizontal wall b/w vertically placed cells
            x1 <- 1 + (1+width)*(j1 - 1)
            z1 <- 1 + (1 + width)*(max(i1, i2) - 1)
            x2 <- 1 + (1 + width)*j1
            z2 <- 1 + (1 + width)*(max(i1, i2) - 1)
            addBlocks(x1, baseline, z1,
                      x2, baseline + height - 1, z2,
                      1, conn)
         }
         else {
                           # vertical wall b/w horizontal cells
            x1 <- 1 + (1 + width)*(max(j1, j2) - 1)
            z1 <- 1 + (1 + width)*(i1 - 1)
            x2 <- 1 + (1 + width)*(max(j1, j2) - 1)
            z2 <- 1 + (1 + width)*i1
            addBlocks(x1, baseline, z1,
                      x2, baseline + height - 1, z2,
                      1, conn)
         }
      }
   }
   close(conn)
   maze
}
