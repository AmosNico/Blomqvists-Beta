library(plot3D)

M <- mesh(seq(0, 1,length.out=10), seq(0, 1,length.out=10))
u <- M$x
v <- M$y


plot_copula <- function(z,title,file_name) {
  png(filename = file_name,height = 700, width = 700)
  persp3D(u, v, z,colvar = NULL,facets = NA,border="black",theta =330,phi=30,
          xlab = "u", ylab = "v", zlab = "z", main=title,
          cex.lab=2.5, cex.axis=2.5, cex.main=2.5) # enlargement of text
  dev.off()
}

# Pi(u,v)
plot_copula(u * v,"Î (u,v)","3.1 product copula.png")

# M(u,v)
plot_copula(pmin(u,v),"M(u,v)","3.2 max copula.png")

# W(u,v)
plot_copula(pmax(matrix(0,10,10),u+v-1),"W(u,v)","3.3 min copula.png")

# Clayton copula
theta <- -0.5
z <- pmax(u^(-theta)+v^(-theta)-1,matrix(0,10,10))^(-1/theta)
plot_copula(z,expression(bold("C"[-0.5]^"cl")),"3.4 Clayton copula.png")
