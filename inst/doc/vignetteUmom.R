### R code from vignette source 'vignetteUmom.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: vignetteUmom.Rnw:45-55
###################################################
library(Umoments)
n <- 10
# draw a sample
smp <- rgamma(n, shape = 3)
# calculate biased estimates up to 6th order
m <- numeric(6)
m[1] <- mean(smp)
for (j in 2:6) {
  m[j] <- mean((smp - m[1])^j)
}


###################################################
### code chunk number 2: vignetteUmom.Rnw:61-62
###################################################
uM4(m[2], m[4], n)


###################################################
### code chunk number 3: vignetteUmom.Rnw:65-67
###################################################
uM2M3(m[2], m[3], m[5], n)
uM2(m[2], n)*uM3(m[3], n)


###################################################
### code chunk number 4: vignetteUmom.Rnw:70-71
###################################################
uM3pow2(m[2], m[3], m[4], m[6], n)


###################################################
### code chunk number 5: vignetteUmom.Rnw:81-94
###################################################
nx <- 10
ny <- 8
shp <- 3
smpx <- rgamma(nx, shape = shp) - shp
smpy <- rgamma(ny, shape = shp)
mx <- mean(smpx)
my <- mean(smpy)
m  <- numeric(6)
for (j in 2:6) {
  m[j] <- mean(c((smpx - mx)^j, (smpy - my)^j))
}
uM2pool(m[2], nx, ny)
uM2pow3pool(m[2], m[3], m[4], m[6], nx, ny)


###################################################
### code chunk number 6: vignetteUmom.Rnw:101-109
###################################################
# simulate a sample
nsmp <- 23
smp <- rgamma(nsmp, shape = 3)
# two categories for pooled estimates
treatment <- sample(0:1, size = nsmp, replace = TRUE)
# estimates
uM(smp, 5)
uMpool(smp, treatment, 6)


###################################################
### code chunk number 7: vignetteUmom.Rnw:127-128
###################################################
one_combination(c(5, 0, 2, 1), "n_x")


###################################################
### code chunk number 8: vignetteUmom.Rnw:130-132
###################################################
#cat(strwrap(one_combination(c(5, 0, 2, 1), "n_x")), sep = "\n")
writeLines(strwrap(one_combination(c(5, 0, 2, 1), "n_x")))


