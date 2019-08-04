###############################################################
# This file is to show variance proportions for 'gait' data.
#
# Author: Leshun Xu
# Last update: 2019-07-22
###############################################################
K <- 13; n <- 4
t = seq(0.025,0.975,0.05)
ybasis = create.bspline.basis(c(0,1), K, n)

harmLfd = vec2Lfd(c(0, 0), c(0, 1))
YdatafdPar = fdPar(ybasis, harmLfd, 0)

###############################################################
# To do FPCA for both variables (start here):
Ydatafd = smooth.basis(t, gait, YdatafdPar)
# plot(Ydatafd)
# Ydatafd[1]$fd$coefs

Ydatapca = pca.fd(Ydatafd$fd,nharm=4)
round(Ydatapca$varprop,4)
# [1] 0.4362 0.2021 0.1171 0.0834

# To do FPCA for both variables (end here):
###############################################################


###############################################################
# To do FPCA for the first variable (start here):
Y1 <- gait[,,1]
Y1fd = smooth.basis(t, Y1, YdatafdPar)
plot(Y1fd, xlab='Time points', ylab='')
plot(Y1fd$fd$coefs, xlab='Time points', ylab='')
Y1pca = pca.fd(Y1fd$fd,nharm=4)

round(Y1pca$varprop,4)
# [1] 0.7032 0.1193 0.0829 0.0379

harmfd = Y1pca$harmonics
harmvals = eval.fd(t,harmfd)

plot(t, harmvals[,1], xlab='Time points', ylab='', type='l')
plot(t, harmvals[,2], xlab='Time points', ylab='', type='l')
plot(t, harmvals[,3], xlab='Time points', ylab='', type='l')
plot(t, harmvals[,4], xlab='Time points', ylab='', type='l')

# To do FPCA for the first variable (end here):
###############################################################

###############################################################
# To do FPCA for the 2nd variable (start here):
Y2 <- gait[,,2]
Y2fd = smooth.basis(t, Y2, YdatafdPar)
plot(Y2fd, xlab='Time points', ylab='')
Y2pca = pca.fd(Y2fd$fd,nharm=4)

round(Y2pca$varprop,4)
# [1] 0.4112 0.2395 0.1487 0.0821

harmfd = Y2pca$harmonics
harmvals = eval.fd(t,harmfd)

plot(t, harmvals[,1], xlab='Time points', ylab='', type='l')
plot(t, harmvals[,2], xlab='Time points', ylab='', type='l')
plot(t, harmvals[,3], xlab='Time points', ylab='', type='l')
plot(t, harmvals[,4], xlab='Time points', ylab='', type='l')

# To do FPCA for the 2nd variable (end here):
###############################################################


###############################################################
# To calculate the errors (start here):
# For Y1 part
fittedY1 <- eval.fd(t, Y1fd$fd)
e1 <- sum(abs(fittedY1 - Y1))
to <- sum(abs(Y1))
fittedY2 <- eval.fd(t, Y2fd$fd)
e2 <- sum(abs(fittedY2 - Y2))
to <- to + sum(abs(Y2))
(e1+e2)/to
# [1] 0.01493801

