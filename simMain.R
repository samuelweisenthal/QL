source('utils.R')
set.seed(1)

n=500

x1 = rnorm(n = 3*n, 0, 1)
x2 = rnorm(n = 3*n, 0, 1)
x3 = rnorm(n = 3*n, 0, 1)


x4 = rnorm(n=3*n, 0, 1)

x = cbind(x1, x2, x3, x4)

beta0 = 0
beta1 = 2
beta2 = 2
beta3 = 2
beta4 = 2

mu = 1/(1+exp(-(beta0 + x %*% c(beta1, beta2, beta3, beta4))))

ns = rpois(n*3, lambda=10)
y = rbinom(n=ns, size=ns, prob=mu)

fit1 = glm(formula = cbind(y, (ns - y)) 
           ~ x[,1] + x[,2] + x[,3] + x[,4], family = binomial)

get.disp(fit1$fitted.values, ns, y, p=5)
res.plot(fit1)

fit12 = glm(formula = cbind(y, (ns - y)) 
            ~ x[,1] + x[,2] + x[,3] + x[,4], family = quasibinomial)

get.disp(fit12$fitted.values, ns, y, p=5)

fit13 = glm(formula = cbind(y, (ns - y)) 
            ~ x[,1] + x[,2] +x[,3], family = binomial)
res.plot(fit13)

get.disp(fit13$fitted.values, ns, y, p=4)

par(mfrow=c(2,2))

fit1 = glm(formula = cbind(y, (ns - y)) 
           ~ x[,1] + x[,2] + x[,3] + x[,4], family = quasibinomial)


phi1 = round(get.disp(fit1$fitted.values, ns, y, p=5),5)
res.plot(fit1)
title(main=paste('y/n ~ x1 + x2 + x3 + x4; phi:', phi1))

fit2 = glm(formula = cbind(y, (ns - y)) 
            ~ x[,1] + x[,2] + x[,3], family = quasibinomial)
res.plot(fit2)
phi2 = round(get.disp(fit2$fitted.values, ns, y, p=4),5)
title(main=paste('y/n ~ x1 + x2 + x3; phi:', phi2))

fit3 = glm(formula = cbind(y, (ns - y)) 
            ~ x[,1] + x[,2], family = quasibinomial)
res.plot(fit3)
phi3 = round(get.disp(fit3$fitted.values, ns, y, p=3),5)
title(main=paste('y/n ~ x1 + x2; phi:', phi3))

fit4 = glm(formula = cbind(y, (ns - y)) 
            ~ x[,1], family = quasibinomial)
res.plot(fit4)
phi4 = round(get.disp(fit4$fitted.values, ns, y, p=2),5)
title(main=paste('y/n ~ x1; phi:', phi4))

fit5 = glm(formula = cbind(y, (ns - y)) 
           ~ 1, family = quasibinomial)
res.plot(fit5)
phi5 = round(get.disp(fit4$fitted.values, ns, y, p=2),5)
title(main=paste('y/n ~ x1; phi:', phi5))


# x2 = c(rep(1,n), rep(2,n), rep(3,n))
# x2 = interaction(x2, x[,4])
# fit2 = glm(formula = cbind(y, (ns - y)) ~ x2, family=binomial)
# fit2 = glm(formula = cbind(y, (ns - y)) ~ x2, family=quasibinomial())
