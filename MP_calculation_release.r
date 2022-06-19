################################
# By Xiu Wang on June 19, 2022 #
################################
###### Functioning codes #######
################################

get.optH <- function(e, Sig){
    n <- length(e)
    one <- rep(1, n)
    Psi <- t(cbind(e, one)) %*% solve(Sig) %*% cbind(e, one)
    return (solve(Sig) %*% cbind(e, one) %*% solve(Psi))
}

get.optE <- function(e, Sig, rf){
    g <- get.optH(e, Sig)[, 2]
    h <- get.optH(e, Sig)[, 1]
    C <- t(g) %*% Sig %*% g
    B <- t(h) %*% Sig %*% g
    A <- t(h) %*% Sig %*% h
    return (-(C + rf * B) / (B + rf * A))
}

get.MP <- function(e, Sig, rf, frontier = TRUE, line = TRUE, new = TRUE, ...){
    best.e <- get.optE(e, Sig, rf)
    opt.H <- get.optH(e, Sig)
    best.w <- opt.H %*% c(best.e, 1)
    best.sd <- sqrt(t(best.w) %*% Sig %*% best.w)

    if (frontier) {
        Es <- c(); sds <- c()
        for (E in seq(-abs(best.e), 2 * abs(best.e), length.out = 200)){
            w <- opt.H %*% c(E, 1)
            Es <- c(Es, E)
            sds <- c(sds, sqrt(t(w) %*% Sig %*% w))
        }
        
        if (new)
            plot(sds, Es, type = "l", xlim = c(0, 1.5 * best.sd), ylim = c(-abs(best.e), 2 * abs(best.e)), xlab = "Standard Deviation", ylab = "Expected Return", ...)
        else 
            lines(sds, Es,...)
        if (line) {
        abline(a = rf, b = (best.e - rf) / best.sd)
        points(0, rf)
        points(best.sd, best.e)
        }
    }

    res <- list()
    res$opt.H <- opt.H
    res$best.w <- best.w; res$best.e <- best.e; res$best.sd <- best.sd
    res$rf <- rf
    if (best.e < rf)
        print("Warning: Optimal expected return tends to infinity!")
    return(res)
}

get.optP <- function(MP, A, ...){
    mus <- c()
    sds <- seq(0, 1.5 * MP$best.e, length.out = 200)
    r.weight <- (MP$best.e - MP$rf) / (A * MP$best.sd^2)
    mu <- MP$rf + r.weight * c(MP$best.e - MP$rf)
    sd <- r.weight * MP$best.sd
    for (isd in sds){
        mus <- c(mus, mu - A/2 * sd^2 + A/2 * isd^2)
    }
    lines(sds, mus, ...)
    points(sd, mu, ...)
    res <- list()
    res$r.weight <- r.weight
    res$weight <- rbind(as.numeric(r.weight) * MP$best.w, Risk.free = 1 - r.weight)
    res$mu <- mu; res$sd <- sd
    return(res)
}

##### About get.MP():
# Receives the mean vector e, covariance matrix Sig, and risk-free rate rf
# Returns an object recording the market portfolio
# Set frontier = TRUE (default) to draw the efficient frontier
# Further set line = TRUE (default) to draw the CML
# Set new = FALSE to draw on the existing plot
# You can also pass other plotting options

##### About get.optP():
# Receives the object returned by get.MP() and a user-sepcified parameter A
# Returns the optimal portfolio
# Tangent indifferent curves will also be drawn

##### Demo: 
# Call below to draw the frontier and CML
# mkt.p <- get.MP(my.return, my.cov, 0.02, col = "blue", lty = 3)
# Then call below to draw the investor's optimal portfolio
# inv.p <- get.optP(mkt.p, A = 4, col = "red") 

# Warnings on sequence length may be raised. You can ignore them
