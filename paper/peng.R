##require(rang)

##graph <- resolve("./ptproc_1.5-1.tar.gz", "2004-07-01")
##dockerize(graph, "~/dev/misc/ptproc", cache = TRUE)

require(ptproc)

set.seed(1000)
x <- cbind(runif(100), runif(100), runif(100))
hPois.cond.int <- function(params, eval.pts, pts = NULL, data = NULL, TT = NULL) {
    mu <- params[1]
    if(is.null(TT))
        rep(mu, nrow(eval.pts))
    else {
        vol <- prod(apply(TT, 2, diff))
        mu * vol
    }
}
ppm <- ptproc(pts = x, cond.int = hPois.cond.int, params = 50, ranges = cbind(c(0,1), c(0,1), c(0,1)))
fit <- ptproc.fit(ppm, optim.control = list(trace = 2), method = "BFGS")
summary(fit)
