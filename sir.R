library(deSolve)
library(ggplot2)

sir <- function(t, y, parms) {
    with(as.list(c(y, parms)), {
        N <- S + I + R
        
        # Events
        birth <- r * (1 - N / c) * N
        death <- mu * N
        dim <- nu * I
        infection <- beta * S * I / N
        recovery <- gamma * I
        
        # Derivatives
        dS <- + birth - death - infection
        dI <- - death - dim + infection - recovery
        dR <- - death + recovery
        
        list(c(dS, dI, dR))
    })
}

params <- list(
    N <- 100,
    r <- 1 / (365 * 25),
    c <- 100,
    mu <- 1 / (365 * 80),
    nu <- 1 / 30,
    beta <- 0.2,
    gamma <- 0.1
)

y0 <- with(params, c(S = N - 1, I = 1, R = 0))

t <- seq(0, 200, 0.1)

out <- lsode(y0, t, sir, parms = params)

# TODO: use ggplot2 instead, and make this look nice
plot(out)
