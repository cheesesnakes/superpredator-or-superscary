#sumary effect E = sum(w_i * E_i) / sum(w_i)

summary_effect <- function(w_i, E_i) {

    return(sum(w_i * E_i) / sum(w_i))
}

# variance of the summary effect var E = 1 / sum(w_i)

variance_summary_effect <- function(w_i) {

    return(1 / sum(w_i))
}

# weighted effect w_i = 1 / var E_i

# Correction for the random effects model Var E_i* = V_i + T

T <- function(Q, df, C) {

    return((Q - df) / C)
}

# Q = sum(w_i * E_i^2) - E^2 / sum(w_i)

Q <- function(w_i, E_i) {

    return(sum(w_i * E_i^2) - ((sum(w_i * E_i)^2)/ sum(w_i)))
}

# df = i - 1

df <- function(i) {

    return(i - 1)
}

# C = sum(w_i) - sum(w_i^2) / sum(w_i)

C <- function(w_i) {

    return(sum(w_i) - (sum(w_i^2) / sum(w_i)))
}

# Z = E / sqrt(var E)

Z <- function(E, var_E) {

    return(E / sqrt(var_E))
}

# hertrterogeneity I^2 = Q - df / Q

I_sq <- function(Q, df) {

    return((Q - df )*100/ Q)
}

# R_sq = 1 - (Q - df) / sum w_i

R_sq <- function(Q, df, w_i) {

    return(1 - ((Q - df) / sum(w_i)))
}

# p-value = 2 * pnorm(-abs(Z))

p <- function(Z) {

    return(2 * pnorm(-abs(Z)))
}

