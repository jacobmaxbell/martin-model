# Demand parameters (same for both firms, symmetric case)
a <- 100   # base demand (max demand when price = 0)
b <- 2     # sensitivity of a firm's own demand to its own price
c <- 0.5   # sensitivity of a firm's demand to the RIVAL's price (substitutability)
mc <- 10   # marginal cost of production

# Demand:  q1 = a - b*p1 + c*p2 ;  q2 = a - b*p2 + c*p1
# Profit_i = (p_i - mc) * q_i

# Best response: maximize profit_i w.r.t. p_i, holding rival's price fixed.
# Solving the first-order condition gives:
#   p_i* = (a + b*mc + c*p_j) / (2*b)
best_response <- function(p_rival, a, b, c, mc) {
  (a + b * mc + c * p_rival) / (2 * b)
}

n_rounds <- 15
p1 <- numeric(n_rounds); p2 <- numeric(n_rounds)
p1[1] <- 50   # arbitrary starting price for firm 1
p2[1] <- 20   # arbitrary starting price for firm 2

for (i in 2:n_rounds) {
  p1[i] <- best_response(p2[i - 1], a, b, c, mc)
  p2[i] <- best_response(p1[i - 1], a, b, c, mc)
}

data.frame(round = 1:n_rounds, price_firm1 = p1, price_firm2 = p2)

# Analytical Nash equilibrium (by symmetry, p1* = p2* = p*)
p_star <- (a + b * mc) / (2 * b - c)
q_star <- a - b * p_star + c * p_star
profit_star <- (p_star - mc) * q_star


# ---- Cournot duopoly (quantity competition) ----

# Inverse demand: P(Q) = a - b*(q1 + q2)
a  <- 100  # choke price (price at which quantity demanded = 0)
b  <- 1    # slope of inverse demand
mc <- 10   # marginal cost, same for both firms

# Best response: maximize profit_i = (a - b*(q_i+q_j) - mc)*q_i w.r.t. q_i
# FOC gives:  q_i* = (a - mc - b*q_j) / (2*b)
best_response <- function(q_rival, a, b, mc) {
  br <- (a - mc - b * q_rival) / (2 * b)
  pmax(br, 0)  # quantity can't go negative
}

n_rounds <- 30
q1 <- numeric(n_rounds); q2 <- numeric(n_rounds)
q1[1] <- 60   # firm 1 starts producing a lot
q2[1] <- 5    # firm 2 starts producing very little

for (i in 2:n_rounds) {
  q1[i] <- best_response(q2[i - 1], a, b, mc)
  q2[i] <- best_response(q1[i - 1], a, b, mc)
}

data.frame(round = 1:n_rounds, qty_firm1 = q1, qty_firm2 = q2)

# Analytical Nash equilibrium (by symmetry, q1* = q2* = q*)
q_star <- (a - mc) / (3 * b)
Q_star <- 2 * q_star
p_star <- a - b * Q_star
profit_star <- (p_star - mc) * q_star

