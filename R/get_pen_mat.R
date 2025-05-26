get_pen_mat <- function(d, r) { # d = Anzahl Spalten = Anzahl Basis-Funktionen; r = welcher Grad Fehler soll noch berücksichtigt werden?
  D1 <- diag(d)
  D1 <- D1[-1, ] - D1[-nrow(D1), ]  # erste Differenzmatrix mit Dimension d-1 x d
  K <- D1
  for (i in 2:r) {
    K <- K[-1, ] - K[nrow(K), ]
  }
return (K)
}
