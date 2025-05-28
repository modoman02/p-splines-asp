get_pen_mat <- function(d, r, tp=FALSE) { # d = Anzahl Spalten = Anzahl Basis-Funktionen; r = welcher Grad Fehler soll noch berücksichtigt werden?
  if (tp) { # l muss > 0 und m muss >= 0 sein, evtl. noch if Block, der das checkt, einbauen
    m <- d - l + 1
    K <- diag(c(rep(0, l - 1), rep(1, m)))  # Im Kneib Buch steht m-2, ich glaube es hängt von der Theorie ab, ob man m-2 oder m nimmt, das müssten wir dann im Paper begründen
  }
  else {
    D1 <- diag(d)
    D1 <- D1[-1, ] - D1[-nrow(D1), ]  # erste Differenzmatrix mit Dimension d-1 x d
    K <- D1
    for (i in 2:r) {
      K <- K[-1, ] - K[nrow(K), ]
    }
  }
return (K)
}
