get_pen_mat <- function(knots, r = 2) { # d = Anzahl Spalten = Anzahl Basis-Funktionen; r = welcher Grad Fehler soll noch berücksichtigt werden?
  tp <- knots$tp
  l <- knots$l
  d <- length(knots$knots) - l - 1
  if (tp) { # l muss > 0 und m muss >= 0 sein, evtl. noch if Block, der das checkt, einbauen
    m <- d - l
    K <- diag(c(rep(0, 1), rep(1, m)))  # Im Kneib Buch steht m-2, ich glaube, es hängt von der Theorie ab, ob man m-2 oder m nimmt, das müssten wir dann im Paper begründen
  }
  else {
    D <- diag(d)
    for (i in 1:r) {
      D <- D[-1, ] - D[-nrow(D), ]
    }
    K <- t(D) %*% D
  }
return (K)
}
