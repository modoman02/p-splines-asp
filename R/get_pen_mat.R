get_pen_mat <- function(d, r, tp=FALSE) { # d = Anzahl Spalten = Anzahl Basis-Funktionen; r = welcher Grad Fehler soll noch berücksichtigt werden?
  if (tp) { # l muss > 0 und m muss >= 0 sein, evtl. noch if Block, der das checkt, einbauen
    m <- d - l + 1
    K <- diag(c(rep(0, l - 1), rep(1, m)))  # Im Kneib Buch steht m-2, ich glaube, es hängt von der Theorie ab, ob man m-2 oder m nimmt, das müssten wir dann im Paper begründen
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
