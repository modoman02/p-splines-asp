# für Berechnung des AIC braucht man die Smoother Matrix S

# lambda grid sollte als Argument frei wählbar sein, damit Genauigkeit/Rechenleistung trade-off abgewägt werden kann
for (i in 1:10) {
  if (i == 4) break
}
print(i)  # gibt 4 aus


# GD matrix wird aktuell in update_parameters() und in calc_mu/sigma() initialisiert. Am besten wäre, es nur in calc_mu/calc_sigma zu
# initialisieren und in jeder update_parameter() eine GD matrix mit dem GD vektor aus den Iterationen upzudaten
