# ==============================================================================
# Mockable bindings (testthat::local_mocked_bindings)
# ==============================================================================
# local_mocked_bindings() kann nur Bindings ersetzen, die im Package-Namespace
# (oder dessen Imports) existieren. Fuer Funktionen aus *base* legen wir daher
# NULL-Bindings an: R's Funktions-Lookup ueberspringt Nicht-Funktionen, d.h.
# im Normalbetrieb wird weiterhin base::* gefunden — testthat kann das Binding
# aber im Test temporaer durch einen Mock ersetzen.
# Siehe ?testthat::local_mocked_bindings, Abschnitt "Base functions".

requireNamespace <- NULL
scan             <- NULL
file.show        <- NULL
