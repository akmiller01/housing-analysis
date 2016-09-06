# library("devtools")
# devtools::install_github("ankitrohatgi/digitizeR")

library("digitizeR")
app <- wpd.launch()
wpd.close(app)