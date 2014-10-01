require(rmarkdown)
render(file.path(getwd(), "mapOpenPayments.Rmd"), output_format="html_document")
file.rename(file.path(getwd(), "mapOpenPayments.md"), file.path(getwd(), "README.md"))
