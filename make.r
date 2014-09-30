require(rmarkdown)
render(file.path(getwd(), "analyzeOpenPayments.Rmd"), output_format="html_document")
file.rename(file.path(getwd(), "analyzeOpenPayments.md"), file.path(getwd(), "README.md"))
