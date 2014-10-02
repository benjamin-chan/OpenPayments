require(rmarkdown)
render(file.path(getwd(), "OpenPayments.Rmd"), output_format="html_document")
file.rename(file.path(getwd(), "OpenPayments.md"), file.path(getwd(), "README.md"))
