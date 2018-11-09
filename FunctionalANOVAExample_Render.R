

## Render the RMarkdown file to both HTML and PDF
rmarkdown::render(input = "FunctionalANOVAExample.Rmd", output_format = c("html_document","pdf_document"))
rm(list = ls())

