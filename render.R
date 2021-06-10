input_rmd <- "articlelist.Rmd"
output_html <- "articlelist.html"

rmarkdown::render(input=input_rmd,output_file=output_html,output_format="html_document")
browseURL(output_html)