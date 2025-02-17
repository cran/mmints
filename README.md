
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmints

<!-- badges: start -->
<!-- badges: end -->

The ‘mmints’ (Mighty Metrika Internals) package aims to automate tasks
commonly used in ‘shiny’ applications. It primarily consists of internal
functions and modules frequently utilized in other Mighty Metrika
‘shiny’ applications.

## Installation

You can install the released version of ‘mmints’ from
[CRAN](https://CRAN.R-project.org):

``` r
install.packages("mmints")
```

You can install the development version of ‘mmints’ from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mightymetrika/mmints")
```

## Example

One feature of ‘mmints’ is the csvUploadModule, which simplifies the
process of: 1. Uploading a CSV data file 2. Displaying the variables and
data types as a ‘DT’ data table 3. Providing functionality for editing
variable types

Here’s how to use the csvUploadModule:

``` r
library(mmints)

csvUpload_app <- function(){
  ui <- shiny::fluidPage(
    shiny::titlePanel("CSV Upload Module"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        csvUploadUI("data_upload")$input
        ),
      shiny::mainPanel(
        csvUploadUI("data_upload")$output
        )
      )
    )
  server <- function(input, output, session) {
    data <- csvUploadServer("data_upload")
    }
  shiny::shinyApp(ui, server)
}


if(interactive()){
  csvUpload_app()
}
```

## License

This project is licensed under the MIT License.

## Contact

For any queries or suggestions, please open an issue on the GitHub
repository.
