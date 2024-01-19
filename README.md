# shiny-cromwell
Shiny app for interacting with the Fred Hutch instances of Cromwell.

This app can also be run on your local machine, should you notice memory limitations in the deployed version or are at another institution.  

## Usage

Clone the repo down to your machine, then cd into the directory, then in R run:

```r
if (!requireNamespace("pkgload", quietly = TRUE)) {
	install.packages("pkgload")
}
pkgload::load_all()
```

Then run:

```r
cromwellApp()
```

And the app will be opened in your default browser. 
