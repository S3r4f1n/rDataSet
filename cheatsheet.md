## New package


```r
create_package("path/to/name")
use_gpl3_license()
```


## Dependencies
**Import** packages that your package requires to work. R will install them when it installs your package. Add with
```r
use_package("dplyr", type = "imports")
use_package("purrr", type = "imports")
```
**Suggest** packages that developers of your package need. Users can install or not, as they like. Add with
```r
use_package("testthat", type = "suggests")
```

## During Developement
```r
load_all() # Load code
test() # Run tests
document() # Rebuild docs and NAMESPACE
check() # Check complete package
```

## Documentation
Use R-Oxygen or usually written as roxygen comments to build the docs of your lib. Add @export for functions which should be exposed when to users.

## Installation

```r
# Clone repo and then install from source
install.packages("path-to-folder", repo = NULL, type = "source")
```
