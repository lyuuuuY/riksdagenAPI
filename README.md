# riksdagenAPI

<!-- badges: start -->
  [![R-CMD-check](https://github.com/lyuuuuY/riksdagenAPI/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lyuuuuY/riksdagenAPI/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
  
  **riksdagenAPI** is an R package that provides functions to interact with the Swedish Parliament's API, allowing users to fetch and analyze data from political speeches.
  
## Installation

You can install the development version of riksdagenAPI from GitHub with:

```r
# If you haven't installed devtools yet, uncomment the next line
# install.packages("devtools")
# install.packages("riksdagenAPI")
devtools::install_github("lyuuuuY/riksdagenAPI")
```
## Example

Here is a simple example of how to use the package to retrieve the top 10 nouns from political speeches:
```r
library(riksdagenAPI)
# Fetch the top 10 nouns from selected speeches
result <- get_top_10_nouns(start_date = "2015-01-01", end_date = "2024-01-01", type_speech = "", party = "", member = "0729710260118", size = 10000)
# Print the results
print(result)
```

## Acknowledgments
Thanks to the Swedish Parliament for providing the API and the data used in this package.