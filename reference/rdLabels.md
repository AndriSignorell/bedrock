# Extract Variable Labels from Rd Documentation

Reads the variable descriptions out of the `\describe` section of a
documented dataset and returns them as a named character vector, the
names being the variable names. This turns documentation that already
exists into labels usable in tables, plots and codebooks, instead of
maintaining the same descriptions a second time in the code.

## Usage

``` r
rdLabels(dataName, package)
```

## Arguments

- dataName:

  character string, the name of the dataset.

- package:

  character string, the name of the package holding the dataset.

## Value

a named character vector of variable descriptions, the names being the
variable names.

## Details

The Rd database is read with
[`tools::Rd_db()`](https://rdrr.io/r/tools/Rdutils.html) and searched
recursively for the first `\describe` section, from which all
`\item{var}{description}` entries are taken. Only that first section is
read: on a page documenting more than one dataset, the labels of the
first one are returned.

Descriptions are returned as written in the Rd file, with whitespace and
line breaks collapsed to single spaces. Rd markup inside a description,
such as `\code{}` or `\eqn{}`, contributes its content without the
surrounding command.

The package must be installed, as the documentation is read from the
installed Rd database rather than from the sources.

## See also

[`tools::Rd_db()`](https://rdrr.io/r/tools/Rdutils.html)

Other pkg.funinfo: [`funArgs()`](funArgs.md),
[`funCalls()`](funCalls.md), [`funKeywords()`](funKeywords.md),
[`funList()`](funList.md), [`rdTitle()`](rdTitle.md)

## Examples

``` r
# the labels of a documented dataset, taken from the \describe
# section of its help page
rdLabels("Pizza", "bedrock")
#>                                                                                        index 
#>                                    "integer, index of the record, complete by construction." 
#>                                                                                         date 
#>                                                                      "date of the delivery." 
#>                                                                                         week 
#>                                                             "numeric, the week of the year." 
#>                                                                                      weekday 
#>                                                              "numeric, the day of the week." 
#>                                                                                         area 
#>                                    "factor with the levels Brent , Camden and Westminster ." 
#>                                                                                        count 
#>                                                   "integer, the number of pizzas delivered." 
#>                                                                                       rebate 
#>                                                       "logical, TRUE if a rebate was given." 
#>                                                                                        price 
#>                                          "numeric, the total price of the pizzas delivered." 
#>                                                                                     operator 
#>                                   "factor with three levels, the operator taking the order." 
#>                                                                                       driver 
#>                                 "factor with seven levels, the driver delivering the order." 
#>                                                                                 delivery_min 
#>                                                     "numeric, the delivery time in minutes." 
#>                                                                                  temperature 
#>                                   "numeric, the temperature in degrees Celsius on delivery." 
#>                                                                                 wine_ordered 
#>                                                  "integer, 1 if wine was ordered, 0 if not." 
#>                                                                               wine_delivered 
#>                                                "integer, 1 if wine was delivered, 0 if not." 
#>                                                                                   wrongpizza 
#>                                              "logical, TRUE if a wrong pizza was delivered." 
#>                                                                                      quality 
#> "ordered factor with the levels low < medium < high , the quality of the pizza on delivery." 
#>                                                                                   vegetarian 
#>                                          "integer, 1 if the order was vegetarian, 0 if not." 
#>                                                                                          nps 
#>                  "numeric, the Net Promoter Score from 1 to 10, an ordinal customer rating." 
#>                                                                                    complaint 
#>                                             "integer, 1 if a complaint was filed, 0 if not." 
#>                                                                                        style 
#>                 "character, the type of pizza, e.g. italian , american , gourmet or vegan ." 
#>                                                                                      channel 
#>                                         "character, the order channel, app , web or phone ." 
#>                                                                                          tip 
#>                                                        "numeric, the tip in monetary units." 
```
