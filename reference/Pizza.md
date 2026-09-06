# Pizza Delivery Data (Extended)

An extended artificial dataset inspired by a similar dataset pizza.sav
in *Arbeitsbuch zur deskriptiven und induktiven Statistik* by Toutenburg
et al. The data describe a pizza delivery service in London serving
three areas, each record being one order and its associated
characteristics.

## Usage

``` r
Pizza
```

## Format

A data frame with 1209 observations on 22 variables:

- index:

  integer, index of the record, complete by construction.

- date:

  date of the delivery.

- week:

  numeric, the week of the year.

- weekday:

  numeric, the day of the week.

- area:

  factor with the levels `Brent`, `Camden` and `Westminster`.

- count:

  integer, the number of pizzas delivered.

- rebate:

  logical, `TRUE` if a rebate was given.

- price:

  numeric, the total price of the pizzas delivered.

- operator:

  factor with three levels, the operator taking the order.

- driver:

  factor with seven levels, the driver delivering the order.

- delivery_min:

  numeric, the delivery time in minutes.

- temperature:

  numeric, the temperature in degrees Celsius on delivery.

- wine_ordered:

  integer, `1` if wine was ordered, `0` if not.

- wine_delivered:

  integer, `1` if wine was delivered, `0` if not.

- wrongpizza:

  logical, `TRUE` if a wrong pizza was delivered.

- quality:

  ordered factor with the levels `low < medium < high`, the quality of
  the pizza on delivery.

- vegetarian:

  integer, `1` if the order was vegetarian, `0` if not.

- nps:

  numeric, the Net Promoter Score from 1 to 10, an ordinal customer
  rating.

- complaint:

  integer, `1` if a complaint was filed, `0` if not.

- style:

  character, the type of pizza, e.g. `italian`, `american`, `gourmet` or
  `vegan`.

- channel:

  character, the order channel, `app`, `web` or `phone`.

- tip:

  numeric, the tip in monetary units.

## Source

Simulated data.

## Details

Compared to the original dataset, this extended version includes
additional behavioural and outcome variables such as customer
satisfaction, Net Promoter Score (NPS), complaints, dietary choices and
tipping behaviour. These variables are generated using probabilistic
models to resemble realistic business data, including noise, imperfect
relationships and heterogeneous customer behaviour.

The dataset is designed to be realistically complex. It contains the
data types commonly met in practice: numerics, integers, factors,
ordered factors, logicals, characters and dates. Missing values occur
both systematically and at random, in every variable except `index`.

The variable `nps` is a simulated Net Promoter Score from 1 to 10,
calibrated to resemble realistic customer feedback distributions,
including asymmetric lower-tail behaviour.

The variable `complaint` is generated using a probabilistic model
depending on delivery time, order correctness and additional noise,
ensuring that complaints are not deterministically linked to single
factors.

The variable `tip` is based on a percentage of the order price and is
influenced by customer satisfaction (`nps`), delivery performance and
driver-specific effects. Tips are zero for complaints or very low
satisfaction, and otherwise increase monotonically with customer
satisfaction while retaining stochastic variation.

Overall, the dataset is designed to provide a realistic benchmark for
statistical modelling, including classification (binary and ordinal),
regression and performance evaluation, e.g. ROC curves and AUC with
confidence intervals.

Every variable carries a `label` attribute with its description, so that
the labels can be used in tables and plots without repeating them in the
code.

## References

Toutenburg H, Schomaker M, Wissmann M, Heumann C (2009): *Arbeitsbuch
zur deskriptiven und induktiven Statistik* Springer, Berlin Heidelberg.

## See also

Other datasets: [`Cards`](cards.md), [`Roulette`](roulette.md),
[`Tarot`](tarot.md), [`courseData()`](courseData.md)

## Examples

``` r
str(bedrock::Pizza)
#> 'data.frame':    1209 obs. of  22 variables:
#>  $ index         : int  1 2 3 4 5 6 7 8 9 10 ...
#>   ..- attr(*, "label")= Named chr "Numeric index of the record."
#>   .. ..- attr(*, "names")= chr "index"
#>  $ date          : Date, format: "2014-03-01" "2014-03-01" ...
#>  $ week          : num  9 9 9 9 9 9 9 9 9 9 ...
#>   ..- attr(*, "label")= Named chr "Week number."
#>   .. ..- attr(*, "names")= chr "week"
#>  $ weekday       : num  6 6 6 6 6 6 6 6 6 6 ...
#>   ..- attr(*, "label")= Named chr "Weekday (integer)."
#>   .. ..- attr(*, "names")= chr "weekday"
#>  $ area          : Factor w/ 3 levels "Brent","Camden",..: 2 3 3 1 1 2 2 1 3 1 ...
#>   ..- attr(*, "label")= Named chr "Factor with levels Brent, Camden, Westminster."
#>   .. ..- attr(*, "names")= chr "area"
#>  $ count         : int  5 2 3 2 5 1 4 NA 3 6 ...
#>   ..- attr(*, "label")= Named chr "Number of pizzas delivered."
#>   .. ..- attr(*, "names")= chr "count"
#>  $ rebate        : logi  TRUE FALSE FALSE FALSE TRUE FALSE ...
#>   ..- attr(*, "label")= Named chr "Logical, TRUE if a rebate was given."
#>   .. ..- attr(*, "names")= chr "rabate"
#>  $ price         : num  65.7 27 41 26 57.6 ...
#>   ..- attr(*, "label")= Named chr "Total price of delivered pizzas."
#>   .. ..- attr(*, "names")= chr "price"
#>  $ operator      : Factor w/ 3 levels "Allanah","Maria",..: 3 3 1 1 3 1 3 1 1 3 ...
#>   ..- attr(*, "label")= Named chr "Factor indicating the operator."
#>   .. ..- attr(*, "names")= chr "operator"
#>  $ driver        : Factor w/ 7 levels "Butcher","Carpenter",..: 7 1 1 7 3 7 7 7 7 3 ...
#>   ..- attr(*, "label")= Named chr "Factor indicating the driver."
#>   .. ..- attr(*, "names")= chr "driver"
#>  $ delivery_min  : num  20 19.6 17.8 37.3 21.8 48.7 49.3 25.6 26.4 24.3 ...
#>   ..- attr(*, "label")= Named chr "Delivery time in minutes."
#>   .. ..- attr(*, "names")= chr "delivery_min"
#>  $ temperature   : num  53 56.4 36.5 NA 50 27 33.9 54.8 48 54.4 ...
#>   ..- attr(*, "label")= Named chr "Temperature in degrees Celsius when delivered."
#>   .. ..- attr(*, "names")= chr "temperature"
#>  $ wine_ordered  : int  0 0 0 0 0 0 1 NA 0 1 ...
#>   ..- attr(*, "label")= Named chr "Integer, 1 if wine was ordered, 0 if not"
#>   .. ..- attr(*, "names")= chr "wine_ordered"
#>  $ wine_delivered: int  0 0 0 0 0 0 1 NA 0 1 ...
#>   ..- attr(*, "label")= Named chr "Integer, 1 if wine was delivered, 0 if not"
#>   .. ..- attr(*, "names")= chr "wine_delivered"
#>  $ wrongpizza    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
#>   ..- attr(*, "label")= Named chr "Logical, TRUE if a wrong pizza was delivered"
#>   .. ..- attr(*, "names")= chr "wrongpizza"
#>  $ quality       : Ord.factor w/ 3 levels "low"<"medium"<..: 2 3 NA NA 2 1 1 3 3 2 ...
#>   ..- attr(*, "label")= Named chr "Ordered factor with levels low < medium < high"
#>   .. ..- attr(*, "names")= chr "quality"
#>  $ vegetarian    : int  0 0 0 NA 0 0 0 NA 0 0 ...
#>   ..- attr(*, "label")= Named chr "Binary indicator whether the order was vegetarian."
#>   .. ..- attr(*, "names")= chr "vegetarian"
#>  $ nps           : num  4 8 NA NA 6 3 6 9 10 7 ...
#>   ..- attr(*, "label")= Named chr "Net Promoter Score (1–10), ordinal customer rating."
#>   .. ..- attr(*, "names")= chr "nps"
#>  $ complaint     : int  0 0 NA NA 0 1 1 NA 0 0 ...
#>   ..- attr(*, "label")= Named chr "Binary indicator whether a complaint was filed."
#>   .. ..- attr(*, "names")= chr "complaint"
#>  $ style         : chr  "american" "italian" "italian" "italian" ...
#>   ..- attr(*, "label")= Named chr "Type of pizza (e.g. italian, american, gourmet, vegan)."
#>   .. ..- attr(*, "names")= chr "pizza_style"
#>  $ channel       : chr  "app" "web" "web" "app" ...
#>   ..- attr(*, "label")= Named chr "Order channel (app, web, phone)."
#>   .. ..- attr(*, "names")= chr "order_channel"
#>  $ tip           : num  5.14 2.37 NA NA 4.56 0 0 NA 5.89 5.6 ...
#>   ..- attr(*, "label")= Named chr "Tip amount in monetary units, derived from price and customer behaviour."
#>   .. ..- attr(*, "names")= chr "tip"

summary(bedrock::Pizza$delivery_min)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    8.80   17.40   24.40   25.65   32.50   65.60 
table(bedrock::Pizza$area, bedrock::Pizza$channel)
#>              
#>               app phone web
#>   Brent       265    48 146
#>   Camden      198    30 109
#>   Westminster 224    38 109

# the missing values are part of the design
colSums(is.na(bedrock::Pizza))
#>          index           date           week        weekday           area 
#>              0             32             32             32             10 
#>          count         rebate          price       operator         driver 
#>             12             12             12              8              5 
#>   delivery_min    temperature   wine_ordered wine_delivered     wrongpizza 
#>              0             39             12             12              4 
#>        quality     vegetarian            nps      complaint          style 
#>            201             51            105            129             22 
#>        channel            tip 
#>             32             81 
```
