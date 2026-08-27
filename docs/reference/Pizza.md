# Pizza delivery data (extended)

An extended artificial dataset inspired by a similar dataset pizza.sav
in *Arbeitsbuch zur deskriptiven und induktiven Statistik* by Toutenburg
et al.  
The dataset contains data of a pizza delivery service in London,
delivering pizzas to three areas. Each record represents one
order/delivery and its associated characteristics.

## Usage

``` r
Pizza
```

## Format

A data frame with 1209 observations and 22 variables.

## Source

Simulated data.

## Details

Compared to the original dataset, this extended version includes
additional behavioural and outcome variables such as customer
satisfaction, Net Promoter Score (NPS), complaints, dietary choices and
tipping behaviour. These variables are generated using probabilistic
models to resemble realistic business data, including noise, imperfect
relationships and heterogeneous customer behaviour.

The dataset is designed to be realistically complex. It contains
commonly used data types such as numerics, factors, ordered factors,
integers, logicals and dates. Missing values occur both systematically
and randomly (except for the index).

A data frame with 1209 observations on 22 variables:

- index:

  Numeric index of the record.

- date:

  Date of delivery.

- week:

  Week number.

- weekday:

  Weekday (integer).

- area:

  Factor with levels Brent, Camden, Westminster.

- count:

  Number of pizzas delivered.

- rebate:

  Logical, TRUE if a rebate was given.

- price:

  Total price of delivered pizzas.

- operator:

  Factor indicating the operator.

- driver:

  Factor indicating the driver.

- delivery_min:

  Delivery time in minutes.

- temperature:

  Temperature in degrees Celsius when delivered.

- wine_ordered:

  Integer, 1 if wine was ordered, 0 if not

- wine_delivered:

  Integer, 1 if wine was delivered, 0 if not

- wrongpizza:

  Logical, TRUE if a wrong pizza was delivered

- quality:

  Ordered factor with levels `low < medium < high`, defining the quality
  of the pizza when delivered

- vegetarian:

  Binary indicator whether the order was vegetarian.

- nps:

  Net Promoter Score (1–10), ordinal customer rating.

- complaint:

  Binary indicator whether a complaint was filed.

- pizza_style:

  Type of pizza (e.g. italian, american, gourmet, vegan).

- order_channel:

  Order channel (app, web, phone).

- tip:

  Tip amount in monetary units, derived from price and customer
  behaviour.

The variable `nps` is a simulated Net Promoter Score (1–10), calibrated
to resemble realistic customer feedback distributions, including
asymmetric lower-tail behaviour.

The variable `complaint` is generated using a probabilistic model
depending on delivery time, order correctness and additional noise,
ensuring that complaints are not deterministically linked to single
factors.

The variable `tip` is based on a percentage of the order price and is
influenced by customer satisfaction (NPS), delivery performance and
driver-specific effects. Tips are zero for complaints or very low
satisfaction, and otherwise increase monotonically with customer
satisfaction while retaining stochastic variation.

Overall, the dataset is designed to provide a realistic benchmark for
statistical modelling, including classification (binary and ordinal),
regression and performance evaluation (e.g. ROC curves, AUC with
confidence intervals).

## References

Toutenburg H, Schomaker M, Wissmann M, Heumann C (2009): *Arbeitsbuch
zur deskriptiven und induktiven Statistik* Springer, Berlin Heidelberg.

## See also

Other datasets: [`Cards`](Cards.md), [`Roulette`](Roulette.md),
[`Tarot`](Tarot.md), [`courseData()`](courseData.md)
