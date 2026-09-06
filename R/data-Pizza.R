
#' Pizza Delivery Data (Extended)
#'
#' An extended artificial dataset inspired by a similar dataset pizza.sav in
#' *Arbeitsbuch zur deskriptiven und induktiven Statistik* by Toutenburg
#' et al. The data describe a pizza delivery service in London serving three
#' areas, each record being one order and its associated characteristics.
#'
#' Compared to the original dataset, this extended version includes additional
#' behavioural and outcome variables such as customer satisfaction, Net Promoter
#' Score (NPS), complaints, dietary choices and tipping behaviour. These
#' variables are generated using probabilistic models to resemble realistic
#' business data, including noise, imperfect relationships and heterogeneous
#' customer behaviour.
#'
#' The dataset is designed to be realistically complex. It contains the data
#' types commonly met in practice: numerics, integers, factors, ordered factors,
#' logicals, characters and dates. Missing values occur both systematically and
#' at random, in every variable except `index`.
#'
#' @format A data frame with 1209 observations on 22 variables:
#' \describe{
#'   \item{index}{integer, index of the record, complete by construction.}
#'   \item{date}{date of the delivery.}
#'   \item{week}{numeric, the week of the year.}
#'   \item{weekday}{numeric, the day of the week.}
#'   \item{area}{factor with the levels `Brent`, `Camden` and
#'     `Westminster`.}
#'   \item{count}{integer, the number of pizzas delivered.}
#'   \item{rebate}{logical, `TRUE` if a rebate was given.}
#'   \item{price}{numeric, the total price of the pizzas delivered.}
#'   \item{operator}{factor with three levels, the operator taking the order.}
#'   \item{driver}{factor with seven levels, the driver delivering the order.}
#'   \item{delivery_min}{numeric, the delivery time in minutes.}
#'   \item{temperature}{numeric, the temperature in degrees Celsius on
#'     delivery.}
#'   \item{wine_ordered}{integer, `1` if wine was ordered, `0` if not.}
#'   \item{wine_delivered}{integer, `1` if wine was delivered, `0` if not.}
#'   \item{wrongpizza}{logical, `TRUE` if a wrong pizza was delivered.}
#'   \item{quality}{ordered factor with the levels `low < medium < high`, the
#'     quality of the pizza on delivery.}
#'   \item{vegetarian}{integer, `1` if the order was vegetarian, `0` if not.}
#'   \item{nps}{numeric, the Net Promoter Score from 1 to 10, an ordinal
#'     customer rating.}
#'   \item{complaint}{integer, `1` if a complaint was filed, `0` if not.}
#'   \item{style}{character, the type of pizza, e.g. `italian`, `american`,
#'     `gourmet` or `vegan`.}
#'   \item{channel}{character, the order channel, `app`, `web` or `phone`.}
#'   \item{tip}{numeric, the tip in monetary units.}
#' }
#'
#' @details
#' The variable `nps` is a simulated Net Promoter Score from 1 to 10,
#' calibrated to resemble realistic customer feedback distributions, including
#' asymmetric lower-tail behaviour.
#'
#' The variable `complaint` is generated using a probabilistic model depending
#' on delivery time, order correctness and additional noise, ensuring that
#' complaints are not deterministically linked to single factors.
#'
#' The variable `tip` is based on a percentage of the order price and is
#' influenced by customer satisfaction (`nps`), delivery performance and
#' driver-specific effects. Tips are zero for complaints or very low
#' satisfaction, and otherwise increase monotonically with customer
#' satisfaction while retaining stochastic variation.
#'
#' Overall, the dataset is designed to provide a realistic benchmark for
#' statistical modelling, including classification (binary and ordinal),
#' regression and performance evaluation, e.g. ROC curves and AUC with
#' confidence intervals.
#'
#' Every variable carries a `label` attribute with its description, so that
#' the labels can be used in tables and plots without repeating them in the
#' code.
#'
#' @examples
#' str(bedrock::Pizza)
#'
#' summary(bedrock::Pizza$delivery_min)
#' table(bedrock::Pizza$area, bedrock::Pizza$channel)
#'
#' # the missing values are part of the design
#' colSums(is.na(bedrock::Pizza))
#'
#' @source Simulated data.
#'
#' @family datasets
#' @concept simulation
#'
#' @references
#' Toutenburg H, Schomaker M, Wissmann M, Heumann C (2009):
#' *Arbeitsbuch zur deskriptiven und induktiven Statistik*
#' Springer, Berlin Heidelberg.
#'
"Pizza"
