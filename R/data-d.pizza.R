
#' Pizza delivery data (extended)
#'
#' An extended artificial dataset inspired by a similar dataset pizza.sav in
#' \emph{Arbeitsbuch zur deskriptiven und induktiven Statistik} by Toutenburg
#' et al.\cr The dataset contains data of a pizza delivery service in London,
#' delivering pizzas to three areas. Each record represents one order/delivery
#' and its associated characteristics.
#'
#' Compared to the original dataset, this extended version includes additional
#' behavioural and outcome variables such as customer satisfaction, Net Promoter
#' Score (NPS), complaints, dietary choices and tipping behaviour. These variables
#' are generated using probabilistic models to resemble realistic business data,
#' including noise, imperfect relationships and heterogeneous customer behaviour.
#'
#' The dataset is designed to be realistically complex. It contains commonly used
#' data types such as numerics, factors, ordered factors, integers, logicals and
#' dates. Missing values occur both systematically and randomly (except for the index).
#'
#' A data frame with 1209 observations on XX variables:
#' \describe{
#'   \item{index}{Numeric index of the record.}
#'   \item{date}{Date of delivery.}
#'   \item{week}{Week number.}
#'   \item{weekday}{Weekday (integer).}
#'   \item{area}{Factor with levels Brent, Camden, Westminster.}
#'   \item{count}{Number of pizzas delivered.}
#'   \item{rabate}{Logical, TRUE if a rebate was given.}
#'   \item{price}{Total price of delivered pizzas.}
#'   \item{operator}{Factor indicating the operator.}
#'   \item{driver}{Factor indicating the driver.}
#'   \item{delivery_min}{Delivery time in minutes.}
#'   \item{temperature}{Temperature in degrees Celsius when delivered.}
#'   \item{wine_ordered}{Integer, 1 if wine was ordered, 0 if not}
#'   \item{wine_delivered}{Integer, 1 if wine was delivered, 0 if not}
#'   \item{wrongpizza}{Logical, TRUE if a wrong pizza was delivered}
#'   \item{quality}{Ordered factor with levels \code{low < medium < high}, 
#'             defining the quality of the pizza when delivered}
#'             
#'   \item{vegetarian}{Binary indicator whether the order was vegetarian.}
#'   \item{nps}{Net Promoter Score (1–10), ordinal customer rating.}
#'   \item{complaint}{Binary indicator whether a complaint was filed.}
#'   \item{pizza_style}{Type of pizza (e.g. italian, american, gourmet, vegan).}
#'   \item{order_channel}{Order channel (app, web, phone).}
#'   \item{tip}{Tip amount in monetary units, derived from price and customer behaviour.}
#' }
#' 
#' @details
#' The variable `nps` is a simulated Net Promoter Score (1–10), calibrated to
#' resemble realistic customer feedback distributions, including asymmetric
#' lower-tail behaviour.
#'
#' The variable `complaint` is generated using a probabilistic model depending on
#' delivery time, order correctness and additional noise, ensuring that complaints
#' are not deterministically linked to single factors.
#'
#' The variable `tip` is based on a percentage of the order price and is influenced
#' by customer satisfaction (NPS), delivery performance and driver-specific effects.
#' Tips are zero for complaints or very low satisfaction, and otherwise increase
#' monotonically with customer satisfaction while retaining stochastic variation.
#'
#' Overall, the dataset is designed to provide a realistic benchmark for statistical
#' modelling, including classification (binary and ordinal), regression and
#' performance evaluation (e.g. ROC curves, AUC with confidence intervals).
#'
#' @format A data frame with 1209 observations and XX variables.
#' @source Simulated data.
#'
#' @references
#' Toutenburg H, Schomaker M, Wissmann M, Heumann C (2009):
#' \emph{Arbeitsbuch zur deskriptiven und induktiven Statistik}
#' Springer, Berlin Heidelberg.
#'
"d.pizza"