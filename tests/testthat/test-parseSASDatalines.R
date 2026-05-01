
library(testthat)

test_that("basic parsing works", {
  txt <- "
    data test;
    input id sex $ age;
    datalines;
    1 M 30
    2 F 25
    ;
  "
  
  df <- parseSASDatalines(txt)
  
  expect_equal(nrow(df), 2)
  expect_equal(ncol(df), 3)
  expect_equal(names(df), c("id", "sex", "age"))
  
  expect_equal(df$id, c(1, 2))
  expect_equal(df$sex, c("M", "F"))
  expect_equal(df$age, c(30, 25))
  
  expect_equal(attr(df, "sas_dataset_name"), "test")
})

test_that("DATA _NULL_ still parses data", {
  txt <- "
    data _NULL_;
    input x y;
    datalines;
    1 2
    3 4
    ;
  "
  
  df <- parseSASDatalines(txt)
  
  expect_equal(nrow(df), 2)
  expect_equal(attr(df, "sas_dataset_name"), "_NULL_")
})

test_that("SAS missing values are converted to NA", {
  txt <- "
    data test;
    input x y $;
    datalines;
    1 A
    . B
    3 .
    ;
  "
  
  df <- parseSASDatalines(txt)
  
  expect_true(is.na(df$x[2]))
  expect_true(is.na(df$y[3]))
})

test_that("CARDS4 block is parsed correctly", {
  txt <- "
    data test;
    input a b;
    cards4;
    1 2
    3 4
    ;;;;
  "
  
  df <- parseSASDatalines(txt)
  
  expect_equal(nrow(df), 2)
  expect_equal(df$a, c(1, 3))
})

test_that("multiple spaces are handled correctly", {
  txt <- "
    data test;
    input x y;
    datalines;
    1     2
    3 4
    ;
  "
  
  df <- parseSASDatalines(txt)
  
  expect_equal(df$x, c(1, 3))
  expect_equal(df$y, c(2, 4))
})

test_that("error on wrong number of tokens", {
  txt <- "
    data test;
    input x y;
    datalines;
    1 2
    3
    ;
  "
  
  expect_error(parseSASDatalines(txt), "Row")
})

test_that("error if DATA statement missing", {
  txt <- "
    input x y;
    datalines;
    1 2
    ;
  "
  
  expect_error(parseSASDatalines(txt), "DATA")
})

test_that("error if INPUT missing", {
  txt <- "
    data test;
    datalines;
    1 2
    ;
  "
  
  expect_error(parseSASDatalines(txt), "INPUT")
})

test_that("error if DATALINES missing", {
  txt <- "
    data test;
    input x y;
  "
  
  expect_error(parseSASDatalines(txt), "DATALINES")
})

test_that("@ is rejected", {
  txt <- "
    data test;
    input x @1 y;
    datalines;
    1 2
    ;
  "
  
  expect_error(parseSASDatalines(txt), "@")
})

test_that("column input is rejected", {
  txt <- "
    data test;
    input x 1-2 y;
    datalines;
    12 3
    ;
  "
  
  expect_error(parseSASDatalines(txt), "Column input")
})

test_that("formatted input is rejected", {
  txt <- "
    data test;
    input x :8.;
    datalines;
    12345678
    ;
  "
  
  expect_error(parseSASDatalines(txt), "Formatted input")
})

test_that("invalid dataset name triggers warning", {
  txt <- "
    data 123invalid;
    input x;
    datalines;
    1
    ;
  "
  
  expect_warning(parseSASDatalines(txt, validate_names = TRUE))
})

test_that("single variable works", {
  txt <- "
    data test;
    input x;
    datalines;
    1
    2
    ;
  "
  
  df <- parseSASDatalines(txt)
  
  expect_equal(df$x, c(1, 2))
})

test_that("empty datalines throws error", {
  txt <- "
    data test;
    input x y;
    datalines;
    ;
  "
  
  expect_error(parseSASDatalines(txt), "No data")
})

