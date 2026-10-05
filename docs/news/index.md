# Changelog

## bedrock 0.1.17

- `courseData()` has been renamed to
  [`readCourseData()`](../reference/readCourseData.md), which describes
  the function better.

## bedrock 0.1.15

- [`resolveFormula()`](../reference/resolveFormula.md): `subset` now
  follows base R semantics (evaluated in `data`, as in
  [`lm()`](https://rdrr.io/r/stats/lm.html)); new
  [`resolveFormulaFromCall()`](../reference/resolveFormula.md) for
  formula methods. `y ~ a:b` is accepted as the cells of several
  grouping variables.
- New [`conceptMap()`](../reference/concepts.md) and
  [`conceptAudit()`](../reference/concepts.md);
  [`getConcepts()`](../reference/concepts.md) is now exported.
- Fixed a test failing on macOS arm64 (CRAN M1mac).

## bedrock 0.1.9

CRAN release: 2026-09-29

### Initial CRAN release

- First public release of bedrock, the base layer of the DescToolsX
  package suite: data manipulation and reshaping, predicates for data
  inspection, vector and string operations, labels and metadata, and
  routines from number theory and combinatorics.
- Performance critical routines are implemented in C++ via Rcpp.
- Documented in the vignette “Combinatorics”.

### Acknowledgements

Parts of the code and documentation were reviewed with the help of large
language models (OpenAI Codex, Anthropic Claude). Every suggestion was
assessed, edited and verified by the maintainer, who remains solely
responsible for the content of this package.
