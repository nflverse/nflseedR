# Contributing

Many hands make light work! Here are some ways you can contribute to this project:

### Open an issue

- You can [open an issue](https://github.com/nflverse/nflseedR/issues/new/choose) if you'd like to request a specific function or report a bug/error.

### Fixing typos

*   You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

### Bigger changes

*   If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

*   You can also bring up changes in the [nflverse discord](https://discord.com/invite/5Er2FBnnQa) and our team will be happy to discuss!

### Code style

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
    You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://roxygen2.r-lib.org/articles/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

### Code of Conduct

Please note that the usethis project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.

*These contribution guidelines were inspired by the guidelines from [{usethis}](https://github.com/r-lib/usethis)*
