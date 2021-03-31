## Resubmission

This is a resubmission per request. 

* It was requested that we unwrap the examples from `\donttest{}` if they can be executed in < 5 seconds. Unfortunately the tests on rhub have shown that this causes NOTES (> 5 seconds) on some Linux and MacOS machines. Instead we have added `testthat` tests for all functions as we don't see a chance to speed up the examples significantly. The tests were previously skipped on cran but now they are more robust (by using the new dependency curl in the function `load_sharpe_games()`) and no more skipped
* Restore user's options in examples

## Test environments

* local R installation, R 4.0.4
* Debian Linux, R-devel, clang, ISO-8859-15 locale 
* Debian Linux, R-devel, GCC 
* Debian Linux, R-devel, GCC, no long double 
* Debian Linux, R-patched, GCC 
* Debian Linux, R-release, GCC 
* Fedora Linux, R-devel, clang, gfortran 
* Fedora Linux, R-devel, GCC 
* Debian Linux, R-devel, GCC ASAN/UBSAN
* macOS 10.13.6 High Sierra, R-release, brew 
* macOS 10.13.6 High Sierra, R-release, CRAN's setup 
* Oracle Solaris 10, x86, 32 bit, R-release
* Ubuntu Linux 20.04.1 LTS, R-devel, GCC 
* Ubuntu Linux 20.04.1 LTS, R-release, GCC 
* Ubuntu Linux 20.04.1 LTS, R-devel with rchk
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit 
* Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit
* Windows Server 2008 R2 SP1, R-patched, 32/64 bit 
* Windows Server 2008 R2 SP1, R-release, 32/64 bit
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission
