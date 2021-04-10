## Release summary

This is a submission per request. 

* The function `load_sharpe_games()` broke a test because of a connection to a github server timed out. It is now designed to fail gracefully and not result in a test error.

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

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
