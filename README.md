IFNread - Reading IFN source files
================

<!-- badges: start -->
[![R-CMD-check](https://github.com/emf-creaf/IFNread/workflows/R-CMD-check/badge.svg)](https://github.com/emf-creaf/IFNread/actions)
<!-- badges: end -->

## Introduction

Package **IFNread** is designed to assist reading source files of the
Spanish National Forest Inventory (‘Inventario Forestal Nacional’ or
‘IFN’).

## Package installation

The latest stable versions GitHub as follows (required package
`devtools` should be installed/updated first):

``` r
devtools::install_github("emf-creaf/IFNread")
```

## Usage

Second, third and fourth inventories (i.e. IFN2, IFN3 and IFN4) are
supported. Functions are defined to read from:

-   Database files (.DBF) in the case of IFN2
-   Access files (.mdb) in the case of IFN3 and IFN4

The package is still under development and it is expected that functions
fail in many cases.
