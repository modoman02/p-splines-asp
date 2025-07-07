# asppsplines 

# Installation guide

This package unfortunately cannot yet be installed directly from GitLab due to problems with access to the university based GitLab. 
Instead, please follow these steps to install it locally on your computer. 

---

### 1. Clone the Repository
First, please clone the repository from GitLab to your machine. 
For an easy way: Open your terminal and run:

```bash
git clone https://gitlab.gwdg.de/nicolas.wackermann/p-splines-asp.git
```

This should create a folder called: p-splines-asp

### 2. Please Install the Package Locally in R

Next, open R and run the following command. Keep in mind to check, if your path is correct. 

```r
# Install devtools if you don’t have it yet
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

library(devtools)
```

```r
# Change to your local path, if needed
devtools::install("p-splines-asp")
```

Alternatively, you can set the working directory to the folder first: 

```r
setwd("path/to/p-splines-asp")
devtools::install(".")
```
This should load the package asppsplines on your system. 

### 3. Load the Package

```r
library(asppsplines)
```

To run the entire analysis workflow, use the function: 
```r
run_all_funcs()
```
This function executes the full pipeline from creating spline basis to parameter estimation.

See the function documentation for details: 

```r
?run_all_funcs
```

---

## Package Description

`asppsplines` is an R package for adaptive spline modeling with heteroscedastic variance structures. 

It aims to implement a flexible and easy to handle framework to estimate both the mean (`μ`) and variance (`σ²`) curves using penalized spline bases.

Key features of the Package:

- Generation of B-spline and truncated power basis functions. The user can decide, which of the two they would like to use. 
- Iterative estimation of mean and variance curves via penalized likelihood and Fisher scoring.
- Automatic selection of smoothing parameters through grid search and Generalized Cross-Validation (GCV).
- Support for heteroscedastic modeling, allowing variance to change smoothly over the predictor space.
- Integrated first versions of plotting functions to visualize fitted curves and confidence intervals.
- A wrapper function `run_all_funcs()` that executes the complete modeling workflow in a single call.
- Summary tools for inspecting results and convergence diagnostics.

This package is useful and designed for students, who wish to model non-linear data with flexible, smooth structures while accounting for non-constant variances across the domain.

---

### Authors: 

- Nicolas Wackermann (<nicolas.wackermann@stud.uni-goettingen.de>)
- Moritz Dallmann (<moritz.dallmann@stud.uni-goettingen.de>)

---

### Repository

The full source code is hosted on GitLab: 
[https://gitlab.gwdg.de/nicolas.wackermann/p-splines-asp](https://gitlab.gwdg.de/nicolas.wackermann/p-splines-asp)

---

