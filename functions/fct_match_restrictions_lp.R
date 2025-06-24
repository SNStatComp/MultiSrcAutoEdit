
## Hulpfunctie match_restrictions_lp (en onderliggende functies)
## om de imputaties in stap 2 aan te passen via een lineair programmeringsprobleem.
## Code is geinspireerd op enkele functies uit het errorlocate-pakket.


expect_values_lp <- function (values, weights, ...){
  if (missing(weights)) {
    weights <- rep(1, length(values))
    names(weights) <- names(values)
  }

  stopifnot(length(values) == length(weights), all(names(values) %in%
                                                     names(weights)))

  is_numeric <- vapply(values, is.numeric, TRUE)
  lin_values <- values[is_numeric]
  is.na(lin_values) <- lin_values >= 1e+07
  lin_is_na <- vapply(lin_values, is.na, TRUE)

  lin_values <- lin_values[!lin_is_na]
  lin_rules1 <- lapply(names(lin_values), function(n) {
    a <- setNames(c(1,-1,1), paste0(n, c('', '.plus', '.min')))
    b <- lin_values[[n]]
    w <- weights[n]
    if (is.finite(w)) { # adjustment to n allowed
      errorlocate:::mip_rule(a, op = "==", b = b, rule = n, weight = Inf)
    } else { # no adjustment to n allowed
      errorlocate:::mip_rule(a[1], op = "==", b = b, rule = n, weight = Inf)
    }
  })

  lin_rules2 <- lapply(names(lin_values)[is.finite(weights[names(lin_values)])], function(n) {
    a <- setNames(1, paste0(n, '.plus'))
    errorlocate:::mip_rule(-a, op = "<=", b = 0, rule = paste0(n, '.plus'), weight = Inf)
  })

  lin_rules3 <- lapply(names(lin_values)[is.finite(weights[names(lin_values)])], function(n) {
    a <- setNames(1, paste0(n, '.min'))
    errorlocate:::mip_rule(-a, op = "<=", b = 0, rule = paste0(n, '.min'), weight = Inf)
  })

  cat_values <- values[!is_numeric]
  cat_rules <- lapply(names(cat_values), function(n) {
    value <- cat_values[[n]]
    a <- setNames(1, paste0(n, INFIX_CAT_NAME, value))
    b <- 1
    if (is.logical(value)) {
      names(a) <- n
      if (!isTRUE(value)) {
        a <- -a
        b <- 0
      }
      if (is.na(value)) {
        a[] <- 0
        b <- 1
      }
    }
    w <- weights[n]
    mr <- errorlocate:::mip_rule(a, op = "==", b = b, rule = n,
                                 weight = weights[n], type = sapply(a, function(x) "binary"))
    mr
  })
  c(lin_rules1, lin_rules2, lin_rules3, cat_rules)
}




match_restrictions_lp <- function(values, rules, weights) {

  lp_miprules <- errorlocate:::to_miprules(rules)

  lp_objective <- c(setNames(weights, paste0(names(weights), ".plus")),
                    setNames(weights, paste0(names(weights), ".min")))

  lp_value_rules <- expect_values_lp(values, weights)

  lp <- errorlocate:::translate_mip_lp(c(lp_miprules, lp_value_rules),
                                       lp_objective)
  lpSolveAPI::lp.control(lp, timeout = TIMEOUT, presolve = 'none')
  s <- lpSolveAPI::solve.lpExtPtr(lp)

  solution <- switch( as.character(s),
                      "0" = TRUE,  # optimal solution found (so feasible)
                      "1" = TRUE,  # sub optimal solution (so feasible)
                      "2" = FALSE, # infeasible
                      "3" = TRUE,  # unbounded (so feasible)
                      "4" = TRUE,  # degenerate (so feasible)
                      # "5" = NA,    # numerical failure, so unknown
                      # "6" = NA,    # process aborted
                      # "7" = NA,    # timeout
                      "9" = TRUE,  # presolved
                      "10" = FALSE, # branch and bound failed
                      "11" = FALSE, # branch and bound stopped
                      "12" = TRUE,  # a feasible branch and bound found
                      "13" = FALSE, # no feasible branch and bound found
                      FALSE
  )

  if (isTRUE(solution)){
    lp_values <- lpSolveAPI::get.variables(lp)
    names(lp_values) <- colnames(lp)
  } else { # try again with presolve
    lp <- errorlocate:::translate_mip_lp(c(lp_miprules, lp_value_rules),
                                         lp_objective)
    lpSolveAPI::lp.control(lp, timeout = TIMEOUT, presolve = 'rows')
    s <- lpSolveAPI::solve.lpExtPtr(lp)

    solution <- switch( as.character(s),
                        "0" = TRUE,  # optimal solution found (so feasible)
                        "1" = TRUE,  # sub optimal solution (so feasible)
                        "2" = FALSE, # infeasible
                        "3" = TRUE,  # unbounded (so feasible)
                        "4" = TRUE,  # degenerate (so feasible)
                        # "5" = NA,    # numerical failure, so unknown
                        # "6" = NA,    # process aborted
                        # "7" = NA,    # timeout
                        "9" = TRUE,  # presolved
                        "10" = FALSE, # branch and bound failed
                        "11" = FALSE, # branch and bound stopped
                        "12" = TRUE,  # a feasible branch and bound found
                        "13" = FALSE, # no feasible branch and bound found
                        FALSE
    )
    if (isTRUE(solution)){
      lp_values <- lpSolveAPI::get.variables(lp)
      names(lp_values) <- colnames(lp)
    } else {
      lp_values <- values
    }
  }

  return(list(status = s, solution = solution, lp_values = lp_values))
}


testregels <- validate::validator(
  if (a < 0) b <= 0.95*a,
  if (a < 0) b >= 1.05*a,
  if (a >= 0) b >= 0.95*a,
  if (a >= 0) b <= 1.05*a
)

testregels <- validate::validator(
  a + b == 100,
  a >= 0,
  b >= 0
)

test <- match_restrictions_lp(
  values = c(a = 100, b = 90),
  rules = testregels,
  weights = c(a = 1, b = 1)
)
test$status
test$solution
test$lp_values
test$lp_values['a']
test$lp_values['b']
