
################################################################################
## Extensions of functions from the errorlocate package necessary for
## automatic error localization with general edit operations
##
## This version of the code works in conjunction with the
## validate, errorlocate (v0.9.9), deductive and rspa packages
##
## Authors: Jacco Daalmans and Sander Scholtus
##
## Date: 8 May 2025
################################################################################


# extension of the function errorlocate:::expect_values

expect_values_eo <- function (values, weights, operations, weights_eo, delta_names = NULL, ...){
  if (missing(weights)) {
    weights <- rep(1, length(values))
    names(weights) <- names(values)
  }
  if (is.null(delta_names)) {
    delta_names <- names(values)
    names(delta_names) <- delta_names
  }
  stopifnot(length(values) == length(weights), all(names(values) %in%
                                                     names(weights)))

  # prepare special edit operations for this record
  if (missing(operations) | missing(weights_eo) | length(operations) == 0) {
    eo <- list()
    weights_eo <- numeric()
  } else {
    vv <- unlist(values)
    eo <- lapply(operations, function (p) {
      vv0 <- vv[names(vv) %in% names(p)]
      if (any(is.na(vv0))) {
        # if any variables that determine the fixed value are missing, ignore this edit operation
        return(NULL)
      } else {
		ptemp <- p[ , names(p) %in% names(vv), drop = FALSE]
		p$fixed <- data.matrix(ptemp) %*% vv0[names(ptemp)]
        p[ , !(names(p) %in% names(vv)), drop = FALSE]
      }
    })

    toremove <- which(sapply(eo, is.null))
    if (length(toremove) > 0) {
      eo <- eo[-toremove]
      weights_eo <- weights_eo[-toremove]
    }

  }

  is_numeric <- vapply(values, is.numeric, TRUE)
  lin_values <- values[is_numeric]
  is.na(lin_values) <- lin_values >= 1e+07
  lin_is_na <- vapply(lin_values, is.na, TRUE)

  if (length(eo) > 0) {
    # transform special edit operations to list by variable
    eo_fixed <- lapply(names(lin_values), function(n) {
      sel <- sapply(eo, function(l) any(l$varTarget == n))
      if (any(sel)) {
        res <- sapply(eo[sel], function(l) l$fixed[l$varTarget == n])
        names(res) <- paste0('.delta_', names(res))
        res
      } else {
        NULL
      }
    })
    eo_free <- lapply(names(lin_values), function(n) {
      sel <- sapply(eo, function(l) any(l$varTarget == n))
      L <- lapply(eo[sel], function(l) l[l$varTarget == n, !(names(l) %in% c('varTarget','fixed')), drop = FALSE])
      names(L) <- NULL
      fr <- do.call(cbind, L)
      unlist(fr)
    })
  } else {
    eo_fixed <- eo_free <- lapply(names(lin_values), function(L) NULL)
  }
  names(eo_fixed) <- names(eo_free) <- names(lin_values)

  lin_values <- lin_values[!lin_is_na]
  lin_rules1 <- lapply(names(lin_values), function(n) {
    a <- c(setNames(1, n),
           -1*eo_fixed[[n]][eo_fixed[[n]] != 0],
           -1*eo_free[[n]])
    ty <- c('double', rep('binary', sum(eo_fixed[[n]] != 0)), rep('double', length(eo_free[[n]])))
    names(ty) <- names(a)
    b <- lin_values[[n]]
    w <- weights[n]
    if (is.finite(w)) {
      n_ub <- paste0(n, "_ub")
      errorlocate:::soft_lin_rule(errorlocate:::mip_rule(a, op = "<=", b = b,
                                                         rule = n_ub, type = ty, weight = w),
                                  name = delta_names[n])
    }
    else {
      errorlocate:::mip_rule(a, op = "==", b = b, rule = n, weight = Inf)
    }
  })

  lin_rules2 <- lapply(names(lin_values), function(n) {
    a <- c(setNames(1, n),
           -1*eo_fixed[[n]][eo_fixed[[n]] != 0],
           -1*eo_free[[n]])
    ty <- c('double', rep('binary', sum(eo_fixed[[n]] != 0)), rep('double', length(eo_free[[n]])))
    names(ty) <- names(a)
    b <- lin_values[[n]]
    w <- weights[n]
    if (is.finite(w)) {
      n_lb <- paste0(n, "_lb")
      errorlocate:::soft_lin_rule(errorlocate:::mip_rule(-a, op = "<=", b = -b,
                                                         rule = n_lb, type = ty, weight = w),
                                  name = delta_names[n])
    }
    else {
      NULL
    }
  })
  lin_rules2 <- lin_rules2[!sapply(lin_rules2, is.null)]

  lin_rules3 <- lapply(names(lin_values), function(n) {
    nm <- names(eo_fixed[[n]])
    if (length(nm) > 0) {
      a <- setNames(rep(1, length(nm)), nm)
      ty <- rep('binary', length(nm))
      names(ty) <- names(a)
      errorlocate:::mip_rule(a, op = "<=", b = 1, rule = n, type = ty, weight = Inf)
    } else {
      NULL
    }
  })
  lin_rules3 <- lin_rules3[!sapply(lin_rules3, is.null)]

  freevars <- lapply(names(eo), function(m) {
    l <- eo[[m]]
    ll <- names(l)[!(names(l) %in% c('varTarget','fixed'))]
    if (length(ll) == 0) {
      NULL
    } else {
      setNames(rep(m, length(ll)), ll)
    }
  })
  freevars <- do.call(c, freevars)
  ._vars_eo <<- names(freevars)

  if (!is.null(freevars)) {
    lin_rules4 <- lapply(._vars_eo, function(n) {
      a <- setNames(1, n)
      b <- 0
      w <- weights_eo[freevars[n]]
      errorlocate:::soft_lin_rule(errorlocate:::mip_rule(a, op = "<=", b = b,
                                                         rule = freevars[n], weight = w))
    })
    lin_rules5 <- lapply(._vars_eo, function(n) {
      a <- setNames(1, n)
      b <- 0
      w <- weights_eo[freevars[n]]
      errorlocate:::soft_lin_rule(errorlocate:::mip_rule(-a, op = "<=", b = -b,
                                                         rule = freevars[n], weight = w))
    })
  } else {
    lin_rules4 <- lin_rules5 <- NULL
  }

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
    if (is.finite(w)) {
      mr <- errorlocate:::soft_cat_rule(mr)
    }
    mr
  })
  c(lin_rules1, lin_rules2, lin_rules3, lin_rules4, lin_rules5, cat_rules)
}



# extension of the errorlocate class "MipRules" for edit operations

miprules_eo <- setRefClass("MipRulesEO",
                           fields = list(
                             rules         = "validator",
                             objective     = "numeric",
                             operations    = "list",
                             ._miprules    = "list",
                             ._log_rules   = "list", # extra constraints for log transformed rules
                             ._value_rules = "list",
                             ._vars        = "character",
                             ._vars_num    = "character",
                             ._vars_eo     = "character",
                             ._log_transform = "data.frame",
                             ._ignored     = "ANY",
                             ._lp          = "ANY"
                           ),
                           methods = list(
                             initialize = function(rules = NULL, operations, n = 10){
                               if (is.null(rules)){ return()}
                               rules <<- rules
                               operations <<- operations
                               objective <<- objective

                               ._miprules <<- errorlocate:::to_miprules(rules)
                               ._vars <<- errorlocate:::get_translatable_vars(rules)

                               var_num <- sapply(._miprules, function(mr){
                                 names(mr$type)[mr$type == "double"]
                               })
                               var_num <- as.character(unique(unlist(var_num)))

                               # extract log transformed variables
                               ._log_transform <<- errorlocate:::log_extract(var_num)

                               # set log constraints
                               ._log_rules <<- errorlocate:::create_log_constraints(._log_transform)

                               # make sure original variables are also in _vars_num
                               ._vars_num <<- unique(c(._log_transform$num_vars, var_num))

                             },
                             mip_rules = function(){
                               c(._miprules, ._log_rules, ._value_rules)
                             },
                             set_values = function(values, weights, operations, weights_eo){
                               if (missing(values) || length(values) == 0){
                                 objective <<- numeric()
                                 ._value_rules <<- list()
                                 return(invisible())
                               }

                               missing_vars <- ._vars[!._vars %in% names(values)]
                               if (length(missing_vars)){
                                 stop("Missing variable(s): "
                                      , paste0("'", missing_vars, "'", collapse = ", ")
                                      , "."
                                      , call. = FALSE)
                               }

                               if (missing(weights)){
                                 weights <- rep(1, length(values))
                                 names(weights) <- names(values)
                               }

                               # omitting vars that are not in rules...
                               values <- values[._vars]
                               weights <- weights[._vars]

                               ._value_rules <<- expect_values_eo(values, weights, operations, weights_eo)

                               # TODO move this to the outside
                               weights <- add_noise(weights)

                               if (length(operations) > 0) {
                                 objective <<- c(setNames(weights, paste0(".delta_", names(weights))),
                                                 setNames(weights_eo, paste0(".delta_", names(weights_eo))))
                               } else {
                                 objective <<- setNames(weights, paste0(".delta_", names(weights)))
                               }

                             },
                             update_log_constraints = function(data, n = 10){
                               ._log_rules <<- errorlocate:::create_log_constraints(._log_transform
                                                                                    , data = data
                                                                                    , n = n
                               )
                             },
                             to_lp = function(...){
                               errorlocate:::translate_mip_lp(mip_rules(), objective, ...)
                             },
                             write_lp = function(filename, ...){
                               lpSolveAPI::write.lp(to_lp(), filename, ...)
                             },
                             execute = function(...){
                               # TODO see if this can be executed in parallel.
                               lp <- errorlocate:::translate_mip_lp(mip_rules(), objective, ...)
                               #TODO set timer, duration etc.
                               s <- solve(lp)

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
                                 values <- lpSolveAPI::get.variables(lp)
                               } else {
                                 values <- rep(1, ncol(lp))
                               }
                               names(values) <- colnames(lp)

                               # work-around: replace part of names
                               lp1 <- to_lp()
                               aux <- colnames(lp1)[as.integer(gsub('^C', '', colnames(lp)[grep('^C', colnames(lp))]))]
                               aux1 <- grep('^C', colnames(lp))
                               ss <- colnames(lp)
                               # print(names(values)[aux1])
                               # print(aux)
                               ss[aux1] <- aux
                               names(values) <- ss

                               adapt <- objective[is.finite(objective)] < 0 # trick to create logical with names
                               adapt_nms <- names(adapt)[names(adapt) %in% names(values)]
                               adapt[adapt_nms] <- values[adapt_nms] == 1
                               if (length(values) == 0){
                                 # seems optimalisation of lpSolvAPI when there is only 1 column of data..
                                 # adapt <- objective > 0
                                 adapt[] <- lpSolveAPI::get.objective(lp) > 0
                               }

                               # remove prefix
                               names(adapt) <- gsub(".delta_", "", names(adapt))
                               #TODO improve the return values based on value of s
                               # Add the same table as with infeasible
                               list(
                                 s = s,
                                 solution = solution,
                                 values = values,
                                 lp = lp,
                                 adapt = adapt,
                                 errors = adapt
                               )
                             },
                             is_infeasible = function(){  # since we only check a subset of the rules,
                               mr <- .self$mip_rules()    # we can only detect infeasiblity, not feasiblity
                               vars <- errorlocate:::get_mr_vars(mr)
                               obj <- rep(1, length(vars))
                               names(obj) <- vars
                               lp <- errorlocate:::translate_mip_lp(mr, obj, break.at.first = TRUE)
                               i <- lpSolveAPI::solve.lpExtPtr(lp)
                               feasible <- switch( as.character(i),
                                                   "0" = TRUE,  # optimal solution found (so feasible)
                                                   "1" = TRUE,  # sub optimal solution (so feasible)
                                                   "2" = FALSE, # infeasible
                                                   "3" = TRUE,  # unbounded (so feasible)
                                                   "4" = TRUE,  # degenerate (so feasible)
                                                   "5" = NA,    # numerical failure, so unknown
                                                   "6" = NA,    # process aborted
                                                   "7" = NA,    # timeout
                                                   "9" = TRUE,  # presolved
                                                   "10" = FALSE, # branch and bound failed
                                                   "11" = FALSE, # branch and bound stopped
                                                   "12" = TRUE,  # a feasible branch and bound found
                                                   "13" = FALSE, # no feasible branch and bound found
                                                   FALSE
                               )
                               !feasible
                             },
                             show = function(){
                               mr <- mip_rules()
                               cat("Mip rules:\n")
                               cat(paste(sapply(mr, as.character), collapse = "\n"))
                               cat("Mip rules object:\n")
                               cat("   methods: '$to_lp()', '$execute', '$set_values()'\n")
                               cat("   properties: '$mip_rules', '$objective', '$is_infeasible', '$rules'\n")
                               # print(mr)
                               cat("\n")
                               cat("Generates the lp program (see ?inspect_mip) \n\n")
                               print(to_lp())

                             }
                           )
)


# extension of the errorlocate class "FHLocalizer" for edit operations

eo_localizer <-
  setRefClass("EOLocalizer",

              contains="ErrorLocalizer",

              fields = list(
                operations = "list",
                ._miprules = "ANY"
              ),

              methods = list(

                initialize = function(rules, operations, ref = NULL){
                  rules <<- rules
                  operations <<- operations
                  ._miprules <<- miprules_eo(rules, operations)
                },

                locate = function(data, weight = NULL, weight_eo, add_noise = TRUE, ...
                                  , cl = NULL
                                  , n = 10
                                  , Ncpus = getOption("Ncpus", 1)
                                  , timeout = 60){
                  vars <- ._miprules$._vars
                  nr_rows <- nrow(data)
                  nr_cols <- ncol(data)
                  names_cols <- colnames(data)
                  missing_vars <- vars[!vars %in% names(data)]

                  if (length(missing_vars)){
                    # if they are part of the environment remove...
                    mv_in_env <- sapply(missing_vars, exists)
                    vars <- setdiff(vars, missing_vars[mv_in_env])
                    ._miprules$._vars <<- vars

                    missing_vars <- missing_vars[!mv_in_env]
                  }

                  if (length(missing_vars)){
                    stop('Missing column(s): '
                         , paste0("'", missing_vars, "'", collapse = ", ")
                         , ". Add them to your data and rerun."
                         , call. = FALSE
                    )
                  }

                  numvars_mip <- ._miprules$._vars_num
                  numvars_data <- names(data)[sapply(data, is.numeric)]
                  numvars <- numvars_mip[numvars_mip %in% numvars_data]

                  # checking size of numeric columns
                  too_big <- sapply(data[numvars], function(v){
                    any(abs(v) > 1e7)
                  })

                  if (isTRUE(any(too_big))){
                    nv <- numvars[too_big]
                    data[nv] <- sapply(data[nv], function(v){
                      is.na(v) <- abs(v) > 1e7
                      v
                    })

                    warning("Large values detected in: "
                            , paste0("'", names(too_big)[too_big] ,"'", collapse = ", ")
                            , ". Values > abs(1e7) were set to NA. "
                            , "\nThis might be indication that these column(s) should be rescaled."
                            , "\n(the problem because otherwise numerically unstable)"
                            , call. = FALSE
                    )
                  }

                  categorical_as_integer <- numvars_data[ numvars_data %in% vars
                                                          & !numvars_data %in% numvars_mip
                  ]
                  if (length(categorical_as_integer)){
                    stop('Categorical columns coded as integers: '
                         , paste0("'", categorical_as_integer, "'", collapse = ", ")
                         , ". Change them to `factor` and rerun."
                         , call. = FALSE
                    )
                  }

                  if (length(weight) == 0){
                    weight <- matrix(1, nrow = nr_rows, ncol = nr_cols)
                    colnames(weight) <- names_cols
                  } else {
                    if (is.null(dim(weight))){
                      if (length(weight) == ncol(data)){
                        if (!is.null(names(weight))){
                          weight <- weight[names_cols]
                        }
                        # use recycling to fill a weight matrix and transpose...
                        weight <- t(matrix(weight, nrow = nr_cols, ncol = nr_rows))
                        colnames(weight) <- names_cols
                      }
                    }
                    stopifnot(dim(weight) == dim(data))
                    if (is.null(colnames(weight))){
                      colnames(weight) <- names_cols
                    } else {
                      weight <- weight[, names_cols, drop = FALSE]
                    }
                    stopifnot(names(weight) == names_cols)
                  }

                  if (missing(weight_eo)) {
                    ## assign weights to edit operations for target function
                    ## default setting (for now):
                    ## operation that involves one variable          --> weight = weight_variable / 2
                    ## operation that involves two variables or more --> weight = min(weight_variable) + 1/2
                    weight_eo <- vapply(names(operations), function(n) {
                      V <- operations[[n]]$varTarget
                      if (length(V) == 1) {
                        as.numeric(weight[ ,V,drop=F]/2)
                      } else {
                        as.numeric(apply(weight[ ,V,drop=F], 1, min) + (1/2))
                      }
                    }, numeric(nrow(weight)))
                    if (is.null(dim(weight_eo))){
                      weight_eo <- t(weight_eo)
                      colnames(weight_eo) <- names(operations)
                    }

                  } else {
                    ## assign weights to edit operations as specified by user
                    if (is.null(dim(weight_eo))){
                      if (length(weight_eo) == length(operations)){
                        if (!is.null(names(weight_eo))){
                          weight_eo <- weight_eo[names(operations)]
                        }
                        # use recycling to fill a weight matrix and transpose...
                        weight_eo <- t(matrix(weight_eo, nrow = length(operations), ncol = nr_rows))
                        colnames(weight_eo) <- names(operations)
                      }
                    }
                    stopifnot(dim(weight_eo) == c(nr_rows, length(operations)))
                    if (is.null(colnames(weight_eo))){
                      colnames(weight_eo) <- names(operations)
                    } else {
                      weight_eo <- weight_eo[, names(operations), drop = FALSE]
                    }
                    stopifnot(names(weight_eo) == names(operations))
                  }

                  # derive log transformed data!
                  # log_transform <- ._miprules$._log_transform
                  # TODO deal with failures when log of negative values is taken...
                  # log_data <- log_derived_data(data, log_transform)

                  # TODO add `n` to arguments of function
                  # set ranges of log constraints if any
                  ._miprules$update_log_constraints(data, n = n)

                  N <- nr_rows
                  rows <- seq_len(N)

                  # filter for records that are valid..., that reduces the processing
                  # time considerably
                  cf <- validate::confront(data, rules)
                  agg <- aggregate(cf, by = "record")
                  invalid <- (agg$nfail + agg$nNA) > 0

                  n_invalid <- sum(invalid)

                  #TODO add ref data !!!

                  # TODO export data/weight/log_data to parallel (is more efficient then
                  # copying the data over and over...)
                  # data, weight, log_data, mip
                  mip <- ._miprules

                  solve_record <- function(r, progress = invisible){
                    starttime <- Sys.time()
                    el <- tryCatch({
                      values <- as.list(data[r,,drop=FALSE])
                      mip$set_values( values = values
                                      , weight[r,]
                                      , operations
                                      , weight_eo[r,]
                                      # , log_values = as.list(log_data[r,,drop=FALSE])
                                      # , delta_names = log_transform$num_vars
                      )
                      mip$execute(timeout=timeout, ...)
                    }, error = function(e){
                      list(solution=FALSE, s = NA, adapt = logical())
                    })

                    if (!isTRUE(el$solution)){
                      # test for numerical instability?
                      # retry because of numerical instability
                      mip$set_values( values = values
                                      , weight[r,]
                                      , operations
                                      , weight_eo[r,]
                      )
                      # could be a scaling issue. Drop geometric scaling
                      warning("Dropping geometric `scaling` for record ",r, " (?lpSolveAPI::lp.control.options)"
                              , call. = FALSE
                      )
                      args <- list(...)
                      args$timeout = timeout
                      args$scaling = c("range", "equilibrate", "integers")
                      el <- do.call(mip$execute, args)
                      if (!isTRUE(el$solution)){
                        dump_path <- file.path(
                          tempdir(),
                          paste0("no_solution_record_", r, ".mps")
                        )
                        mip$write_lp( dump_path,type="mps")
                        warning( "dumping lp problem for record ", r
                                 , " in '", dump_path, "'"
                                 , call. = FALSE
                        )
                      }
                    }

                    # remove lp object, too memory hungry...
                    el$lp <- NULL
                    el$duration <- Sys.time() - starttime
                    el$row <- r
                    progress()
                    el
                  }
                  show_progress <- interactive() && n_invalid > 0

                  Ncpus <- min(Ncpus, n_invalid)
                  setup_cluster <- Ncpus > 1 && is.null(cl) && n_invalid > 1

                  if (setup_cluster){
                    cl <- parallel::makeCluster(Ncpus, setup_strategy = "sequential")
                    on.exit(parallel::stopCluster(cl))
                  }

                  sols <- if (is.null(cl)){ # sequential
                    progress <- invisible
                    if (show_progress) {
                      pb <- txtProgressBar(min=0, max = n_invalid, style=3)
                      e <- new.env(parent = emptyenv())
                      e$val <- 0
                      e$lasttime <- Sys.time()

                      progress <- function(){
                        e$val <- e$val + 1
                        if (Sys.time() - e$lasttime > 1){
                          setTxtProgressBar(pb, e$val)
                          e$lasttime <- Sys.time()
                        }
                      }
                    }
                    sols <- lapply(rows[invalid], solve_record, progress = progress)
                    if (show_progress){
                      setTxtProgressBar(pb, n_invalid)
                      close(pb)
                    }
                    sols
                  } else if (is.numeric(cl)){
                    message("starting parallel: ", cl, " cores (non-Windows)")
                    parallel::mclapply( rows[invalid]
                                        , solve_record
                                        , mc.cores = cl
                    )
                  } else if (inherits(cl, "cluster")){
                    message("Setting up ", class(cl)[1], " job with ", length(cl)," nodes")
                    parallel::clusterEvalQ(cl, library(errorlocate))
                    parallel::clusterExport( cl
                                             , c("data", "weights", "mip")
                                             , envir = environment()
                    )
                    message("Starting cluster job...")
                    # parallel::parLapplyLB(cl, rows[invalid], solve_record)
                    parallel::parLapplyLB(cl, rows[invalid], solve_record)
                  }
                  # collect info during processing
                  status <- integer(N)
                  duration <- numeric(N)
                  solution <- logical(N)
                  solution[] <- TRUE

                  adapt <- matrix( FALSE
                                   , nrow = nr_rows
                                   , ncol = nr_cols + length(operations)
                                   , dimnames = list(NULL, c(names_cols, names(operations)))
                  )

                  for (sol in sols){
                    r <- sol$row
                    idx <- match(names(sol$adapt), c(names_cols, names(operations)))
                    adapt[r,idx] <-  sol$adapt
                    duration[r] <- sol$duration
                    solution[r] <- sol$solution
                    status[r] <- sol$s
                  }

                  wpr <- cbind(weight, weight_eo)
                  wpr[!adapt | is.na(adapt)] <- 0

                  weight_per_record <- rowSums(wpr, na.rm=T)

                  if (any(!solution)){
                    warning("For some records the procedure was unsuccesful, "
                            , "please check the '$solution' and '$status' of the errorlocations.\n"
                            , "Records: "
                            , paste0(which(!solution), collapse = " ,")
                            , call. = FALSE
                    )
                  }

                  is.na(adapt)[ ,(1:nr_cols)] <- is.na(data)

                  errorlocate:::create_errorlocation(
                    values = adapt,
                    weight = weight_per_record,
                    duration = duration,
                    status = status,
                    solution = solution
                  )
                }
              )
  )


# extension of the errorlocate::locate_errors method for data = data.frame and x = validator

locate_errors_eo <- function(data, x, operations = list(), ..., cl = NULL,
                             Ncpus = getOption("Ncpus", 1), timeout = 60) {
  .local <- function (data, x, operations, weight = NULL,
                      ref = NULL, ..., cl = NULL,
                      Ncpus = getOption("Ncpus", 1), timeout = 60)
  {
    eo <- eo_localizer(rules = x, operations = operations)
    locate_errors(data = data, eo, ref = ref, weight = weight,
                  ..., cl = cl, Ncpus = Ncpus, timeout = timeout)
  }
  .local(data, x, operations, ..., cl = NULL,
         Ncpus = getOption("Ncpus", 1), timeout = timeout)
}



# extension of the errorlocate::replace_errors method for data = data.frame and x = errorlocation
# note: option of not replacing errors by NAs in original replace_errors function is removed here

replace_errors_eo <- function(data, x, operations = list(), ref = NULL, ..., cl = NULL,
                              Ncpus = 1) {

  V <- values(x, na_as_error = TRUE)

  if (length(operations) == 0) { # no special edit operations

    is.na(data) <- V

  } else { # special edit operations included

    Vdat <- V[ , names(data), drop = FALSE]
    dataorig <- data

    for (op in names(operations)) { # treat each special edit operation in turn

      opinfo <- operations[[op]]

      R <- which(V[ , op]) # records in which this operation has been selected
      vtarg <- opinfo$varTarget # variables that are affected by this operation
      vsource <- intersect(names(opinfo), names(dataorig)) # variables that affect this operation

      # replace affected variables by fixed part of edit operation
      data[R, vtarg] <- data.matrix(dataorig[R, vtarg]) +
        data.matrix(dataorig[R, vsource, drop = FALSE]) %*%
        t(data.matrix(opinfo[ , vsource, drop=FALSE]))
      # (except when the affected variable has also been selected to be imputed on its own!)
      is.na(data[R, vtarg]) <- Vdat[R, vtarg, drop = FALSE]

      # if the edit operation contains free parameters, add these to data
      freevars <- setdiff(names(opinfo), c(names(dataorig), 'varTarget'))
      if (length(freevars) > 0) {

        if (exists('freedata')) {
          freevarsnew <- setdiff(freevars, names(freedata))
        } else {
          freevarsnew <- freevars
        }

        if (length(freevarsnew) > 0) {
          freedatanew <- matrix(0, nrow = nrow(data), ncol = length(freevarsnew))
          freedatanew <- as.data.frame(freedatanew)
          names(freedatanew) <- freevarsnew
          if (exists('freedata')) {
            freedata <- cbind(freedata, freedatanew)
          } else {
            freedata <- freedatanew
          }
        }

        freedata[R, freevars] <- NA_real_

        # add edit restrictions to store the relationship between target variables and free parameters
        # restrictions of the form "target variable == fixed part + free part"
        # (except when the target variable has also been selected to be imputed on its own!)
        freepart <- sapply(vtarg, function(v) paste(paste0(opinfo[opinfo$varTarget == v, freevars],
                                                           ' * ', freevars), collapse = ' + '))
        add <- sapply(R, function(r) paste(sapply(vtarg[!Vdat[r,vtarg]],
                                                  function(v) paste0(v, ' == ', data[r, v],
                                                                     ' + ', freepart[v])), collapse = ', '))

        if (!exists('addEdits')) {
          addEdits <- data.frame(addEdits = rep('', nrow(data)), stringsAsFactors = FALSE)
        }
        addEdits[R, 'addEdits'] <- paste0(addEdits[R, 'addEdits'], ', ', add)

        data[R, vtarg] <- NA # now set affected variables equal to NA so they can be imputed
      }

    }

    # tidying up expressions of edit restrictions
    if (exists('addEdits')) {
      addEdits$addEdits <- gsub(pattern = '^, ', replacement = '', x = addEdits$addEdits)
      addEdits$addEdits <- gsub(pattern = '[+] [-]', replacement = '-', x = addEdits$addEdits)
    }

    is.na(data) <- Vdat
    if (exists('freedata')) data <- cbind(data, freedata, addEdits)

  }

  attr(data, "errorlocation") <- x
  data
}


# wrapper that calls deductive::impute_lr function for blocks of records in data
# where each block has the same set of augmented validation rules

impute_lr_eo <- function(dat, x, ...) {

  if ('addEdits' %in% names(dat)) {
    strat <- unique(dat$addEdits)
    R <- lapply(strat, function(s) which(dat$addEdits == s))
  } else {
    strat <- ''
    R <- list(1:nrow(dat))
  }

  for (s in 1:length(R)) {
    if (strat[s] == '') {
      xs <- x
    } else {
      xs <- x + eval(parse(text = sprintf('validator(%s)', strat[s])))
    }
    dat[R[[s]], ] <- impute_lr(dat = dat[R[[s]], ], x = xs, ...)
  }

  dat

}



# wrapper that calls rspa::match_restriction function for blocks of records in data
# where each block has the same set of augmented validation rules

match_restrictions_eo <- function(dat, restrictions, adjust, weight,
          remove_tag = TRUE, keepvars, ...) {

  stopifnot(inherits(dat, "data.frame"))
  stopifnot(inherits(restrictions, "validator"))

  leps <- 1e-8

  if (missing(keepvars)) {
    vars <- variables(restrictions)
  } else {
    vars <- keepvars
  }

  if (missing(adjust)) {
    adjust <- tagged_values(dat)[ , vars, drop = FALSE]
    if (is.null(adjust))
      adjust <- array(TRUE, dim = dim(dat[ , vars, drop = FALSE]))
  }

  if (missing(weight)) {
    weight <- rep(1, length(vars))
  }
  if (is.vector(weight)) {
    stopifnot(length(weight) == length(vars))
    weight <- matrix(rep(weight, times = nrow(dat)), byrow = TRUE,
                     nrow = nrow(dat))
  }
  else {
    stopifnot(ncol(weight) == length(vars))
  }
  colnames(weight) <- vars
  stopifnot(all(dim(adjust) == dim(dat[ , vars, drop = FALSE])))
  colnames(adjust) <- vars

  if ('addEdits' %in% names(dat)) {
    strat <- unique(dat$addEdits)
    R <- lapply(strat, function(s) which(dat$addEdits == s))
  } else {
    strat <- ''
    R <- list(1:nrow(dat))
  }

  for (s in 1:length(R)) {
    if (strat[s] == '') {
      x <- restrictions
      varsadd <- NULL
      a <- adjust[R[[s]], , drop = FALSE]
      w <- weight[R[[s]], , drop = FALSE]
    } else {
      xadd <- eval(parse(text = sprintf('validator(%s)', strat[s])))
      x <- restrictions + xadd
      varsadd <- setdiff(variables(xadd), vars)
      a <- cbind(adjust[R[[s]], , drop = FALSE],
                 matrix(TRUE, nrow = length(R[[s]]), ncol = length(varsadd)))
      colnames(a) <- c(colnames(adjust), varsadd)
      w <- cbind(weight[R[[s]], , drop = FALSE],
                 matrix(leps, nrow = length(R[[s]]), ncol = length(varsadd)))
      colnames(w) <- c(colnames(weight), varsadd)
    }

    dat[R[[s]], c(vars, varsadd)] <- match_restrictions(dat = dat[R[[s]], c(vars, varsadd), drop = FALSE],
                                                        restrictions = x,
                                                        adjust = a, weight = w,
                                                        remove_tag = FALSE, ...)
  }

  if (remove_tag)
    remove_tag(dat)[ , vars, drop = FALSE]
  else dat[ , vars, drop = FALSE]

}

