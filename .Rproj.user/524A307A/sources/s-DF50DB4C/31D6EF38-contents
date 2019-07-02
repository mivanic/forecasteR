solveModel <-
  function(endogenous,
           exogenous,
           model,
           solver,
           lowerBounds = NULL,
           upperBounds = NULL) {
    # get a list of determined variables (have a definite value rather than NA) these variables do not need to be solved for and their
    # corresponding equations will be removed
    determinedVariables = names(vector[is.finite(vector)])

    # exclude equations that are not needed when an equation name equals a determined variable, it is excluded

    modifiedModel = list()
    for (m in names(model)) {
      if (!is.element(m, determinedVariables)) {
        modifiedModel[[m]] = model[[m]]
      }
    }

    exogenous = c(exogenous, vector[determinedVariables])

    # modifiedModel = model
    unknownVariables = vector[!is.finite(vector)]

    # set unknown (NA) variables to some starting value =1
    unknownVariables[] = 1


    unknownVariables =     solver(
      model = model,
      endogenous = endogenous,
      exogenous = exogenous,
      lowerBounds = lowerBounds,
      upperBounds = upperBounds
    )$results

    return(unknownVariables)
  }

#' Forecast missing values based on a model (set of equations).
#'
#' \code{forecast} returns the dataframe after the the missing values have been filled using the provided model.
#'
#' @param data A dataframe with some blanks in endogenous variables, and a period column
#' @param model A list of equations specifying the relationships among endogenous variables. The names of equations specify endogenous variables that they solve.
#' @param periods For which periods should the simulation be run?
#' @param periodColumn The name of the column with the period values
#' @param transformation A function specifying how the data need to be transformed post each simulation (e.g., define lagged variables)
#' @param lowerBounds An optional list of lower bounds for endogenous variables
#' @param upperBounds An optional list of upper bounds for endogenous variables
#' @param solver A function that can solve a system of equations (accepts model, endogenous, exogenous and return list with "result" as a list of the same structure as endogenous)
#' @return A data frame
#' @export


forecast <-
  function(data,
           model,
           periods,
           periodColumn,
           transformation,
           lowerBounds,
           upperBounds,
           solver) {
    # get all variables referenced in the model
    modelVariables = unique(Reduce(function(ac, ad)
      c(ac, all.vars(ad)), model, c()))

    # get endogenous variables
    endogenousVariableNames = names(model)

    # execute for each required period
    for (p in periods) {
      currentData = transformation(data)

      currentRow = which(currentData[, periodColumn] == p)[1]

      solvedModel = solveModel(
        endogenous = data[currentRow, endogenousVariableNames],
        exogenous = data[currentRow, setdiff(modelVariables, endogenousVariableNames)],
        model = model,
        lowerBounds = lowerBounds,
        upperBounds = upperBounds,
        solver = solver
      )

      data[currentRow, endogenousVariableNames] = solvedModel[endogenousVariableNames]

    }
    return(data)
  }
