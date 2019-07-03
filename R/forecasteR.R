#' Estimate statistical models on data based on a given list of relationships
#'
#' \code{estimateStatisticalModels} returns a list of statistical models.
#'
#' @inheritParams forecast
#' @param formulas A list of formulas (e.g., list(x = price ~ weather + month, y = demand ~ price + income))
#' @return A formula
#' @export

estimateModels <- function(data, formulas)
  Map(function(f)
    stats::lm(data = data, formula = f), formulas)

#' Turn an estimated model into a formula
#'
#' \code{modelToFormula} returns a formula with coefficients placed as factors.
#'
#' @param model An estimated model which includes the original call and the estimated coefficients
#' @return A formula
#' @export

modelToFormula <- function(model) {
  frm = stats::formula(model)
  coe = stats::coefficients(model)

  leftSide = frm[[2]]
  rightSide = paste(paste(coe, names(coe), sep = '*'), collapse = '+')

  return(stats::as.formula(paste(leftSide, rightSide, sep = '~')))
}

solveModel <-
  function(endogenous,
           exogenous,
           model,
           solver,
           lowerBounds = NULL,
           upperBounds = NULL) {
    # get a list of determined variables (have a definite value rather than NA) these variables do not need to be solved for and their
    # corresponding equations will be removed
    determinedVariables = names(endogenous[is.finite(endogenous)])

    # exclude equations that are not needed when an equation name equals a determined variable, it is excluded

    modifiedModel = list()
    for (m in names(model)) {
      if (!is.element(m, determinedVariables)) {
        modifiedModel[[m]] = model[[m]]
      }
    }

    exogenous = c(exogenous, endogenous[determinedVariables])

    # modifiedModel = model
    unknownVariables = endogenous[!is.finite(endogenous)]

    # set unknown (NA) variables to some starting value =1
    unknownVariables[] = 1


    unknownVariables =     solver(
      formulas = modifiedModel,
      endogenous = unknownVariables,
      exogenous = exogenous,
      lowerBounds = lowerBounds,
      upperBounds = upperBounds
    )$results

    toReturn = endogenous
    toReturn[names(unknownVariables)] =  unknownVariables[names(unknownVariables)]

    return(toReturn)
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
           transformation = NULL,
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
      if (is.null(transformation)) {
        currentData = data
      } else {
        currentData = transformation(data)
      }


      currentRow = which(currentData[, periodColumn] == p)[1]


      solvedModel = solveModel(
        endogenous = unlist(currentData[currentRow, endogenousVariableNames, drop = F]),
        exogenous = unlist(currentData[currentRow, setdiff(modelVariables, endogenousVariableNames), drop =
                                         F]),
        model = model,
        lowerBounds = lowerBounds,
        upperBounds = upperBounds,
        solver = solver
      )

      data[currentRow, endogenousVariableNames] = solvedModel[endogenousVariableNames]

    }
    return(data)
  }
