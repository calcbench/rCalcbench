library(httr)

login <- function() {
    response <- httr::POST("https://www.calcbench.com/account/LogOnAjax",
        body = list(
            email = Sys.getenv("CALCBENCH_USERNAME"),
            strng = Sys.getenv("CALCBENCH_PASSWORD")
        ),
        encode = "json"
    )

    response_text <- httr::content(response)

    if (response_text != "TRUE") {
        stop("Incorrect Username/password", call. = FALSE)
    }
}

#' Get filings
#' @export
filings <- function(entire_universe = NULL,
                    company_identifiers = NULL,
                    start_date = NULL,
                    end_date = NULL) {
    login()
    period_parameters <- list()
    if (!(missing(start_date) && missing(end_date))) {
        period_parameters <- c(
            periodParameters,
            list(
                periodParamters = list(
                    endDate = end_date,
                    startDate = start_date
                )
            )
        )
    }

    response <- httr::POST("https://www.calcbench.com/api/filingsV2",
        body = list(
            companiesParameters = list(
                entireUniverse = entire_universe,
                companyIdentifiers = company_identifiers
            ),
            periodParameters = period_parameters,
            pageParameters = list(
                includePerformance = FALSE
            )
        ),
        encode = "json"
    )
    parsed <- jsonlite::fromJSON(httr::content(response, "text"))
    for (column in c(
        "filing_date",
        "calcbench_accepted",
        "period_end_date",
        "calcbench_finished_load"
    )) {
        parsed[, column] <- as.POSIXct(parsed[, column],
            format = "%Y-%m-%dT%H:%M:%OS"
        )
    }
    return(parsed)
}