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
filings <- function() {
    login()
    response <- httr::POST("https://www.calcbench.com/api/filingsV2",
        body = list(
            companiesParameters = list(
                entireUniverse = TRUE
            ),
            periodParameters = list(
                dateRange = list(
                    endDate = "2021-10-12",
                    startDate = "2021-10-10"
                ),
                periodType = "dateRange"
            ),
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