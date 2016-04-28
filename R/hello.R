
#' commit_df
#'
#' Query using the Google Query Language (GQL)
#'
#' @param kind dataframe name
#' @param name name column to use. Use "row.names" to use row names.
#'
#' @examples
#' data(mtcars)
#' commit_df(kind = "test")
#'
#' @return Data frame of results
#'
#' @seealso \url{https://cloud.google.com/datastore/docs/apis/gql/gql_reference} - The google query language
#' @importFrom dplyr %>%
#'
#' @export

wb_gene <- function(gene) {

  req <- httr::GET("http://www.wormbase.org/species/c_elegans/gene/pot-2",
                   config = content_type_json())
  result <- content(req)$results[[1]]

  if (result$taxonomy$species == "elegans") {
    # Continue here
    req <- httr::GET("http://www.wormbase.org/rest/widget/gene/WBGene00006763/overview",
                     config = content_type_json())

  }
}
