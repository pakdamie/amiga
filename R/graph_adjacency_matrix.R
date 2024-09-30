#' Plot the adjacency matrix
#'
#' Primitive plotting function as of right now on what your system of equations
#' look like right now
#'
#' @param adjacency_matrix
#'
#' @return ggplot2 object that shows the states as rectangles AND how they
#' are connected or not.
#' @export
#'
#' @examples graph_adjacency_matrix(adj_mat)
graph_adjacency_matrix <- function(adjacency_matrix) {

  number_of_nodes <- ncol(adjacency_matrix)

  x_middle <- seq(0, 20, length = number_of_nodes)
  y_middle <- 0

  box_positions <- data.frame(
    xmin = x_middle - 2,
    xmax = x_middle + 2,
    ymin = y_middle - 2,
    ymax = y_middle + 2,
    state = rownames(adjacency_matrix),
    number_pos = seq(1:nrow(adjacency_matrix))
  )


  edge_list <- as.data.frame(which(adjacency_matrix == 1, arr.ind = TRUE))

  edge_list1 <- left_join(edge_list, box_positions,
    by = c("row" = "number_pos")
  )

  edge_list2 <- left_join(edge_list, box_positions,
    by = c("col" = "number_pos")
  )


  full_edge_df <- data.frame(
    x = edge_list1[, "xmin"],
    xend = edge_list2[, "xmax"],
    y = y_middle,
    yend = y_middle
  )

  flow_chart_GG <- ggplot(full_edge_df) +
    geom_segment(data = full_edge_df, aes(
      x = x, xend = xend,
      y = y,
      yend = yend
    )) +
    geom_rect(
      data =   box_positions , aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      color = "black", fill = "white"
    ) +
    geom_text(
      data =  box_positions  ,
      aes(label = state, x = x_middle, y = y_middle),
      size = 10
    ) +
    coord_equal() +
    theme_void()

  return(flow_chart_GG)
}
