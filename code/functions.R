my_geeglm <- function(formula, data, id, ...) {
  id <- rlang::enexpr(id)
  
  # keep compelte cases amoung the variables needed in the model
  data <-
    select(data, all_of(all.vars(formula)), !!id) %>%
    dplyr::filter(complete.cases(.))
  
  # build GEE model
  rlang::inject(
    geepack::geeglm(
      formula = formula,
      data = data, 
      id = !!id,
      ...
    )
  )
}
