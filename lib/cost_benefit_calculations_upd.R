## Cost-Benefit Calculations

## Reserve Fund Calculation 



cost_reserve_fund <- function(reserve.fund.losses,bartheta1,param_i,param_r,param_d) {
  
    cost <- sapply(reserve.fund.losses, function(x)
                    ((param_i - param_r)/(1 + param_d)) * ( bartheta1 - x) + x)
    cost <- unlist(cost)
    
    return(cost)
    
    
}


cost_post_budget <- function(budget.reallocation.losses,param_h,param_d) {
  
    cost <- sapply(budget.reallocation.losses, function(x)
                    (1 + param_h) / ( 1 + param_d) * x)
    cost <- unlist(cost)
    
    return(cost)
  
}


cost_insurance <- function(insurance.layer.aal, param_m) {
  
    cost <- sapply(insurance.layer.aal, function(x)
                    x * param_m)
    cost <- unlist(cost)
    
    return(cost)
}


cost_borrowing <- function(borrowing.losses,param_b,param_d,param_t,param_e,param_n) {
  
  a_nd <- (1 - (1 + param_d)^(-1*param_n)) / param_d
  
  a_ne <- (1 - (1 + param_e)^-(1*param_n)) / param_e
  
  k <-  ((1 + param_b)/ (1 + param_d))^(param_t/12) * a_nd/a_ne
  
  cost <- sapply(borrowing.losses, function(x)
                                   k * x)
  

  cost <- unlist(cost)
  
  return(cost)
}


cost_contingent_credit <- function(contingent.credit.losses,bar_theta2,param_delta,param_p,param_d,param_c) {
  
  a_pd <- (1 - (1 + param_d)^(-1*param_p)) / param_d
  
  a_pc <- (1 - (1 + param_c)^(-1*param_p)) / param_c
  
  cost <- sapply(contingent.credit.losses, function(x)
                                          param_delta*bar_theta2 + (a_pd/a_pc)*x )
  
  return(cost)
  
}
  

