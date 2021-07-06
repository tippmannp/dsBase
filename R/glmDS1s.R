glmDS1s <- function(formulas, family, weights, offset, data){
    res = c()
    
    for (formula in formulas) {
        res = c(
            res,
            glmDS1(formula, family, weights, offset, data)
        )        
    }
    
    return(res)
}
# glmDS1s