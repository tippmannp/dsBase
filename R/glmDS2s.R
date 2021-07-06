glmDS2s <- function (formulas, family, beta.vect, offset, weights, dataName) {
    res = c()
    
    for (formula in formulas) {
        res = c(
            res,
            glmDS2(formula, family, beta.vect, offset, weights, dataName)
        )        
    }
    
    return(res)
}
# glmDS2s