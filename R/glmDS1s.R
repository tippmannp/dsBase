glmDS1s <- function(formulas_string, family, weights, offset, data) {
    
    res = c()    
    
    formulas = lapply(
        sapply(
            strsplit(
                formulas_string,
                split='|',
                fixed=T
            ),
            identity
        ),
        stats::as.formula
    )
    
    for (formula in formulas) {
        res = append(
            res,
            glmDS1(
                formula,
                family,
                weights,
                offset,
                data
            )
        )        
    }
    
    return(res)
    
}
# AGGREGATE FUNCTION
# glmDS1s