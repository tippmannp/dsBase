glmDS2s <- function(formulas_string, family, beta.vect, offset, weights, dataName) {
    
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
            glmDS2(
                formula,
                family,
                beta.vect,
                offset,
                weights,
                dataName
            )
        )        
    }
    
    return(res)
    
}
# AGGREGATE FUNCTION
# glmDS2s