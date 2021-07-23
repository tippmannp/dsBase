glmDS2s <- function(variables_string, family, beta.vect, offset, weights, dataName) {
    
    res = c()
    
    wantedlabels_y = sapply(
        strsplit(
            variables_string,
            split = ',',
            fixed = T
        ),
        identity
    )
    
    n = length(wantedlabels_y)
    
    wantedlabels = wantedlabels_y[1:n-1]
    y = wantedlabels_y[n]
    
    formula_strings = c()
    
    # iterate through all variables
    for (i in 1:length(wantedlabels)) {
        # variable name
        label_i = wantedlabels[i]
        
        # calculate y ~ 1 + label_i estimate
        
        # qualify variable names with table name
        var_x = paste('D', y, sep='$')
        var_y = paste('D', label_i, sep='$')
        # formula
        myformula = paste(var_y, var_x, sep='~')
        
        formula_strings = append(formula_strings, myformula)
    }
    
    formulas = lapply(
        formula_strings,
        stats::as.formula
    )
    
    #formulas = lapply(
    #    sapply(
    #        strsplit(
    #            formulas_string,
    #            split='|',
    #            fixed=T
    #        ),
    #        identity
    #    ),
    #    stats::as.formula
    #)
    
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
