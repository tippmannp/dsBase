glmDS1s <- function(formulas_string, family, weights, offset, data) {
    
    res = c()
    
    
    n = length(formulas_string)
    
    wantedlabels = formulas_string[1:n-1]
    y = formulas_string[n]
    
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
        
        formulas = append(formulas, myformula)
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