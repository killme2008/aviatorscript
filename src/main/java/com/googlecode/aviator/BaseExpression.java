package com.googlecode.aviator;

import java.util.Set;


/**
 * Base expression
 * 
 * @author dennis
 * 
 */
public abstract class BaseExpression implements Expression {

    private Set<String> varNames;


    public BaseExpression(Set<String> varNames) {
        super();
        this.varNames = varNames;
    }


    /*
     * (non-Javadoc)
     * 
     * @see com.googlecode.aviator.IExpression#execute()
     */
    public Object execute() {
        return this.execute(null);
    }


    /*
     * (non-Javadoc)
     * 
     * @see com.googlecode.aviator.IExpression#getVariableNames()
     */
    public Set<String> getVariableNames() {
        return this.varNames;
    }

}
