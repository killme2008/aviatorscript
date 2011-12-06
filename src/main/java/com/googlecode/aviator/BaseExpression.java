package com.googlecode.aviator;

import java.util.List;
import java.util.Set;


/**
 * Base expression
 * 
 * @author dennis
 * 
 */
public abstract class BaseExpression implements Expression {

    private List<String> varNames;


    public BaseExpression(List<String> varNames) {
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
    public List<String> getVariableNames() {
        return this.varNames;
    }

}
