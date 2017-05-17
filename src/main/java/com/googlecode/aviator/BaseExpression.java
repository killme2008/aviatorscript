package com.googlecode.aviator;

import java.util.ArrayList;
import java.util.LinkedHashSet;
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
    private List<String> varFullNames;


    public BaseExpression(List<String> varNames) {
        super();
        this.varFullNames = varNames;
        LinkedHashSet<String> tmp = new LinkedHashSet<String>(varNames.size());
        // process nested names
        for (String name : varNames) {
            if (name.contains(".")) {
                name = name.substring(0, name.indexOf("."));
            }
            tmp.add(name);
        }
        this.varNames = new ArrayList<String>(tmp);
    }


    /*
     * (non-Javadoc)
     * 
     * @see com.googlecode.aviator.IExpression#execute()
     */
    public Object execute() {
        return this.execute(null);
    }


    public List<String> getVariableFullNames() {
        return varFullNames;
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
