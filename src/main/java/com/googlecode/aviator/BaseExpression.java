package com.googlecode.aviator;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;


/**
 * Base expression
 *
 * @author dennis
 *
 */
public abstract class BaseExpression implements Expression {

  private List<String> varNames;
  private List<String> varFullNames;
  private String expression;


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


  /**
   * Returns the expression string when turn on {@link Options#TRACE_EVAL} option, else returns
   * null.
   *
   * @return expression in string.
   */
  public String getExpression() {
    return this.expression;
  }

  public void setExpression(String expression) {
    this.expression = expression;
  }


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.IExpression#execute()
   */
  @Override
  public Object execute() {
    return this.execute(null);
  }


  @Override
  public List<String> getVariableFullNames() {
    return this.varFullNames;
  }


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.IExpression#getVariableNames()
   */
  @Override
  public List<String> getVariableNames() {
    return this.varNames;
  }

}
