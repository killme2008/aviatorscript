package com.googlecode.aviator.runtime;

/**
 * A function argument.
 *
 * @since 4.2.0
 * @author dennis(killme2008@gmail.com)
 *
 */
public class FunctionArgument {
  private final int index;
  private final String expression;

  public FunctionArgument(final int index, final String name) {
    super();
    this.index = index;
    this.expression = name;
  }

  /**
   * Returns the parameter index in function,starts from zero.
   *
   * @return
   */
  public int getIndex() {
    return this.index;
  }

  /**
   * Returns the parameter expression.
   *
   * @return
   */
  public String getExpression() {
    return this.expression;
  }

  @Override
  public String toString() {
    return "FunctionArgument [index=" + this.index + ", expression=" + this.expression + "]";
  }

  public static FunctionArgument from(final int index, final String name) {
    return new FunctionArgument(index, name);
  }
}
