package com.googlecode.aviator.runtime;

import java.io.Serializable;

/**
 * Function param
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class FunctionParam implements Serializable {
  private static final long serialVersionUID = 2500321752781875680L;
  private final int index;
  private final String name;
  private final boolean isVariadic;



  public FunctionParam(final int index, final String name, final boolean isVariadic) {
    super();
    this.index = index;
    this.name = name;
    this.isVariadic = isVariadic;
  }

  public int getIndex() {
    return this.index;
  }

  public String getName() {
    return this.name;
  }

  public boolean isVariadic() {
    return this.isVariadic;
  }

  @Override
  public String toString() {
    return "FunctionParam [index=" + this.index + ", name=" + this.name + ", isVariadic="
        + this.isVariadic + "]";
  }


}
