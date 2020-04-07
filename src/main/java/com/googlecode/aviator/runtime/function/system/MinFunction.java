package com.googlecode.aviator.runtime.function.system;

/**
 * min function to find the smallest element in arguments.
 * 
 * @author dennis
 *
 */
public class MinFunction extends AbstractMinMaxFunction {


  private static final long serialVersionUID = -3048977411022985044L;

  @Override
  public String getName() {
    return "min";
  }

  @Override
  protected Op getOp() {
    return Op.Min;
  }

}
