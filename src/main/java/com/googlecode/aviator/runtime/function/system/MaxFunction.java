package com.googlecode.aviator.runtime.function.system;

/**
 * max function to find the largest element in arguments.
 *
 * @author dennis
 *
 */
public class MaxFunction extends AbstractMinMaxFunction {


  private static final long serialVersionUID = -2638341290892443991L;

  @Override
  public String getName() {
    return "max";
  }

  @Override
  protected Op getOp() {
    return Op.Max;
  }

}
