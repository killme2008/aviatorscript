package com.googlecode.aviator.runtime.function.system;

/**
 * max function to find the largest element in arguments.
 *
 * @author dennis
 *
 */
public class MaxFunction extends AbstractMinMaxFunction {

  @Override
  public String getName() {
    return "max";
  }

  @Override
  protected Op getOp() {
    return Op.Max;
  }

}
