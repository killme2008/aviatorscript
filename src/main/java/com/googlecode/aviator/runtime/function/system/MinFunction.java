package com.googlecode.aviator.runtime.function.system;

/**
 * min function to find the smallest element in arguments.
 * 
 * @author dennis
 *
 */
public class MinFunction extends AbstractMinMaxFunction {

  @Override
  public String getName() {
    return "min";
  }

  @Override
  protected Op getOp() {
    return Op.Min;
  }

}
