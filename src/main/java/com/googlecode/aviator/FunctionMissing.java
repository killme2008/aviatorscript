package com.googlecode.aviator;

import com.googlecode.aviator.runtime.type.AviatorFunction;

/**
 * Function missing callback when function not found.
 * 
 * @author dennis
 *
 */
public interface FunctionMissing {

  /**
   * Invoked when function not found
   * 
   * @param name function name
   */
  public AviatorFunction onFunctionMissing(String name);
}
