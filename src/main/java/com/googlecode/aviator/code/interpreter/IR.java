package com.googlecode.aviator.code.interpreter;

import java.io.Serializable;

/**
 * IR interface
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public interface IR extends Serializable {
  void eval(InterpretContext context);

  /**
   * Returns true when the IR execution cost may be expensive
   * 
   * @return
   */
  default boolean mayBeCost() {
    return false;
  }
}
