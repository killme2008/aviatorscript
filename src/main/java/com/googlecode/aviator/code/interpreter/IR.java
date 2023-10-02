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
}
