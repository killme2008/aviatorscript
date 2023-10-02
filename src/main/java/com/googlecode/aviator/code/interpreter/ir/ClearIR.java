package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;

public class ClearIR implements IR {

  private static final long serialVersionUID = -486328244006736142L;

  private ClearIR() {};

  public static final ClearIR INSTANCE = new ClearIR();

  @Override
  public void eval(final InterpretContext context) {
    context.clearStack();
    context.dispatch();
  }

  @Override
  public String toString() {
    return "clear";
  }

}
