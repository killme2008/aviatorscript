package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;

public class ClearIR implements IR {

  private ClearIR() {};

  public static final ClearIR INSTANCE = new ClearIR();

  @Override
  public void eval(final InterpretContext context) {
    context.clearStack();
  }

  @Override
  public String toString() {
    return "clear";
  }

}
