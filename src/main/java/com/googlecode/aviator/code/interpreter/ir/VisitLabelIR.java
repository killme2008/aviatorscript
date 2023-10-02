package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;

public class VisitLabelIR implements IR {
  private static final long serialVersionUID = 5535667281818822262L;
  private final Label label;

  public VisitLabelIR(final Label label) {
    super();
    this.label = label;
  }

  public Label getLabel() {
    return this.label;
  }

  @Override
  public void eval(final InterpretContext context) {
    throw new UnsupportedOperationException();
  }

  @Override
  public String toString() {
    return this.label + ":";
  }
}
