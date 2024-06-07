package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;

public class GotoIR implements IR, JumpIR {
  private static final long serialVersionUID = 5095135626036497287L;
  private int pc;
  private final Label label;
  private final SourceInfo sourceInfo;


  public GotoIR(final Label label, final SourceInfo sourceInfo) {
    super();
    this.label = label;
    this.sourceInfo = sourceInfo;
  }

  public int getPc() {
    return this.pc;
  }

  @Override
  public void setPc(final int pc) {
    this.pc = pc;
  }

  @Override
  public Label getLabel() {
    return this.label;
  }

  @Override
  public boolean mayBeCost() {
    return true;
  }

  @Override
  public void eval(final InterpretContext context) {
    context.jumpTo(this.pc);
    context.dispatch(false);
  }

  @Override
  public String toString() {
    return "goto " + this.pc + "  [" + this.label + "]      " + this.sourceInfo;
  }

}
