package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;
import com.googlecode.aviator.runtime.type.AviatorObject;

public class BranchIfIR implements IR, JumpIR {
  private static final long serialVersionUID = 5374232314686082568L;
  private int pc;
  private final Label label;
  private final SourceInfo sourceInfo;

  public BranchIfIR(final Label label, final SourceInfo sourceInfo) {
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
  public void eval(final InterpretContext context) {
    AviatorObject top = context.peek();
    if (top.booleanValue(context.getEnv())) {
      context.jumpTo(this.pc);
      context.dispatch(false);
    } else {
      context.dispatch();
    }
  }



  @Override
  public boolean mayBeCost() {
    return true;
  }

  @Override
  public String toString() {
    return "branch_if " + this.pc + "  [" + this.label + "]      " + this.sourceInfo;
  }

}
