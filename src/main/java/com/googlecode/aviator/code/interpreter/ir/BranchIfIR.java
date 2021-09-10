package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.interpreter.InterpretContext;
import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.runtime.type.AviatorObject;

public class BranchIfIR implements IR, JumpIR {
  private int pc;
  private final Label label;


  public BranchIfIR(final Label label) {
    super();
    this.label = label;
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
    }
  }

  @Override
  public String toString() {
    return "branch_if " + this.pc + "[" + this.label + "]";
  }

}
