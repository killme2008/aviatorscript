package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;

public class GotoIR implements IR, JumpIR {
  private int pc;
  private final Label label;


  public GotoIR(final Label label) {
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
    context.jumpTo(this.pc);
  }

  @Override
  public String toString() {
    return "goto " + this.pc + "[" + this.label + "]";
  }

}
