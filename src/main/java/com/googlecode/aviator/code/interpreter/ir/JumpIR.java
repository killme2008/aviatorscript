package com.googlecode.aviator.code.interpreter.ir;

public interface JumpIR {
  void setPc(int pc);

  Label getLabel();
}
