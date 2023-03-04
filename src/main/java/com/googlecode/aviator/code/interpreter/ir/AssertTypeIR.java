package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;

public class AssertTypeIR implements IR {

  private static final long serialVersionUID = -856359371027702741L;

  public static enum AssertTypes {
    Number, String, Bool,
  }

  private final AssertTypes type;

  public AssertTypeIR(final AssertTypes type) {
    super();
    this.type = type;
  }

  @Override
  public void eval(final InterpretContext context) {
    switch (this.type) {
      case Bool:
        context.peek().booleanValue(context.getEnv());
        break;
      case Number:
        context.peek().numberValue(context.getEnv());
        break;
      case String:
        context.peek().stringValue(context.getEnv());
        break;
    }
    context.dispatch();
  }

  @Override
  public String toString() {
    return "assert " + this.type.name().toLowerCase();
  }

}
