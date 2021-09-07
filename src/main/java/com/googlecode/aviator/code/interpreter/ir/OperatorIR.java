package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.interpreter.Context;
import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * OperatorType IR
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class OperatorIR implements IR {
  private final OperatorType op;

  public OperatorIR(final OperatorType op) {
    super();
    this.op = op;
  }

  @Override
  public void eval(final Context context) {
    int arity = this.op.getArity();
    // TODO func call

    AviatorObject[] args = new AviatorObject[arity];
    for (int i = args.length - 1; i >= 0; i--) {
      args[i] = context.pop();
    }

    context.push(this.op.eval(args, context.getEnv()));
  }

}
