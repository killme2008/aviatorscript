package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;

public class NewLambdaIR implements IR {

  private static final long serialVersionUID = 991107439815059038L;
  private final String lambdaName;



  public NewLambdaIR(final String lambdaName) {
    super();
    this.lambdaName = lambdaName;
  }



  @Override
  public void eval(final InterpretContext context) {
    context.push(context.getExpression().newLambda(context.getEnv(), this.lambdaName));
    context.dispatch();
  }

  @Override
  public String toString() {
    return "new_lambda " + this.lambdaName;
  }
}
