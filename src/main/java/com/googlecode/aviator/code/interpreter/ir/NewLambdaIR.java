package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.interpreter.Context;
import com.googlecode.aviator.code.interpreter.IR;

public class NewLambdaIR implements IR {

  private final String lambdaName;



  public NewLambdaIR(final String lambdaName) {
    super();
    this.lambdaName = lambdaName;
  }



  @Override
  public void eval(final Context context) {
    context.push(context.getExpression().newLambda(context.getEnv(), this.lambdaName));
  }

}
