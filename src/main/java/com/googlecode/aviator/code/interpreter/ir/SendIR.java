package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.asm.ASMCodeGenerator;
import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.TraceFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Env;

public class SendIR implements IR {

  private static final long serialVersionUID = 2747763526113139010L;
  private final String name;
  private final int arity;
  private final boolean unpackArgs;
  private int funcId = -1;
  private final SourceInfo sourceInfo;


  public SendIR(final String name, final int arity, final boolean unpackArgs, final int funcId,
      final SourceInfo sourceInfo) {
    super();
    this.name = name;
    this.arity = arity;
    this.unpackArgs = unpackArgs;
    this.funcId = funcId;
    this.sourceInfo = sourceInfo;
  }

  private AviatorObject callFn(final AviatorFunction fn, final AviatorObject[] args,
      final int arity, final Env env) {

    if (arity == 0) {
      return fn.call(env);
    }

    switch (arity) {
      case 1:
        return fn.call(env, args[0]);
      case 2:
        return fn.call(env, args[0], args[1]);
      case 3:
        return fn.call(env, args[0], args[1], args[2]);
      case 4:
        return fn.call(env, args[0], args[1], args[2], args[3]);
      case 5:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4]);
      case 6:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5]);
      case 7:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
      case 8:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
      case 9:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8]);
      case 10:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8], args[9]);
      case 11:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8], args[9], args[10]);
      case 12:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8], args[9], args[10], args[11]);
      case 13:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8], args[9], args[10], args[11], args[12]);
      case 14:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8], args[9], args[10], args[11], args[12], args[13]);
      case 15:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8], args[9], args[10], args[11], args[12], args[13], args[14]);
      case 16:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15]);
      case 17:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16]);
      case 18:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16],
            args[17]);
      case 19:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16],
            args[17], args[18]);
      case 20:
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16],
            args[17], args[18], args[19]);
      default:
        assert (args.length >= 20);
        AviatorObject[] remainingArgs = new AviatorObject[args.length - 20];
        System.arraycopy(args, 20, remainingArgs, 0, remainingArgs.length);
        return fn.call(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
            args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16],
            args[17], args[18], args[19], remainingArgs);
    }
  }


  @Override
  public void eval(final InterpretContext context) {
    AviatorFunction fn = null;
    if (this.name != null) {
      fn = RuntimeUtils.getFunction(context.getEnv(), this.name);
    }

    int i = this.arity;
    AviatorObject[] args = new AviatorObject[this.arity];

    while (--i >= 0) {
      args[i] = context.pop();
    }

    if (this.name == null) {
      fn = (AviatorFunction) context.pop();
    }


    if (RuntimeUtils.isTracedEval(context.getEnv())) {
      fn = TraceFunction.wrapTrace(fn);
    }

    if (this.unpackArgs) {
      fn = RuntimeUtils.unpackArgsFunction(fn);
    }

    if (this.funcId >= 0) {
      // put function arguments ref id to env.
      context.getEnv().put(ASMCodeGenerator.FUNC_ARGS_INNER_VAR, this.funcId);
    }
    context.push(callFn(fn, args, this.arity, context.getEnv()));
    context.dispatch();
  }

  @Override
  public boolean mayBeCost() {
    return true;
  }

  @Override
  public String toString() {
    return "send " + (this.name == null ? "<top>" : this.name) + ", " + this.arity + ", "
        + this.unpackArgs + "      " + this.sourceInfo;
  }

}
