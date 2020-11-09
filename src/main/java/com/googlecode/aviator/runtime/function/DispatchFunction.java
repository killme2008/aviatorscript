package com.googlecode.aviator.runtime.function;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.TreeMap;
import com.googlecode.aviator.exception.FunctionNotFoundException;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * Dispatch function by argument arity.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class DispatchFunction extends AbstractVariadicFunction {

  private static final Object[] EMPTY_VAR_ARGS = new Object[0];

  private static final long serialVersionUID = 5993768652338524385L;

  private final IdentityHashMap<Integer, LambdaFunction> functions = new IdentityHashMap<>();

  private final TreeMap<Integer, LambdaFunction> variadicFunctions = new TreeMap<>();

  private final String name;

  public DispatchFunction(final String name) {
    super();
    this.name = name;
  }

  @Override
  public String getName() {
    return this.name;
  }

  public void install(final LambdaFunction fn) {
    if (fn.isVariadic()) {
      this.variadicFunctions.put(fn.getArity(), fn);
    } else {
      this.functions.put(fn.getArity(), fn);
    }
  }

  @Override
  public AviatorObject variadicCall(final Map<String, Object> env, AviatorObject... args) {
    final int arity = args.length;
    LambdaFunction fn = this.functions.get(arity);

    if (fn == null) {
      // 1. try to retrieve the variadic by arity+1
      fn = this.variadicFunctions.get(arity + 1);

      if (fn == null) {
        Map.Entry<Integer, LambdaFunction> entry = this.variadicFunctions.floorEntry(arity);
        if (entry != null) {
          fn = entry.getValue();
        }
      }

      if (fn == null) {
        throw new FunctionNotFoundException(
            "Function `" + this.name + "` with args(" + arity + ") not found");
      }
    }
    assert (args.length + 1 >= arity);

    if (fn.isVariadic()) {
      if (arity + 1 == fn.getArity()) {
        AviatorObject[] newArgs = new AviatorObject[arity + 1];
        System.arraycopy(args, 0, newArgs, 0, arity);
        newArgs[arity] = AviatorRuntimeJavaType.valueOf(EMPTY_VAR_ARGS);

        args = newArgs;
      } else {
        AviatorObject[] newArgs = new AviatorObject[fn.getArity()];
        System.arraycopy(args, 0, newArgs, 0, fn.getArity() - 1);
        AviatorObject[] varArgs = new AviatorObject[arity - fn.getArity() + 1];
        System.arraycopy(args, fn.getArity() - 1, varArgs, 0, varArgs.length);
        newArgs[fn.getArity() - 1] = AviatorRuntimeJavaType.valueOf(varArgs);

        args = newArgs;
      }
    }

    switch (fn.getArity()) {
      case 0:
        return fn.call(env);
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
        throw new IllegalArgumentException(
            "Wrong number of args (" + arity + ") passed to: " + this.name);
    }

  }

}
