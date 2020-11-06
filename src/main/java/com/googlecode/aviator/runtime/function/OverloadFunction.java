package com.googlecode.aviator.runtime.function;

import java.util.IdentityHashMap;
import java.util.Map;
import com.googlecode.aviator.exception.FunctionNotFoundException;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * Overload function
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class OverloadFunction extends AbstractVariadicFunction {

  private static final long serialVersionUID = 5993768652338524385L;

  private final IdentityHashMap<Integer, AviatorFunction> functions = new IdentityHashMap<>();

  private final String name;

  public OverloadFunction(final String name) {
    super();
    this.name = name;
  }

  @Override
  public String getName() {
    return this.name;
  }

  public void install(final int arity, final AviatorFunction fn) {
    this.functions.put(arity, fn);
  }

  @Override
  public AviatorObject variadicCall(final Map<String, Object> env, final AviatorObject... args) {
    AviatorFunction fn = this.functions.get(args.length);

    if (fn == null) {
      throw new FunctionNotFoundException(
          "Function `" + this.name + "` with args(" + args.length + ") not found");
    }

    switch (args.length) {
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
            "Wrong number of args (" + args.length + ") passed to: " + this.name);
    }

  }

}
