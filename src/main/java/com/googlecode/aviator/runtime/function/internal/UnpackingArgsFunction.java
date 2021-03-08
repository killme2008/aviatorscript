package com.googlecode.aviator.runtime.function.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.utils.Constants;

/**
 * Unpacking arguments on runtime
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class UnpackingArgsFunction extends AbstractVariadicFunction {

  private static final long serialVersionUID = -7537763640116320630L;


  private final AviatorFunction fn;

  public UnpackingArgsFunction(final AviatorFunction fn) {
    super();
    this.fn = fn;
  }

  @Override
  public String getName() {
    return this.fn.getName();
  }

  @Override
  public AviatorObject variadicCall(final Map<String, Object> env, final AviatorObject... args) {
    assert (args.length > 0);

    List<AviatorObject> realArgList = new ArrayList<AviatorObject>(args.length + 10);

    for (AviatorObject arg : args) {
      if (arg.meta(Constants.UNPACK_ARGS) != null) {
        for (Object obj : RuntimeUtils.seq(arg.getValue(env), env)) {
          realArgList.add(AviatorRuntimeJavaType.valueOf(obj));
        }
      } else {
        realArgList.add(arg);
      }
    }

    AviatorObject[] realArgs = new AviatorObject[realArgList.size()];
    realArgs = realArgList.toArray(realArgs);

    switch (realArgs.length) {
      case 0:
        return this.fn.call(env);
      case 1:
        return this.fn.call(env, realArgs[0]);
      case 2:
        return this.fn.call(env, realArgs[0], realArgs[1]);
      case 3:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2]);
      case 4:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3]);
      case 5:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4]);
      case 6:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5]);
      case 7:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6]);
      case 8:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7]);
      case 9:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8]);
      case 10:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8], realArgs[9]);
      case 11:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8], realArgs[9], realArgs[10]);
      case 12:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8], realArgs[9], realArgs[10],
            realArgs[11]);
      case 13:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8], realArgs[9], realArgs[10],
            realArgs[11], realArgs[12]);
      case 14:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8], realArgs[9], realArgs[10],
            realArgs[11], realArgs[12], realArgs[13]);
      case 15:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8], realArgs[9], realArgs[10],
            realArgs[11], realArgs[12], realArgs[13], realArgs[14]);
      case 16:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8], realArgs[9], realArgs[10],
            realArgs[11], realArgs[12], realArgs[13], realArgs[14], realArgs[15]);
      case 17:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8], realArgs[9], realArgs[10],
            realArgs[11], realArgs[12], realArgs[13], realArgs[14], realArgs[15], realArgs[16]);
      case 18:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8], realArgs[9], realArgs[10],
            realArgs[11], realArgs[12], realArgs[13], realArgs[14], realArgs[15], realArgs[16],
            realArgs[17]);
      case 19:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8], realArgs[9], realArgs[10],
            realArgs[11], realArgs[12], realArgs[13], realArgs[14], realArgs[15], realArgs[16],
            realArgs[17], realArgs[18]);
      case 20:
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8], realArgs[9], realArgs[10],
            realArgs[11], realArgs[12], realArgs[13], realArgs[14], realArgs[15], realArgs[16],
            realArgs[17], realArgs[18], realArgs[19]);
      default:
        assert (realArgs.length >= 20);
        AviatorObject[] remainingArgs = new AviatorObject[realArgs.length - 20];
        System.arraycopy(realArgs, 20, remainingArgs, 0, remainingArgs.length);
        return this.fn.call(env, realArgs[0], realArgs[1], realArgs[2], realArgs[3], realArgs[4],
            realArgs[5], realArgs[6], realArgs[7], realArgs[8], realArgs[9], realArgs[10],
            realArgs[11], realArgs[12], realArgs[13], realArgs[14], realArgs[15], realArgs[16],
            realArgs[17], realArgs[18], realArgs[19], remainingArgs);
    }
  }
}
