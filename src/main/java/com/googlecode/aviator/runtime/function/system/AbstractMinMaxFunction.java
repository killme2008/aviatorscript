package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * Abstract base class for system min/max function.
 *
 * @author dennis
 *
 */
public abstract class AbstractMinMaxFunction extends AbstractVariadicFunction {
  static enum Op {
    Min, Max
  }



  @Override
  public AviatorObject variadicCall(Map<String, Object> env, AviatorObject... args) {

    if (args == null || args.length == 0) {
      return AviatorNil.NIL;
    }

    boolean wasFirst = true;
    AviatorObject result = AviatorNil.NIL;
    for (AviatorObject obj : args) {
      result = compareObjects(env, result, obj, wasFirst);
      if (wasFirst) {
        wasFirst = false;
      }
      if (getOp() == Op.Min && result.isNull(env)) {
        break;
      }
    }

    return result;
  }


  protected abstract Op getOp();



  private AviatorObject compareObjects(Map<String, Object> env, AviatorObject result,
      AviatorObject obj, boolean wasFirst) {
    if (obj.isNull(env)) {
      switch (getOp()) {
        case Min:
          return obj;
        case Max:
          return result;
      }
    }
    if (wasFirst || compare(env, result, obj)) {
      result = obj;
    }
    return result;
  }

  private boolean compare(Map<String, Object> env, AviatorObject result, AviatorObject obj) {
    int c = obj.compare(result, env);
    switch (getOp()) {
      case Min:
        return c < 0;
      case Max:
        return c > 0;
    }
    return false;
  }
}
