package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;

/**
 * type(x) function return the type of x, the result is a string such as 'string', 'long', 'double'
 * etc.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class TypeFunction extends AbstractFunction {


  private static final long serialVersionUID = 501798543495705123L;
  private static final AviatorString STRING_TYPE = new AviatorString("string", true);
  private static final AviatorString PATTERN_TYPE = new AviatorString("pattern", true);
  private static final AviatorString RANGE_TYPE = new AviatorString("range", true);
  private static final AviatorString NIL_TYPE = new AviatorString("nil", true);
  private static final AviatorString LONG_TYPE = new AviatorString("long", true);
  private static final AviatorString FUNC_TYPE = new AviatorString("function", true);
  private static final AviatorString DOUBLE_TYPE = new AviatorString("double", true);
  private static final AviatorString DECIMAL_TYPE = new AviatorString("decimal", true);
  private static final AviatorString BOOL_TYPE = new AviatorString("boolean", true);
  private static final AviatorString BIGINT_TYPE = new AviatorString("bigint", true);

  @Override
  public String getName() {
    return "type";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, AviatorObject arg1) {
    boolean retry = false;
    while (true) {
      switch (arg1.getAviatorType()) {
        case BigInt:
          return BIGINT_TYPE;
        case Boolean:
          return BOOL_TYPE;
        case Decimal:
          return DECIMAL_TYPE;
        case Double:
          return DOUBLE_TYPE;
        case Lambda:
        case Method:
          return FUNC_TYPE;
        case Long:
          return LONG_TYPE;
        case Nil:
          return NIL_TYPE;
        case Range:
          return RANGE_TYPE;
        case Pattern:
          return PATTERN_TYPE;
        case String:
          return STRING_TYPE;
        case JavaType:
          if (retry) {
            return new AviatorString(arg1.getValue(env).getClass().getCanonicalName());
          }
          arg1 = FunctionUtils.wrapReturn(arg1.getValue(env));
          retry = true;
          break;
      }
    }
  }
}
