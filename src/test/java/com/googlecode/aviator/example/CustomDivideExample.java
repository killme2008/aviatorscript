package com.googlecode.aviator.example;

import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorDouble;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorType;

/**
 * An example to demo custom division operator.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class CustomDivideExample {
  public static void main(final String[] args) {
    AviatorEvaluator.getInstance().addOpFunction(OperatorType.DIV, new AbstractFunction() {



      @Override
      public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
          final AviatorObject arg2) {
        if (arg1.getAviatorType() == AviatorType.Long
            && arg2.getAviatorType() == AviatorType.Long) {
          // If arg1 and arg2 are all long type.
          // Cast arg2 into double and divided by arg1.
          double d = FunctionUtils.getNumberValue(arg1, env).longValue()
              / FunctionUtils.getNumberValue(arg2, env).doubleValue();
          return AviatorDouble.valueOf(d);
        } else {
          // Otherwise, call aviatorscript's div function.
          return arg1.div(arg2, env);
        }
      }

      @Override
      public String getName() {
        return OperatorType.DIV.getToken();
      }
    });

    System.out.println(AviatorEvaluator.execute("1/2"));
  }
}
