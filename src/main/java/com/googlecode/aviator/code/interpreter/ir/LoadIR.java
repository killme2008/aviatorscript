package com.googlecode.aviator.code.interpreter.ir;


import com.googlecode.aviator.code.interpreter.Context;
import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.token.NumberToken;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.runtime.type.AviatorBigInt;
import com.googlecode.aviator.runtime.type.AviatorDecimal;
import com.googlecode.aviator.runtime.type.AviatorDouble;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorPattern;
import com.googlecode.aviator.runtime.type.AviatorString;
import com.googlecode.aviator.utils.TypeUtils;

/**
 * load an operand
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class LoadIR implements IR {
  private final Token<?> token;

  public LoadIR(final Token<?> token) {
    super();
    this.token = token;
  }

  @Override
  public void eval(final Context context) {
    if (this.token == null) {
      return;
    }

    // load token to stack
    switch (this.token.getType()) {
      case Number:
        // load numbers
        NumberToken numberToken = (NumberToken) this.token;
        Number number = numberToken.getNumber();

        if (TypeUtils.isBigInt(number)) {
          context.push(AviatorBigInt.valueOf(this.token.getLexeme()));
        } else if (TypeUtils.isDecimal(number)) {
          context.push(AviatorDecimal.valueOf(this.token.getLexeme()));
        } else if (TypeUtils.isDouble(number)) {
          context.push(AviatorDouble.valueOf(number.doubleValue()));
        } else {
          context.push(AviatorLong.valueOf(number.longValue()));
        }
        break;
      case String:
        context.push(new AviatorString((String) this.token.getValue(null)));
        break;
      case Pattern:
        context.push(new AviatorPattern((String) this.token.getValue(null)));
        break;
      case Variable:
        context.push(new AviatorJavaType(this.token.getLexeme()));
        break;
      default:
        throw new ExpressionRuntimeException("Can't load " + this.token);
    }

  }
}
