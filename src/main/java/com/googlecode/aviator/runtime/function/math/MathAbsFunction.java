/**
 * Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program;
 * if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.runtime.function.math;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorBigInt;
import com.googlecode.aviator.runtime.type.AviatorDecimal;
import com.googlecode.aviator.runtime.type.AviatorDouble;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.TypeUtils;


/**
 * math.abs(d) function
 * 
 * @author dennis
 * 
 */
public class MathAbsFunction extends AbstractFunction {

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    Number number = FunctionUtils.getNumberValue(arg1, env);
    if (TypeUtils.isDecimal(number)) {
      return new AviatorDecimal(((BigDecimal) number).abs(AviatorEvaluator.getMathContext()));
    } else if (TypeUtils.isBigInt(number)) {
      return new AviatorBigInt(((BigInteger) number).abs());
    } else if (TypeUtils.isDouble(number)) {
      return new AviatorDouble(Math.abs(number.doubleValue()));
    } else {
      return AviatorLong.valueOf(Math.abs(number.longValue()));
    }
  }


  @Override
  public String getName() {
    return "math.abs";
  }

}
