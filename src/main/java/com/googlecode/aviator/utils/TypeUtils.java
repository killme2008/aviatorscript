/**
 * Copyright (C) [2010-2012] dennis zhuang (killme2008@gmail.com)
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
package com.googlecode.aviator.utils;

import java.math.BigDecimal;
import java.math.BigInteger;
import com.googlecode.aviator.AviatorEvaluator;


/**
 * Java type to aviator type utilities
 * 
 * @author boyan
 * 
 */
public class TypeUtils {

  public static final boolean isBigInt(Object value) {
    return value instanceof BigInteger;
  }


  public static final boolean isDecimal(Object value) {
    return value instanceof BigDecimal;
  }


  public static final boolean isLong(Object value) {
    return value instanceof Integer || value instanceof Long || value instanceof Byte
        || value instanceof Short;
  }


  public static final boolean isDouble(Object value) {
    return value instanceof Float || value instanceof Double;

  }


  public static final boolean isString(Object value) {
    return value instanceof String || value instanceof Character;
  }

  public static long NEWTON_METHOD_REPEATS = 10000;


  public static int comapreLong(long x, long y) {
    if (x > y)
      return 1;
    else if (x < y)
      return -1;
    else
      return 0;
  }


  /**
   * newton method to get natural logarithm
   * 
   * @param x
   * @return
   */
  public static BigDecimal ln(BigDecimal x) {
    if (x.equals(BigDecimal.ONE)) {
      return BigDecimal.ZERO;
    }

    x = x.subtract(BigDecimal.ONE);
    BigDecimal ret = new BigDecimal(NEWTON_METHOD_REPEATS + 1);
    for (long i = NEWTON_METHOD_REPEATS; i >= 0; i--) {
      BigDecimal N = new BigDecimal(i / 2 + 1).pow(2);
      N = N.multiply(x, AviatorEvaluator.getMathContext());
      ret = N.divide(ret, AviatorEvaluator.getMathContext());

      N = new BigDecimal(i + 1);
      ret = ret.add(N, AviatorEvaluator.getMathContext());

    }

    ret = x.divide(ret, AviatorEvaluator.getMathContext());
    return ret;
  }

}
