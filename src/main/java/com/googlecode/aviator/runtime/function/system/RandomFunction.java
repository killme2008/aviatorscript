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
package com.googlecode.aviator.runtime.function.system;

import java.security.SecureRandom;
import java.util.Map;
import java.util.Random;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorDouble;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * rand() function to generate random double value
 * 
 * @author dennis
 * 
 */
public class RandomFunction extends AbstractFunction {

  private static ThreadLocal<Random> randomLocal = new ThreadLocal<Random>() {

    @Override
    protected Random initialValue() {
      return new SecureRandom();
    }

  };


  @Override
  public AviatorObject call(Map<String, Object> env) {
    return AviatorDouble.valueOf(randomLocal.get().nextDouble());
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg) {
    return AviatorLong
        .valueOf(randomLocal.get().nextInt(FunctionUtils.getNumberValue(arg, env).intValue()));
  }


  @Override
  public String getName() {
    return "rand";
  }
}
