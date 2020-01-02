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
package com.googlecode.aviator.runtime.type;

import java.util.Map;
import com.googlecode.aviator.utils.LongWrapper;


/**
 * Aviator runtime java type,used by when generate runtime result.
 *
 * @author dennis
 *
 */
public class AviatorRuntimeJavaType extends AviatorJavaType {
  private static final String TEMP_VAR = "G_";

  private static final int TEMP_LEN = TEMP_VAR.length() + String.valueOf(Long.MIN_VALUE).length();

  public static final ThreadLocal<LongWrapper> TEMP_NUM = new ThreadLocal<LongWrapper>() {

    @Override
    protected LongWrapper initialValue() {
      return new LongWrapper(0L);
    }

  };

  public static String genTempVar() {
    StringBuilder sb = new StringBuilder(TEMP_LEN);
    return sb.append(TEMP_VAR).append(TEMP_NUM.get().getAndAdd()).toString();
  }

  protected final Object object;

  public static AviatorObject valueOf(final Object object) {
    if (object == null) {
      return AviatorNil.NIL;
    }
    if (object instanceof AviatorObject) {
      return (AviatorObject) object;
    }
    return new AviatorRuntimeJavaType(object);
  }

  public AviatorRuntimeJavaType(final Object object) {
    super(genTempVar());
    this.object = object;
  }


  @Override
  public Object getValue(final Map<String, Object> env) {
    return this.object;
  }

}
