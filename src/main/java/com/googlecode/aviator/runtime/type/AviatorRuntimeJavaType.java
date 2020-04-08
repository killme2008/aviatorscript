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
import java.util.concurrent.Callable;
import com.googlecode.aviator.utils.Reflector;
import com.googlecode.aviator.utils.VarNameGenerator;


/**
 * Aviator runtime java type,used by when generate runtime result.
 *
 * @author dennis
 *
 */
public class AviatorRuntimeJavaType extends AviatorJavaType {


  private static final long serialVersionUID = 3107203976124904223L;

  public static final ThreadLocal<VarNameGenerator> TEMP_VAR_GEN =
      new ThreadLocal<VarNameGenerator>() {

        @Override
        protected VarNameGenerator initialValue() {
          return new VarNameGenerator();
        }

      };

  protected Object object;
  protected Callable<Object> callable;

  public static AviatorObject valueOf(final Object object) {
    if (object == null) {
      return AviatorNil.NIL;
    }
    if (object instanceof AviatorObject) {
      return (AviatorObject) object;
    }
    return new AviatorRuntimeJavaType(object);
  }

  /**
   * Deprecated since 5.0.0, please use {@link AviatorRuntimeJavaType#valueOf(Object)} instead.
   *
   * @deprecated
   * @param object
   */
  @Deprecated
  public AviatorRuntimeJavaType(final Object object) {
    super(genName());
    this.object = object;
  }

  public Callable<Object> getCallable() {
    return this.callable;
  }

  public void setCallable(final Callable<Object> callable) {
    this.callable = callable;
  }

  public static String genName() {
    return TEMP_VAR_GEN.get().gen();
  }

  @Override
  public Object getValue(final Map<String, Object> env) {
    if (this.callable != null) {
      try {
        return this.callable.call();
      } catch (Exception e) {
        throw Reflector.sneakyThrow(e);
      }
    }
    return this.object;
  }

}
