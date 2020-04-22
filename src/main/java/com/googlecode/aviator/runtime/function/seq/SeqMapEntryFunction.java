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
package com.googlecode.aviator.runtime.function.seq;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


/**
 * seq.entry(key, value) function to create a Map.Entry instance.
 *
 * @author dennis
 *
 */
public class SeqMapEntryFunction extends AbstractFunction {
  private static final long serialVersionUID = -93114232885437186L;


  @SuppressWarnings({"rawtypes"})
  private static final class MapEntry implements Map.Entry {
    private final Object key;
    private Object value;

    private MapEntry(final Object key, final Object value) {
      this.key = key;
      this.value = value;
    }

    @Override
    public Object getKey() {
      return this.key;
    }

    @Override
    public Object getValue() {
      return this.value;
    }

    @Override
    public Object setValue(final Object value) {
      Object oldVal = this.value;
      this.value = value;
      return oldVal;
    }

    @Override
    public int hashCode() {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((this.key == null) ? 0 : this.key.hashCode());
      result = prime * result + ((this.value == null) ? 0 : this.value.hashCode());
      return result;
    }

    @Override
    public boolean equals(final Object obj) {
      if (this == obj) {
        return true;
      }
      if (obj == null) {
        return false;
      }
      if (!(obj instanceof Map.Entry)) {
        return false;
      }
      Map.Entry other = (Map.Entry) obj;
      if (this.key == null) {
        if (other.getKey() != null) {
          return false;
        }
      } else if (!this.key.equals(other.getKey())) {
        return false;
      }
      if (this.value == null) {
        if (other.getValue() != null) {
          return false;
        }
      } else if (!this.value.equals(other.getValue())) {
        return false;
      }
      return true;
    }

  }



  @Override

  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    final Object key = arg1.getValue(env);
    final Object value = arg2.getValue(env);

    return AviatorRuntimeJavaType.valueOf(new MapEntry(key, value));
  }


  @Override
  public String getName() {
    return "seq.entry";
  }

}
