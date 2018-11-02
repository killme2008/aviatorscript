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

import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import java.util.Map;


/**
 * Function to make predicate for filter function
 * 
 * @author dennis
 * 
 */
public class SeqMakePredicateFunFunction extends AbstractFunction {
  private final String name;
  private final OperatorType opType;
  private final AviatorObject value;


  public SeqMakePredicateFunFunction(String name, OperatorType opType) {
    this(name, opType, null);
  }


  public SeqMakePredicateFunFunction(String name, OperatorType opType, AviatorObject value) {
    super();
    this.name = name;
    this.opType = opType;
    this.value = value;
  }


  @Override
  public AviatorObject call(Map<String, Object> env) {
    return new AviatorRuntimeJavaType(new SeqPredicateFunction(this.name, this.opType, this.value));
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    return new AviatorRuntimeJavaType(new SeqPredicateFunction(this.name, this.opType, arg1));
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
    return new AviatorRuntimeJavaType(new SeqPredicateFunction(this.name, this.opType, arg1, arg2));
  }

  @Override
  public String getName() {
    return this.name;
  }

}
