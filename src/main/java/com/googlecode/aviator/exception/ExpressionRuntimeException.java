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
package com.googlecode.aviator.exception;

/**
 * Expression runtime exception
 * 
 * @author dennis
 * 
 */
public class ExpressionRuntimeException extends RuntimeException {
  static final long serialVersionUID = -1;


  public ExpressionRuntimeException() {
    super();

  }


  public ExpressionRuntimeException(String message, Throwable cause) {
    super(message, cause);

  }


  public ExpressionRuntimeException(String message) {
    super(message);

  }


  public ExpressionRuntimeException(Throwable cause) {
    super(cause);

  }

}
