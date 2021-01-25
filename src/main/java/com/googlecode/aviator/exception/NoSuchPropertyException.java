package com.googlecode.aviator.exception;

/**
 * No such property exception when property not found in java beans.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 5.2.3
 */
public class NoSuchPropertyException extends ExpressionRuntimeException {

  private static final long serialVersionUID = -3749680912179160158L;

  public NoSuchPropertyException() {
    super();
  }

  public NoSuchPropertyException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public NoSuchPropertyException(final String message) {
    super(message);
  }

  public NoSuchPropertyException(final Throwable cause) {
    super(cause);
  }

}
