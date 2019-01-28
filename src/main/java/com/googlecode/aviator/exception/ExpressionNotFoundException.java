package com.googlecode.aviator.exception;

/**
 * Compiled expression not found exception.
 *
 * @author dennis
 *
 */
public class ExpressionNotFoundException extends ExpressionRuntimeException {

  /**
   *
   */
  private static final long serialVersionUID = -8151661221248281327L;

  public ExpressionNotFoundException() {
    super();
  }

  public ExpressionNotFoundException(String message, Throwable cause) {
    super(message, cause);
  }

  public ExpressionNotFoundException(String message) {
    super(message);
  }

  public ExpressionNotFoundException(Throwable cause) {
    super(cause);
  }

}
