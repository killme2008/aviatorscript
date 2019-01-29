package com.googlecode.aviator.exception;

/**
 * Function not found exception.
 *
 * @author dennis
 *
 */
public class FunctionNotFoundException extends ExpressionRuntimeException {

  /**
   *
   */
  private static final long serialVersionUID = -8151661221248281327L;

  public FunctionNotFoundException() {
    super();
  }

  public FunctionNotFoundException(String message, Throwable cause) {
    super(message, cause);
  }

  public FunctionNotFoundException(String message) {
    super(message);
  }

  public FunctionNotFoundException(Throwable cause) {
    super(cause);
  }

}
