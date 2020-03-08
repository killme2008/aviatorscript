package com.googlecode.aviator.exception;

public class LoadScriptFailureException extends ExpressionRuntimeException {

  /**
   *
   */
  private static final long serialVersionUID = -4311966452570613367L;

  public LoadScriptFailureException() {
    super();
  }

  public LoadScriptFailureException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public LoadScriptFailureException(final String message) {
    super(message);
  }

  public LoadScriptFailureException(final Throwable cause) {
    super(cause);
  }

}
