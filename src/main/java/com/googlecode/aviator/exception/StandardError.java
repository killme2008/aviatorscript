package com.googlecode.aviator.exception;

/**
 * Standard error.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class StandardError extends Exception {

  private static final long serialVersionUID = 7601099490172089234L;

  public StandardError() {
    super();
  }

  public StandardError(final String message, final Throwable cause, final boolean enableSuppression,
      final boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }

  public StandardError(final String message, final Throwable cause) {
    super(message, cause);
  }

  public StandardError(final String message) {
    super(message);
  }

  public StandardError(final Throwable cause) {
    super(cause);
  }


}
