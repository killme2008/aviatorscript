package com.googlecode.aviator.exception;

/**
 * The expression execution is timed out.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 5.4.2
 */
public class TimeoutException extends ExpressionRuntimeException {

  private static final long serialVersionUID = -3749680912179160158L;

  public TimeoutException() {
    super();
  }

  public TimeoutException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public TimeoutException(final String message) {
    super(message);
  }

  public TimeoutException(final Throwable cause) {
    super(cause);
  }

}
