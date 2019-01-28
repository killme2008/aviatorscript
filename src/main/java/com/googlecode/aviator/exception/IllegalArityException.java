package com.googlecode.aviator.exception;

/**
 * Illegal function arity exception.
 *
 * @author dennis
 *
 */
public class IllegalArityException extends RuntimeException {
  static final long serialVersionUID = -1;


  public IllegalArityException() {
    super();

  }


  public IllegalArityException(String message, Throwable cause) {
    super(message, cause);

  }


  public IllegalArityException(String message) {
    super(message);

  }


  public IllegalArityException(Throwable cause) {
    super(cause);

  }

}
