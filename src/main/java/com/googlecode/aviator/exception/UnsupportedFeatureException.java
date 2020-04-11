package com.googlecode.aviator.exception;

import com.googlecode.aviator.Feature;

public class UnsupportedFeatureException extends ExpressionSyntaxErrorException {
  private static final long serialVersionUID = 6543462982851212129L;

  public UnsupportedFeatureException(final Feature feature) {
    super("Feature " + feature + " is not enabled");
  }

  public UnsupportedFeatureException() {
    super();
  }

  public UnsupportedFeatureException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public UnsupportedFeatureException(final String message) {
    super(message);
  }

  public UnsupportedFeatureException(final Throwable cause) {
    super(cause);
  }

}
