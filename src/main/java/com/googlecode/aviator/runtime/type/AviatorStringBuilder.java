package com.googlecode.aviator.runtime.type;

import java.util.Map;

/**
 * Aviator string builder for fast concatenating string.
 *
 * @author boyan(boyan@antfin.com)
 *
 */
public class AviatorStringBuilder extends AviatorString {
  private final StringBuilder sb;

  public AviatorStringBuilder(final StringBuilder sb) {
    super(null);
    this.sb = sb;
  }

  public AviatorStringBuilder(final String lexeme) {
    super(null);
    this.sb = new StringBuilder(lexeme);
  }

  @Override
  public String getLexeme() {
    return this.sb.toString();
  }


  @Override
  public AviatorObject deref(final Map<String, Object> env) {
    return new AviatorString(getLexeme());
  }

  @Override
  public AviatorObject add(final AviatorObject other, final Map<String, Object> env) {
    this.sb.append(other.getValue(env));
    return this;
  }
}
