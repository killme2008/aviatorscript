package com.googlecode.aviator.runtime.type.string;

import java.util.Map;

/**
 * A string literal segment
 * 
 * @author dennis(killme2008@gmail.com)
 *
 */
public class LiteralSegment implements StringSegment {
  String literal;


  public LiteralSegment(final String literal) {
    super();
    this.literal = literal;
  }


  @Override
  public String toString() {
    return "LiteralSegment [literal=" + this.literal + "]";
  }


  @Override
  public StringBuilder appendTo(final StringBuilder sb, final Map<String, Object> env) {
    return sb.append(this.literal);
  }
}
