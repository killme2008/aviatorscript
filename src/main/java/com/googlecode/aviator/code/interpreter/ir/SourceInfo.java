package com.googlecode.aviator.code.interpreter.ir;

import java.io.Serializable;

/**
 * Source info to debug.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class SourceInfo implements Serializable {
  private static final long serialVersionUID = -5836810241460848224L;
  public final String sourceFile;
  public final int lineNo;

  public SourceInfo(final String sourceFile, final int lineNo) {
    super();
    this.sourceFile = sourceFile;
    this.lineNo = lineNo;
  }

  @Override
  public String toString() {
    return "(" + this.sourceFile + ": " + this.lineNo + ")";
  }


}
