package com.googlecode.aviator.code.interpreter.ir;

/**
 * Source info to debug.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class SourceInfo {
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
