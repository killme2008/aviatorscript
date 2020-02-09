package com.googlecode.aviator.parser;

import java.util.Deque;

public class ScopeInfo {
  int parenDepth;
  int bracketDepth;
  int lambdaDepth;
  int braceDepth;
  boolean inForLoop;

  Deque<DepthState> depthState;

  public ScopeInfo(final int parenDepth, final int bracketDepth, final int lambdaDepth,
      final int braceDepth, final boolean inForLoop, final Deque<DepthState> depthState) {
    super();
    this.parenDepth = parenDepth;
    this.bracketDepth = bracketDepth;
    this.lambdaDepth = lambdaDepth;
    this.braceDepth = braceDepth;
    this.depthState = depthState;
    this.inForLoop = inForLoop;
  }
}
