package com.googlecode.aviator.parser;

import java.util.Deque;

public class ScopeInfo {
  int parenDepth;

  int bracketDepth;

  int lambdaDepth;

  Deque<DepthState> depthState;

  public ScopeInfo(final int parenDepth, final int bracketDepth, final int lambdaDepth,
      final Deque<DepthState> depthState) {
    super();
    this.parenDepth = parenDepth;
    this.bracketDepth = bracketDepth;
    this.lambdaDepth = lambdaDepth;
    this.depthState = depthState;
  }
}
