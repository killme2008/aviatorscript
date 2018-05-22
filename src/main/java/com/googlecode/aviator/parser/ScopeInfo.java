package com.googlecode.aviator.parser;

import java.util.LinkedList;

public class ScopeInfo {
  int parenDepth;

  int bracketDepth;

  int lambdaDepth;

  LinkedList<DepthState> depthState;

  public ScopeInfo(int parenDepth, int bracketDepth, int lambdaDepth,
      LinkedList<DepthState> depthState) {
    super();
    this.parenDepth = parenDepth;
    this.bracketDepth = bracketDepth;
    this.lambdaDepth = lambdaDepth;
    this.depthState = depthState;
  }
}
