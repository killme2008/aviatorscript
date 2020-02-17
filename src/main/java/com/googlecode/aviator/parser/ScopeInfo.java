package com.googlecode.aviator.parser;

import java.util.Deque;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;

/**
 * Lexical scope info
 * 
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ScopeInfo {
  int parenDepth;
  int bracketDepth;
  int lambdaDepth;
  int braceDepth;
  boolean newLexicalScope;

  Deque<DepthState> depthState;

  public ScopeInfo(final int parenDepth, final int bracketDepth, final int lambdaDepth,
      final int braceDepth, final boolean inNewScope, final Deque<DepthState> depthState) {
    super();
    this.parenDepth = parenDepth;
    this.bracketDepth = bracketDepth;
    this.lambdaDepth = lambdaDepth;
    this.braceDepth = braceDepth;
    this.depthState = depthState;
    this.newLexicalScope = inNewScope;
  }

  void enterBracket() {
    this.bracketDepth++;
    this.depthState.add(DepthState.Bracket);
  }

  void leaveBracket() {
    this.bracketDepth--;
    if (this.depthState.removeLast() != DepthState.Bracket) {
      throw new ExpressionSyntaxErrorException("Mismatch bracket");
    }
  }

  void enterLambda() {
    this.lambdaDepth++;
    this.depthState.add(DepthState.Lambda);
  }

  void leaveLambda() {
    this.lambdaDepth--;
    if (this.depthState.removeLast() != DepthState.Lambda) {
      throw new ExpressionSyntaxErrorException("Mismatch lambda definition");
    }
  }

  void enterParen() {
    this.parenDepth++;
    this.depthState.add(DepthState.Parent);
  }

  void leaveParen() {
    this.parenDepth--;
    if (this.depthState.removeLast() != DepthState.Parent) {
      throw new ExpressionSyntaxErrorException("Mismatch paren");
    }
  }

  void enterBrace() {
    this.braceDepth++;
    this.depthState.add(DepthState.Brace);
  }

  void leaveBrace() {
    this.braceDepth--;
    if (this.depthState.removeLast() != DepthState.Brace) {
      throw new ExpressionSyntaxErrorException("Mismatch brace");
    }
  }
}
