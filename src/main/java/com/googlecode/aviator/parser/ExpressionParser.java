/**
 * Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program;
 * if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.parser;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.Feature;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.code.CodeGenerator;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.exception.UnsupportedFeatureException;
import com.googlecode.aviator.lexer.ExpressionLexer;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.lexer.token.CharToken;
import com.googlecode.aviator.lexer.token.DelegateToken;
import com.googlecode.aviator.lexer.token.DelegateToken.DelegateTokenType;
import com.googlecode.aviator.lexer.token.PatternToken;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Token.TokenType;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.utils.Constants;


/**
 * Syntex parser for expression
 *
 * @author dennis
 *
 */
public class ExpressionParser implements Parser {
  private final ExpressionLexer lexer;

  /*
   * Lookhead token
   */
  private Token<?> lookhead;

  private Token<?> prevToken;

  private CodeGenerator codeGenerator;

  private ScopeInfo scope;


  private boolean inPattern = false;

  private final AviatorEvaluatorInstance instance;

  private final boolean captureFuncArgs;

  private final Set<Feature> featureSet;


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.parser.Parser#getCodeGenerator()
   */
  @Override
  public CodeGenerator getCodeGenerator() {
    return this.codeGenerator;
  }

  @Override
  public SymbolTable getSymbolTable() {
    return this.lexer.getSymbolTable();
  }

  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.parser.Parser#setCodeGenerator(com.googlecode.aviator.code.
   * CodeGenerator)
   */
  @Override
  public void setCodeGenerator(final CodeGenerator codeGenerator) {
    this.codeGenerator = codeGenerator;
  }

  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.parser.Parser#enterScope()
   */
  @Override
  public ScopeInfo enterScope(final boolean inNewScope) {
    ScopeInfo current = this.scope;
    this.scope = new ScopeInfo(0, 0, 0, 0, inNewScope, new ArrayDeque<DepthState>());
    return current;
  }

  /**
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.parser.Parser#restoreScope(com.googlecode.aviator.parser.ExpressionParser.DepthInfo)
   */
  @Override
  public void restoreScope(final ScopeInfo info) {
    this.scope = info;
  }

  public ExpressionParser(final AviatorEvaluatorInstance instance, final ExpressionLexer lexer,
      final CodeGenerator codeGenerator) {
    super();
    this.scope = new ScopeInfo(0, 0, 0, 0, false, new ArrayDeque<DepthState>());
    this.instance = instance;
    this.captureFuncArgs = instance.getOptionValue(Options.CAPTURE_FUNCTION_ARGS).bool;
    this.lexer = lexer;
    this.lookhead = this.lexer.scan();
    this.featureSet = this.instance.getOptionValue(Options.FEATURE_SET).featureSet;
    if (this.lookhead == null) {
      reportSyntaxError("blank script");
    }
    setCodeGenerator(codeGenerator);
    getCodeGeneratorWithTimes().setParser(this);
  }

  private void ensureFeatureEnabled(final Feature feature) {
    if (!this.featureSet.contains(feature)) {
      throw new UnsupportedFeatureException(feature);
    }
  }

  /**
   * Call __reducer_return(result)
   */
  public void returnStatement() {
    move(true);
    CodeGenerator cg = getCodeGeneratorWithTimes();
    cg.onTernaryEnd(this.lookhead);
    if (expectChar(';')) {
      // 'return;' => 'return nil;'
      if (this.scope.newLexicalScope) {
        cg.onMethodName(Constants.ReducerReturnFn);
        cg.onConstant(Variable.NIL);
        cg.onMethodParameter(this.lookhead);
        cg.onMethodInvoke(this.lookhead);
      } else {
        cg.onConstant(Variable.NIL);
      }
      move(true);
      return;
    } else {
      if (this.scope.newLexicalScope) {
        cg.onMethodName(Constants.ReducerReturnFn);
        if (!ternary()) {
          reportSyntaxError("invalid value for return, missing ';'?");
        }
        cg.onMethodParameter(this.lookhead);
        cg.onMethodInvoke(this.lookhead);
      } else {
        if (!ternary()) {
          reportSyntaxError("invalid value for return, missing ';'?");
        }
      }
    }

    if (!expectChar(';')) {
      reportSyntaxError("missing ';' for return statement");
    }
    move(true);
  }

  public boolean ternary() {
    int gcTimes = this.getCGTimes;
    join();
    if (this.lookhead == null || expectChar(':') || expectChar(',')) {
      return gcTimes < this.getCGTimes;
    }
    if (expectChar('?')) {
      move(true);
      CodeGenerator cg = getCodeGeneratorWithTimes();
      cg.onTernaryBoolean(this.lookhead);
      if (!ternary()) {
        reportSyntaxError("invalid token for ternary operator");
      }
      if (expectChar(':')) {
        move(true);
        cg.onTernaryLeft(this.lookhead);
        if (!ternary()) {
          reportSyntaxError("invalid token for ternary operator");
        }
        cg.onTernaryRight(this.lookhead);
      } else {
        reportSyntaxError("expect ':'");
      }
    }
    return gcTimes < this.getCGTimes;
  }


  public void join() {
    and();
    while (true) {
      if (expectChar('|')) {
        getCodeGeneratorWithTimes().onJoinLeft(this.lookhead);
        move(true);
        if (expectChar('|')) {
          move(true);
          and();
          getCodeGeneratorWithTimes().onJoinRight(this.lookhead);
        } else {
          reportSyntaxError("expect '|'");
        }
      } else {
        if (this.lookhead == null) {
          break;
        } else {
          break;
        }
      }

    }
  }


  private boolean expectChar(final char ch) {
    if (this.lookhead == null) {
      return false;
    }
    return this.lookhead.getType() == TokenType.Char && ((CharToken) this.lookhead).getCh() == ch;
  }


  public void bitOr() {
    xor();
    while (true) {
      if (expectChar('|')) {
        move(true);
        if (expectChar('|')) {
          back();
          break;
        }
        xor();
        getCodeGeneratorWithTimes().onBitOr(this.lookhead);
      } else {
        break;
      }
    }
  }


  public void xor() {
    bitAnd();
    while (true) {
      if (expectChar('^')) {
        move(true);
        bitAnd();
        getCodeGeneratorWithTimes().onBitXor(this.lookhead);
      } else {
        break;
      }
    }
  }


  public void bitAnd() {
    equality();
    while (true) {
      if (expectChar('&')) {
        move(true);
        if (expectChar('&')) {
          back();
          break;
        }
        equality();
        getCodeGeneratorWithTimes().onBitAnd(this.lookhead);
      } else {
        break;
      }
    }
  }


  public void and() {
    bitOr();
    while (true) {
      if (expectChar('&')) {
        CodeGenerator cg = getCodeGeneratorWithTimes();
        cg.onAndLeft(this.lookhead);
        move(true);
        if (expectChar('&')) {
          move(true);
          bitOr();
          cg.onAndRight(this.lookhead);
        } else {
          reportSyntaxError("expect '&'");
        }
      } else {
        break;
      }
    }

  }


  public void equality() {
    rel();
    while (true) {
      if (expectChar('=')) {
        move(true);
        if (expectChar('=')) {
          move(true);
          rel();
          getCodeGeneratorWithTimes().onEq(this.lookhead);
        } else if (expectChar('~')) {
          // It is a regular expression
          move(true);
          rel();
          getCodeGeneratorWithTimes().onMatch(this.lookhead);
        } else {
          // this.back();
          // assignment
          statement();
          ensureFeatureEnabled(Feature.Assignment);
          getCodeGeneratorWithTimes().onAssignment(this.lookhead);
        }
      } else if (expectChar('!')) {
        move(true);
        if (expectChar('=')) {
          move(true);
          rel();
          getCodeGeneratorWithTimes().onNeq(this.lookhead);
        } else {
          reportSyntaxError("expect '='");
        }
      } else {
        break;
      }
    }
  }


  public void rel() {
    shift();
    while (true) {
      if (expectChar('<')) {
        move(true);
        if (expectChar('=')) {
          move(true);
          expr();
          getCodeGeneratorWithTimes().onLe(this.lookhead);
        } else {
          expr();
          getCodeGeneratorWithTimes().onLt(this.lookhead);
        }
      } else if (expectChar('>')) {
        move(true);
        if (expectChar('=')) {
          move(true);
          expr();
          getCodeGeneratorWithTimes().onGe(this.lookhead);
        } else {
          expr();
          getCodeGeneratorWithTimes().onGt(this.lookhead);
        }
      } else {
        break;
      }
    }
  }


  public void shift() {
    expr();
    while (true) {
      if (expectChar('<')) {
        move(true);
        if (expectChar('<')) {
          move(true);
          expr();
          getCodeGeneratorWithTimes().onShiftLeft(this.lookhead);
        } else {
          back();
          break;
        }
      } else if (expectChar('>')) {
        move(true);
        if (expectChar('>')) {
          move(true);
          if (expectChar('>')) {
            move(true);
            expr();
            getCodeGeneratorWithTimes().onUnsignedShiftRight(this.lookhead);
          } else {
            expr();
            getCodeGeneratorWithTimes().onShiftRight(this.lookhead);
          }

        } else {
          back();
          break;
        }
      } else {
        break;
      }
    }
  }


  public void expr() {
    term();
    while (true) {
      if (expectChar('+')) {
        move(true);
        term();
        getCodeGeneratorWithTimes().onAdd(this.lookhead);
      } else if (expectChar('-')) {
        move(true);
        term();
        getCodeGeneratorWithTimes().onSub(this.lookhead);
      } else {
        break;
      }
    }
  }


  public void term() {
    unary();
    while (true) {
      if (expectChar('*')) {
        move(true);
        unary();
        getCodeGeneratorWithTimes().onMult(this.lookhead);
      } else if (expectChar('/')) {
        move(true);
        unary();
        getCodeGeneratorWithTimes().onDiv(this.lookhead);
      } else if (expectChar('%')) {
        move(true);
        unary();
        getCodeGeneratorWithTimes().onMod(this.lookhead);
      } else {
        break;
      }
    }
  }


  public void unary() {
    if (expectChar('!')) {
      move(true);
      // check if it is a seq function call,"!" as variable
      if (expectChar(',') || expectChar(')')) {
        back();
        factor();
      } else {
        unary();
        getCodeGeneratorWithTimes().onNot(this.lookhead);
      }
    } else if (expectChar('-')) {
      move(true);
      // check if it is a seq function call,"!" as variable
      if (expectChar(',') || expectChar(')')) {
        back();
        factor();
      } else {
        unary();
        getCodeGeneratorWithTimes().onNeg(this.lookhead);
      }
    } else if (expectChar('~')) {
      move(true);
      // check if it is a seq function call,"~" as variable
      if (expectChar(',') || expectChar(')')) {
        back();
        factor();
      } else {
        unary();
        getCodeGeneratorWithTimes().onBitNot(this.lookhead);
      }
    } else {
      factor();
    }


    while (expectChar('[') || expectChar('(')) {
      if (isConstant(this.prevToken)) {
        break;
      }

      if (expectChar('[')) {
        // (...)[index]
        arrayAccess();
      } else if (expectChar('(')) {
        this.scope.enterParen();
        final CodeGenerator cg = getCodeGeneratorWithTimes();
        cg.onMethodName(anonymousMethodName());
        move(true);
        // FIXME: duplicated code with method()
        List<FunctionArgument> params = null;
        if (this.captureFuncArgs) {
          params = new ArrayList<>();
        }
        int paramIndex = 0;
        int lastTokenIndex = getLookheadStartIndex();
        if (!expectChar(')')) {
          ternary();
          cg.onMethodParameter(this.lookhead);
          if (this.captureFuncArgs) {
            params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
          }

          while (expectChar(',')) {
            move(true);
            lastTokenIndex = getLookheadStartIndex();
            if (!ternary()) {
              reportSyntaxError("invalid argument");
            }
            cg.onMethodParameter(this.lookhead);
            if (this.captureFuncArgs) {
              params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
            }
          }
        }
        if (expectChar(')')) {
          cg.onMethodInvoke(this.lookhead.withMeta(Constants.PARAMS_META, params));
          this.scope.leaveParen();
          move(true);
        }
      }
    }

  }

  private int getLookheadStartIndex() {
    // We should calculate the lookhead token's start index, because the token may be reserved by
    // symbol table and it's start index is wrong.
    return this.lookhead != null ? (this.lexer.getCurrentIndex() - getLookheadLexemeLength()) : -1;
  }

  private int getLookheadLexemeLength() {
    int len = this.lookhead.getLexeme().length();
    if (this.lookhead.getType() == TokenType.String) {
      // Must include quote symbols.
      len += 2;
    }
    return len;
  }

  private String getParamExp(final int lastTokenIndex) {
    if (lastTokenIndex >= 0 && getLookheadStartIndex() >= 0) {
      return this.lexer.getScanString().substring(lastTokenIndex, getLookheadStartIndex());
    } else {
      return null;
    }
  }

  public static final CharToken LEFT_PAREN = new CharToken('(', -1);
  public static final CharToken RIGHT_PAREN = new CharToken(')', -1);


  public boolean isOPVariable(final Token<?> token) {
    if (token.getType() != TokenType.Char) {
      return false;
    }
    CharToken charToken = (CharToken) token;

    move(true);
    if (expectChar(',') || expectChar(')')) {
      back();
      String lexeme = String.valueOf(charToken.getCh());
      if (lexeme.equals("-")) {
        lexeme = "-sub";
      }
      return this.instance.containsFunction(lexeme);
    } else {
      back();
      return false;
    }
  }

  public void factor() {
    if (this.lookhead == null) {
      reportSyntaxError("illegal token");
    }
    if (this.lookhead == Variable.END) {
      return;
    }
    if (expectChar('(')) {
      move(true);
      this.scope.enterParen();
      ternary();
      if (expectChar(')')) {
        move(true);
        this.scope.leaveParen();
      }

    } else if (this.lookhead.getType() == TokenType.Number
        || this.lookhead.getType() == TokenType.String
        || this.lookhead.getType() == TokenType.Variable || this.lookhead == Variable.TRUE
        || this.lookhead == Variable.FALSE || isOPVariable(this.lookhead)) {
      if (this.lookhead.getType() == TokenType.Variable) {
        checkVariableName();
      }
      // binary operation as variable for seq functions
      if (this.lookhead.getType() == TokenType.Char) {
        CharToken charToken = (CharToken) this.lookhead;
        if (!ExpressionLexer.isBinaryOP(charToken.getCh())) {
          reportSyntaxError("unexpect char '" + charToken.getCh() + "'");
        }
        // make it as variable
        this.lookhead = this.lexer.getSymbolTable()
            .reserve(new Variable(charToken.getLexeme(), charToken.getStartIndex()));
      }
      move(true);
      // function
      Token<?> prev = this.prevToken;
      if (prev.getType() == TokenType.Variable && expectChar('(')) {
        if (prev == Variable.LAMBDA) {
          lambda(false);
        } else {
          method();
        }
      } else if (prev.getType() == TokenType.Variable) {
        arrayAccess();
      } else {
        getCodeGeneratorWithTimes().onConstant(prev);
      }
    } else if (expectChar('/')) {
      pattern();
    } else if (expectChar('}')) {
      return;
    } else {
      reportSyntaxError("invalid token");
    }

  }


  private void lambda(final boolean fn) {
    ensureFeatureEnabled(Feature.Lambda);
    this.scope.enterLambda();
    getCodeGeneratorWithTimes().onLambdaDefineStart(this.prevToken);
    this.scope.enterParen();
    move(true);
    if (!expectChar(')')) {
      lambdaArgument();

      while (expectChar(',')) {
        move(true);
        lambdaArgument();
      }
    }
    if (expectChar(')')) {
      this.scope.leaveParen();
      move(true);

      if (fn) {
        if (!expectChar('{')) {
          reportSyntaxError("expect '{'");
        }
      } else {
        if (!expectChar('-')) {
          reportSyntaxError("expect '->' for lambda body");
        }
        move(true);
        if (!expectChar('>')) {
          reportSyntaxError("expect '->' for lambda body");
        }
      }

      move(true);
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
      statements();

      if (fn) {
        if (!expectChar('}')) {
          reportSyntaxError("missing '}' to close function body");
        }
      } else {
        if (this.lookhead != Variable.END) {
          reportSyntaxError("expect lambda 'end'");
        }
      }

      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
      this.scope.leaveLambda();
      move(true);
    }
  }

  private String currentTokenLexeme() {
    return this.lookhead == null ? "END_OF_STRING" : this.lookhead.getLexeme();
  }


  private void lambdaArgument() {
    if (this.lookhead.getType() == TokenType.Variable) {
      if (!isJavaIdentifier(this.lookhead.getLexeme())) {
        reportSyntaxError("illegal argument name: " + currentTokenLexeme());
      }
      getCodeGeneratorWithTimes().onLambdaArgument(this.lookhead);
      move(true);
    } else {
      reportSyntaxError("expect argument name, but is: " + currentTokenLexeme());
    }
  }


  private void arrayAccess() {
    // check if it is a array index access
    boolean hasArray = false;
    while (expectChar('[')) {
      if (!hasArray) {
        getCodeGeneratorWithTimes().onArray(this.prevToken);
        move(true);
        hasArray = true;
      } else {
        move(true);
      }
      getCodeGeneratorWithTimes().onArrayIndexStart(this.prevToken);
      array();
    }
    if (!hasArray) {
      getCodeGeneratorWithTimes().onConstant(this.prevToken);
    }
  }


  private void array() {
    this.scope.enterBracket();
    if (this.prevToken == Variable.TRUE || this.prevToken == Variable.FALSE
        || this.prevToken == Variable.NIL) {
      reportSyntaxError(this.prevToken.getLexeme() + " could not use [] operator");
    }
    if (!ternary()) {
      reportSyntaxError("missing index for array access");
    }
    if (expectChar(']')) {
      this.scope.leaveBracket();
      move(true);
      getCodeGeneratorWithTimes().onArrayIndexEnd(this.lookhead);
    }
  }

  private void checkVariableName() {
    if (!((Variable) this.lookhead).isQuote()) {
      String[] names = this.lookhead.getLexeme().split("\\.");
      for (String name : names) {
        if (!isJavaIdentifier(name)) {
          reportSyntaxError("illegal identifier: " + name);
        }
      }
    }
  }

  private void method() {
    if (expectChar('(')) {
      this.scope.enterParen();
      getCodeGeneratorWithTimes().onMethodName(this.prevToken);
      move(true);
      int paramIndex = 0;
      List<FunctionArgument> params = null;
      if (this.captureFuncArgs) {
        params = new ArrayList<>();
      }
      int lastTokenIndex = getLookheadStartIndex();
      if (!expectChar(')')) {
        ternary();
        getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
        if (this.captureFuncArgs) {
          params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
        }
        while (expectChar(',')) {
          move(true);
          lastTokenIndex = getLookheadStartIndex();
          if (!ternary()) {
            reportSyntaxError("invalid argument");
          }
          getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
          if (this.captureFuncArgs) {
            params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
          }
        }
      }
      if (expectChar(')')) {
        getCodeGeneratorWithTimes()
            .onMethodInvoke(this.lookhead.withMeta(Constants.PARAMS_META, params));
        move(true);
        this.scope.leaveParen();
      }
    }
  }


  /**
   * Test whether a given string is a valid Java identifier.
   *
   * @param id string which should be checked
   * @return <code>true</code> if a valid identifier
   */
  public static final boolean isJavaIdentifier(final String id) {
    if (id == null) {
      return false;
    }

    if (id.equals("")) {
      return false;
    }

    if (!java.lang.Character.isJavaIdentifierStart(id.charAt(0))) {
      return false;
    }

    for (int i = 1; i < id.length(); i++) {
      if (!java.lang.Character.isJavaIdentifierPart(id.charAt(i))) {
        return false;
      }
    }
    if (id.equals("null")) {
      return false;
    }
    return true;
  }


  private void pattern() {
    // It is a pattern
    int startIndex = this.lookhead.getStartIndex();
    move(true);
    this.inPattern = true;
    StringBuilder sb = new StringBuilder();
    while (this.lookhead != null) {
      while (!expectChar('/')) {
        sb.append(this.lookhead.getLexeme());
        move(false);
      }
      if (this.prevToken.getType() == TokenType.Char
          && ((CharToken) this.prevToken).getLexeme().equals("\\")) {
        sb.append("/");
        move(false);
        continue;
      }
      this.inPattern = false;
      break;
    }
    if (this.inPattern) {
      reportSyntaxError("invalid regular pattern:" + sb.toString());
    }
    getCodeGeneratorWithTimes().onConstant(new PatternToken(sb.toString(), startIndex));
    move(true);
  }


  private void reportSyntaxError(final String message) {
    int index = isValidLookhead() ? this.lookhead.getStartIndex() : this.lexer.getCurrentIndex();

    if (this.lookhead != null) {
      this.lexer.pushback(this.lookhead);
    }

    String msg = "Syntax error: " + message + //
        " at " + index + //
        ", lineNumber: " + this.lexer.getLineNo() + //
        ", token : " + //
        this.lookhead + ",\nwhile parsing expression: `\n" + //
        this.lexer.getScanString() + "^^^\n`";

    ExpressionSyntaxErrorException e = new ExpressionSyntaxErrorException(msg);
    StackTraceElement[] traces = e.getStackTrace();
    List<StackTraceElement> filteredTraces = new ArrayList<>();
    for (StackTraceElement t : traces) {
      // if (t.getClassName().equals(this.getClass().getName())) {
      // continue;
      // }
      filteredTraces.add(t);
    }
    e.setStackTrace(filteredTraces.toArray(new StackTraceElement[filteredTraces.size()]));
    throw e;
  }

  private boolean isValidLookhead() {
    return this.lookhead != null && this.lookhead.getStartIndex() > 0;
  }


  public void move(final boolean analyse) {
    if (this.lookhead != null) {
      this.prevToken = this.lookhead;
      this.lookhead = this.lexer.scan(analyse);
    } else {
      reportSyntaxError("illegal token");
    }
  }


  public void back() {
    this.lexer.pushback(this.lookhead);
    this.lookhead = this.prevToken;
  }


  public Expression parse() {
    StatementType statementType = statements();
    if (this.lookhead != null) {
      if (statementType == StatementType.Ternary) {
        reportSyntaxError("unexpect token '" + currentTokenLexeme()
            + "', maybe forget to insert ';' to complete last expression ");
      } else {
        reportSyntaxError("unexpect token '" + currentTokenLexeme() + "'");
      }
    }
    return getCodeGeneratorWithTimes().getResult(true);
  }

  static enum StatementType {
    Ternary, Return, Empty, Other
  }

  /**
   * Call __reducer_break()
   */
  private void breakStatement() {
    if (!this.scope.newLexicalScope) {
      reportSyntaxError("break only can be used in for-loop");
    }
    move(true);
    getCodeGeneratorWithTimes().onMethodName(Constants.ReducerBreakFn);
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
    if (!expectChar(';')) {
      reportSyntaxError("missing ';' for break");
    }
    move(true);
  }

  /**
   * Call __reducer_cont(nil)
   */
  private void continueStatement() {
    if (!this.scope.newLexicalScope) {
      reportSyntaxError("continue only can be used in for-loop");
    }
    move(true);
    getCodeGeneratorWithTimes().onMethodName(Constants.ReducerContFn);
    getCodeGeneratorWithTimes().onConstant(Variable.NIL);
    getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
    if (!expectChar(';')) {
      reportSyntaxError("missing ';' for continue");
    }
    move(true);
  }


  /**
   * <pre>
   *  while(test) {
   *     ...body...
   *  }
   *  ...statements...
   * </pre>
   *
   * ===>
   *
   * <pre>
   *  __reducer_callcc(__reducer_loop, lambda() ->
   *       if(test) {
   *          ...body...
   *       }else {
   *          break;
   *       }
   *  end, lambda()- >
   *       ...statements...
   *  end);
   * </pre>
   */
  private void whileStatement() {
    move(true);

    // prepare to call __reducer_callcc(LOOP, iterator, statements)
    getCodeGeneratorWithTimes().onMethodName(Constants.ReducerFn);
    getCodeGeneratorWithTimes().onConstant(Constants.REDUCER_LOOP);
    getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);

    // create a lambda function wraps while body(iterator)
    boolean newLexicalScope = this.scope.newLexicalScope;
    this.scope.newLexicalScope = true;
    {
      getCodeGeneratorWithTimes().onLambdaDefineStart(
          this.prevToken.withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
      ifStatement(true, false);
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
      getCodeGenerator().onMethodParameter(this.lookhead);
    }

    if (expectChar(';')) {
      // the statement is ended.
      getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
    } else {
      // create a lambda function wraps statements after while(statements)
      getCodeGeneratorWithTimes().onLambdaDefineStart(this.prevToken //
          .withMeta(Constants.SCOPE_META, this.scope.newLexicalScope) //
          .withMeta(Constants.INHERIT_ENV_META, true));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
      if (statements() == StatementType.Empty) {
        getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
      }
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
    }
    getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);

    // call __reducer_callcc(LOOP, iterator, statements)
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
    // restore newLexicalScope
    this.scope.newLexicalScope = newLexicalScope;

  }

  private void letStatement() {
    move(true);
    checkVariableName();
    getCodeGenerator().onConstant(this.lookhead);
    move(true);
    if (!expectChar('=')) {
      reportSyntaxError("expect '='");
    }
    move(true);
    if (statement() == StatementType.Empty) {
      reportSyntaxError("invalid value to define");
    }
    ensureFeatureEnabled(Feature.Assignment);
    getCodeGeneratorWithTimes().onAssignment(this.lookhead.withMeta(Constants.DEFINE_META, true));
    if (!expectChar(';')) {
      reportSyntaxError("missing ';' for let statement");
    }
    move(true);
  }

  private void fnStatement() {
    move(true);

    checkVariableName();
    getCodeGeneratorWithTimes().onConstant(this.lookhead);
    move(true);
    if (!expectChar('(')) {
      reportSyntaxError("expect '(' after function name");
    }
    lambda(true);
    ensureFeatureEnabled(Feature.Assignment);
    getCodeGeneratorWithTimes().onAssignment(currentToken().withMeta(Constants.DEFINE_META, true));
  }

  private Token<?> currentToken() {
    Token<?> token = this.lookhead;
    if (token == null) {
      token = new CharToken((char) -1, this.lexer.getCurrentIndex());
    }
    return token;
  }


  private boolean scopeStatement() {
    /**
     * <pre>
     *   {
     *    ...body...
     *   }
     *   ...statements...
     * </pre>
     *
     * =>
     *
     * <pre>
     *  __if_callcc((lambda() -> ...body... end)(),  lambda() ->
     *     ...statements...
     *  end);
     * </pre>
     */

    boolean hasReturn = false;
    boolean newLexicalScope = this.scope.newLexicalScope;
    this.scope.newLexicalScope = true;
    // prepare to call __if_callcc(result, statements)
    getCodeGeneratorWithTimes().onMethodName(Constants.IfReturnFn);

    // Create a lambda to wrap the scope body.
    {
      move(true);
      this.scope.enterBrace();
      getCodeGeneratorWithTimes().onLambdaDefineStart(
          this.prevToken.withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
      hasReturn = statements() == StatementType.Return;
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
      getCodeGeneratorWithTimes().onMethodName(anonymousMethodName());
      getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);

      if (!expectChar('}')) {
        reportSyntaxError("missing '}' to close scope");
      }
      move(true);
      this.scope.leaveBrace();
      getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
    }

    if (expectChar(';')) {
      // the statement is ended.
      getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
    } else {
      // create a lambda function wraps statements after scope statement (statements)
      getCodeGeneratorWithTimes().onLambdaDefineStart(this.prevToken //
          .withMeta(Constants.SCOPE_META, this.scope.newLexicalScope) //
          .withMeta(Constants.INHERIT_ENV_META, true));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
      if (statements() == StatementType.Empty) {
        getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
      }
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
    }
    getCodeGenerator().onMethodParameter(this.lookhead);

    // call __if_callcc(result, statements)
    getCodeGenerator().onMethodInvoke(this.lookhead);
    this.scope.newLexicalScope = newLexicalScope;

    return hasReturn;
  }

  private void tryStatement() {
    getCodeGeneratorWithTimes().onMethodName(new Variable("__try", -1));
    move(true);
    if (!expectChar('{')) {
      reportSyntaxError("expect '{' after try");
    }
    move(true);

    boolean newLexicalScope = this.scope.newLexicalScope;
    this.scope.newLexicalScope = true;
    // create a lambda function wraps try body
    {

      getCodeGeneratorWithTimes().onLambdaDefineStart(this.prevToken //
          .withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
      statements();
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
      getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
    }
    if (!expectChar('}')) {
      reportSyntaxError("missing '}' for try body");
    }
    move(true);
    if (this.lookhead != Variable.CATCH) {
      reportSyntaxError("expect catch clauses after try body");
    }
    boolean hasCatch = false;
    boolean hasFinally = false;

    while (this.lookhead == Variable.CATCH) {
      if (!hasCatch) {
        // create a handler list.
        getCodeGeneratorWithTimes().onMethodName(new Variable("seq.list", -1));
        hasCatch = true;
      }

      move(true);
      // create a lambda function wraps catch handlers
      if (!expectChar('(')) {
        reportSyntaxError("expect '(' after catch");
      }
      move(true);
      if (this.lookhead == null || this.lookhead.getType() != TokenType.Variable) {
        reportSyntaxError("invalid exception class name");
      }
      checkVariableName();
      Token<?> exceptionClass = this.lookhead;
      move(true);
      if (this.lookhead == null || this.lookhead.getType() != TokenType.Variable) {
        reportSyntaxError("invalid bound variable name for exception");
      }
      checkVariableName();
      Token<?> boundVar = this.lookhead;
      move(true);

      if (!expectChar(')')) {
        reportSyntaxError("missing ')' for catch caluse");
      }
      move(true);

      if (!expectChar('{')) {
        reportSyntaxError("missing '{' for catch block");
      }
      move(true);
      {
        // create a catch handler
        getCodeGeneratorWithTimes().onMethodName(new Variable("__catch_handler", -1));
        getCodeGeneratorWithTimes().onLambdaDefineStart(this.prevToken //
            .withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
        getCodeGeneratorWithTimes().onLambdaArgument(boundVar);
        getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
        statements();
        getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
        getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
        getCodeGeneratorWithTimes().onConstant(exceptionClass);
        getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
        getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
      }
      getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
      if (!expectChar('}')) {
        reportSyntaxError("missing '}' for to complete catch block");
      }
      move(true);
    }

    if (hasCatch) {
      // Invoke seq.list to create handler list
      getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
      getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
    }

    if (this.lookhead == Variable.FINALLY) {
      hasFinally = true;
      move(true);
      if (!expectChar('{')) {
        reportSyntaxError("missing '{' for finally block");
      }
      move(true);
      // create a lambda to
      getCodeGeneratorWithTimes().onLambdaDefineStart(this.prevToken //
          .withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
      statements();
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
      if (!expectChar('}')) {
        reportSyntaxError("missing '}' for finally block");
      }
      move(true);
    } else {
      getCodeGeneratorWithTimes().onConstant(Variable.NIL);
    }

    if (!hasCatch && !hasFinally) {
      reportSyntaxError("missing catch or finally blocks for catch");
    }

    if (expectChar(';')) {
      // The statement is ended.
      getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
    } else {
      // create a lambda function wraps statements after try..catch
      getCodeGeneratorWithTimes().onLambdaDefineStart(this.prevToken //
          .withMeta(Constants.SCOPE_META, this.scope.newLexicalScope) //
          .withMeta(Constants.INHERIT_ENV_META, true));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
      if (statements() == StatementType.Empty) {
        getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
      }
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
    }

    getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);

    this.scope.newLexicalScope = newLexicalScope;
    getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
  }

  private StatementType statement() {
    if (this.lookhead == Variable.IF) {
      ensureFeatureEnabled(Feature.If);
      if (ifStatement(false, false)) {
        return StatementType.Return;
      } else {
        return StatementType.Other;
      }
    } else if (this.lookhead == Variable.FOR) {
      ensureFeatureEnabled(Feature.ForLoop);
      forStatement();
      return StatementType.Other;
    } else if (this.lookhead == Variable.RETURN) {
      ensureFeatureEnabled(Feature.Return);
      returnStatement();
      return StatementType.Return;
    } else if (this.lookhead == Variable.BREAK) {
      breakStatement();
      return StatementType.Return;
    } else if (this.lookhead == Variable.CONTINUE) {
      continueStatement();
      return StatementType.Return;
    } else if (this.lookhead == Variable.LET) {
      ensureFeatureEnabled(Feature.Let);
      letStatement();
      return StatementType.Other;
    } else if (this.lookhead == Variable.WHILE) {
      ensureFeatureEnabled(Feature.WhileLoop);
      whileStatement();
      return StatementType.Other;
    } else if (this.lookhead == Variable.FN) {
      ensureFeatureEnabled(Feature.Fn);
      fnStatement();
      return StatementType.Other;
    } else if (this.lookhead == Variable.TRY) {
      tryStatement();
      return StatementType.Other;
    } else if (expectChar('{')) {
      ensureFeatureEnabled(Feature.LexicalScope);
      if (scopeStatement()) {
        return StatementType.Return;
      } else {
        return StatementType.Other;
      }
    } else {
      if (ternary()) {
        return StatementType.Ternary;
      } else {
        return StatementType.Empty;
      }
    }
  }


  /**
   * <pre>
   *  for x in coll {
   *     ...body...
   *  }
   *  ...statements...
   * </pre>
   *
   * ===>
   *
   * <pre>
   *  __reducer_callcc(seq, lambda(x) ->
   *      ...body...
   *  end, lambda()- >
   *       ...statements...
   *  end);
   * </pre>
   */
  private void forStatement() {
    move(true);

    Token<?> reducerArg = this.lookhead;
    checkVariableName();
    move(true);
    if (this.lookhead == Variable.IN) {
      move(true);
      // prepare to call __reducer_callcc(seq, iterator, statements)
      {
        getCodeGeneratorWithTimes().onMethodName(Constants.ReducerFn);
        // The seq
        if (!ternary()) {
          reportSyntaxError("missing collection");
        }
        getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
      }
      if (expectChar('{')) {
        move(true);
        this.scope.enterBrace();

        boolean newLexicalScope = this.scope.newLexicalScope;
        this.scope.newLexicalScope = true;

        // create a lambda function wraps for-loop body(iterator)
        {
          getCodeGeneratorWithTimes().onLambdaDefineStart(
              this.prevToken.withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
          getCodeGeneratorWithTimes().onLambdaArgument(reducerArg);
          getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
          statements();
          getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
          getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
        }

        if (expectChar('}')) {
          move(true);
          this.scope.leaveBrace();
        } else {
          reportSyntaxError("missing '}' in for-loop");
        }

        if (expectChar(';')) {
          // The statement is ended.
          getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
        } else {
          // create a lambda function wraps statements after for-loop(statements)
          getCodeGeneratorWithTimes().onLambdaDefineStart(this.prevToken //
              .withMeta(Constants.SCOPE_META, this.scope.newLexicalScope) //
              .withMeta(Constants.INHERIT_ENV_META, true));
          getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
          if (statements() == StatementType.Empty) {
            getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
          }
          getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
        }
        getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
        // call __reducer_callcc(seq, iterator, statements)
        getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
        // restore newLexicalScope
        this.scope.newLexicalScope = newLexicalScope;
      } else {
        reportSyntaxError("expect '{' in for-loop");
      }
    } else {
      reportSyntaxError("expect 'in' keyword while using for-loop");
    }
  }


  private StatementType statements() {
    if (this.lookhead == null) {
      return StatementType.Empty;
    }

    StatementType stmtType = statement();
    ensureDepthState();
    while (expectChar(';') || stmtType == StatementType.Other || stmtType == StatementType.Return) {

      ensureNoStatementAfterReturn(stmtType);

      if (this.lookhead != null && this.lookhead != Variable.END && !expectChar('}')) {
        getCodeGeneratorWithTimes().onTernaryEnd(this.lookhead);
      }

      if (expectChar(';')) {
        move(true);
      }

      if (this.lookhead == null) {
        break;
      }

      StatementType nextStmtType = statement();
      if (nextStmtType == StatementType.Empty) {
        break;
      }
      stmtType = nextStmtType;
      ensureDepthState();
    }
    ensureNoStatementAfterReturn(stmtType);

    return stmtType;
  }

  private void ensureNoStatementAfterReturn(final StatementType statementType) {
    if (statementType == StatementType.Return && this.lookhead != null) {
      if (this.lookhead != Variable.END && !expectChar('}')) {
        reportSyntaxError("unreachable code");
      }
    }
  }

  /**
   * <pre>
   *  if(test) {
   *     ...if-body...
   *  }else {
   *     ...else-body...
   *  }
   *  ...statements...
   * </pre>
   *
   * ===>
   *
   * <pre>
   *  __if_callcc(test ? (lambda() -> ...if-body... end)() :  (lambda() -> ...else-body... end)(),
   *   lambda()- >
   *       ...statements...
   *  end);
   * </pre>
   */
  private boolean ifStatement(final boolean isWhile, final boolean isElsif) {
    if (!isWhile) {
      move(true);
    }
    boolean ifBodyHasReturn = false;
    boolean elseBodyHasReturn = false;
    boolean newLexicalScope = this.scope.newLexicalScope;
    this.scope.newLexicalScope = true;
    // prepare to call __if_callcc(result, statements)
    getCodeGeneratorWithTimes().onMethodName(Constants.IfReturnFn);

    {
      if (!ternary()) {
        reportSyntaxError("missing test statement for if");
      }

      getCodeGeneratorWithTimes().onTernaryBoolean(this.lookhead);
      if (expectChar('{')) {
        move(true);
        this.scope.enterBrace();
        getCodeGeneratorWithTimes().onLambdaDefineStart(
            this.prevToken.withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
        getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
        ifBodyHasReturn = statements() == StatementType.Return;
        getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
        getCodeGeneratorWithTimes().onMethodName(anonymousMethodName());
        getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
        getCodeGeneratorWithTimes().onTernaryLeft(this.lookhead);
      } else {
        reportSyntaxError("expect '{' for " + getLoopKeyword(isWhile) + " statement");
      }
      if (!expectChar('}')) {
        reportSyntaxError("missing '}' to close " + getLoopKeyword(isWhile) + " body");
      }
      this.scope.leaveBrace();
      move(true);

      elseBodyHasReturn = elseStatement(isWhile, ifBodyHasReturn);
      getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
    }

    {
      //
      if (isWhile || isElsif) {
        // Load ReducerEmptyVal directly.
        getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
      } else {

        if (expectChar(';')) {
          // the statement is ended.
          getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
        } else {
          // create a lambda function wraps statements after if statement (statements)
          getCodeGeneratorWithTimes().onLambdaDefineStart(this.prevToken //
              .withMeta(Constants.SCOPE_META, this.scope.newLexicalScope) //
              .withMeta(Constants.INHERIT_ENV_META, true));
          getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
          if (statements() == StatementType.Empty) {
            getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
          } else {
            if (ifBodyHasReturn && elseBodyHasReturn && !isElsif) {
              reportSyntaxError("unreachable code");
            }
          }
          getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
        }
      }
      getCodeGenerator().onMethodParameter(this.lookhead);
      // call __if_callcc(result, statements)
      getCodeGenerator().onMethodInvoke(this.lookhead);
      this.scope.newLexicalScope = newLexicalScope;
    }

    return ifBodyHasReturn && elseBodyHasReturn;
  }

  private String getLoopKeyword(final boolean isWhile) {
    return isWhile ? "while" : "if";
  }

  private boolean elseStatement(final boolean isWhile, final boolean ifBodyHasReturn) {
    if (isWhile) {
      // Call __reducer_break(nil)
      final CodeGenerator cg = getCodeGeneratorWithTimes();
      cg.onMethodName(Constants.ReducerBreakFn);
      cg.onConstant(Variable.NIL);
      cg.onMethodParameter(this.lookhead);
      cg.onMethodInvoke(this.lookhead);
      cg.onTernaryRight(this.lookhead);
      return false;
    }

    if (expectChar(';')) {
      return withoutElse();
    }

    boolean hasReturn = false;
    boolean hasElsif = this.lookhead == Variable.ELSIF;
    boolean hasElse = this.lookhead == Variable.ELSE;
    if (this.lookhead != null && (hasElse || hasElsif || ifBodyHasReturn)) {
      if (hasElse) {
        move(true);
        if (expectChar('{')) {
          this.scope.enterBrace();
          move(true);
          hasReturn = elseBody(false);
          if (expectChar('}')) {
            this.scope.leaveBrace();
            move(true);
          } else {
            reportSyntaxError("missing '}' to close 'else' body");
          }
        } else {
          reportSyntaxError("expect '{' for else statement");
        }
      } else if (hasElsif) {
        hasReturn = ifStatement(false, true);
        getCodeGenerator().onTernaryRight(this.lookhead);
      } else if (ifBodyHasReturn) {
        hasReturn = elseBody(true);
      } else {
        return withoutElse();
      }
      return hasReturn;
    } else {
      // Missing else statement, always nil.
      return withoutElse();
    }

  }

  private boolean withoutElse() {
    final CodeGenerator cg = getCodeGeneratorWithTimes();
    cg.onConstant(Variable.NIL);
    cg.onTernaryRight(this.lookhead);
    return false;
  }

  private boolean elseBody(final boolean inheritEnv) {
    getCodeGeneratorWithTimes().onLambdaDefineStart(this.lookhead //
        .withMeta(Constants.SCOPE_META, this.scope.newLexicalScope) //
        .withMeta(Constants.INHERIT_ENV_META, inheritEnv) //
    );
    getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
    boolean hasReturn = statements() == StatementType.Return;
    getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
    getCodeGeneratorWithTimes().onMethodName(anonymousMethodName());
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
    getCodeGeneratorWithTimes().onTernaryRight(this.lookhead);
    return hasReturn;
  }

  private DelegateToken anonymousMethodName() {
    return new DelegateToken(this.lookhead, DelegateTokenType.Method_Name);
  }


  private void ensureDepthState() {
    DepthState state = this.scope.depthState.peekLast();
    if (state != null) {
      back();
      switch (state) {
        case Parent:
          if (this.scope.parenDepth > 0) {
            reportSyntaxError("insert ')' to complete statement");
          }
          break;
        case Bracket:
          if (this.scope.bracketDepth > 0) {
            reportSyntaxError("insert ']' to complete statement");
          }
          break;
        case Lambda:
          if (this.scope.lambdaDepth > 0) {
            reportSyntaxError("insert 'end' to complete lambda statement");
          }
          break;
        case Brace:
          if (this.scope.braceDepth > 0) {
            reportSyntaxError("insert '}' to complete statement");
          }
          break;
      }
    }
  }

  public static boolean isConstant(final Token<?> token) {
    switch (token.getType()) {
      case Number:
      case Pattern:
      case String:
        return true;
      default:
        return false;
    }
  }

  public static boolean isLiteralToken(final Token<?> token) {
    switch (token.getType()) {
      case Variable:
        return token == Variable.TRUE || token == Variable.FALSE || token == Variable.NIL;
      case Char:
      case Number:
      case Pattern:
      case String:
        return true;
      default:
        return false;
    }
  }

  private int getCGTimes;

  private final CodeGenerator getCodeGeneratorWithTimes() {
    this.getCGTimes++;
    return this.codeGenerator;
  }

}
