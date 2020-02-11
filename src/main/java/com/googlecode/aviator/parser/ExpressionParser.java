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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.code.CodeGenerator;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.lexer.ExpressionLexer;
import com.googlecode.aviator.lexer.token.CharToken;
import com.googlecode.aviator.lexer.token.DelegateToken;
import com.googlecode.aviator.lexer.token.DelegateToken.DelegateTokenType;
import com.googlecode.aviator.lexer.token.PatternToken;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Token.TokenType;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.utils.Constants;


/**
 * Syntex parser for expression
 *
 * @author dennis
 *
 */
public class ExpressionParser implements Parser {
  private final ExpressionLexer lexer;

  static final Set<String> RESERVED_WORDS = new HashSet<>();
  static {
    RESERVED_WORDS.add(Variable.TRUE.getLexeme());
    RESERVED_WORDS.add(Variable.FALSE.getLexeme());
    RESERVED_WORDS.add(Variable.NIL.getLexeme());
  }
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


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.parser.Parser#getCodeGenerator()
   */
  @Override
  public CodeGenerator getCodeGenerator() {
    return this.codeGenerator;
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
    if (this.lookhead == null) {
      throw new ExpressionSyntaxErrorException("Blank expression");
    }
    setCodeGenerator(codeGenerator);
    getCodeGeneratorWithTimes().setParser(this);
  }

  public void returnStatement() {
    move(true);
    getCodeGeneratorWithTimes().onTernaryEnd(this.lookhead);
    if (this.scope.newLexicalScope) {
      getCodeGeneratorWithTimes().onMethodName(Constants.ReducerReturnFn);
      ternary();
      getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
      getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
    } else {
      ternary();
    }
    if (!expectChar(';')) {
      reportSyntaxError("Missing ';' for return statement");
    }
    move(true);
  }

  public void ternary() {
    join();
    if (this.lookhead == null || expectChar(':') || expectChar(',')) {
      return;
    }
    if (expectChar('?')) {
      move(true);
      getCodeGeneratorWithTimes().onTernaryBoolean(this.lookhead);
      ternary();
      if (expectChar(':')) {
        move(true);
        getCodeGeneratorWithTimes().onTernaryLeft(this.lookhead);
        ternary();
        getCodeGeneratorWithTimes().onTernaryRight(this.lookhead);
      } else {
        reportSyntaxError("expect ':'");
      }
    }
  }


  public void join() {
    and();
    while (true) {
      if (isJoinToken()) {
        getCodeGeneratorWithTimes().onJoinLeft(this.lookhead);
        move(true);
        if (isJoinToken()) {
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


  private boolean isJoinToken() {
    return expectChar('|');
  }


  private boolean expectChar(final char ch) {
    if (this.lookhead == null) {
      return false;
    }
    return this.lookhead.getType() == TokenType.Char && ((CharToken) this.lookhead).getCh() == ch;
  }


  private boolean isAndToken() {
    return expectChar('&');
  }


  public void bitOr() {
    xor();
    while (true) {
      if (isJoinToken()) {
        move(true);
        if (isJoinToken()) {
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
      if (isAndToken()) {
        move(true);
        if (isAndToken()) {
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
      if (isAndToken()) {
        getCodeGeneratorWithTimes().onAndLeft(this.lookhead);
        move(true);
        if (isAndToken()) {
          move(true);
          bitOr();
          getCodeGeneratorWithTimes().onAndRight(this.lookhead);
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
        enterParen();
        getCodeGeneratorWithTimes().onMethodName(anonymousMethodName());
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
          getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
          if (this.captureFuncArgs) {
            params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
          }

          while (expectChar(',')) {
            move(true);
            lastTokenIndex = getLookheadStartIndex();
            ternary();
            getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
            if (this.captureFuncArgs) {
              params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
            }
          }
        }
        if (expectChar(')')) {
          getCodeGeneratorWithTimes()
              .onMethodInvoke(this.lookhead.withMeta(Constants.PARAMS_META, params));
          leaveParen();
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
      reportSyntaxError("Illegal factor");
    }
    if (this.lookhead == Variable.END) {
      return;
    }
    if (expectChar('(')) {
      enterParen();
      move(true);
      ternary();
      if (expectChar(')')) {
        move(true);
        leaveParen();
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
          reportSyntaxError("Unexpect char '" + charToken.getCh() + "'");
        }
        // make it as variable
        this.lookhead = new Variable(charToken.getLexeme(), charToken.getStartIndex());
      }
      move(true);
      // function
      Token<?> prev = this.prevToken;
      if (prev.getType() == TokenType.Variable && expectChar('(')) {
        if (prev == Variable.LAMBDA) {
          lambda();
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
      reportSyntaxError("invalid value");
    }

  }


  private void lambda() {
    enterLambda();
    getCodeGeneratorWithTimes().onLambdaDefineStart(this.prevToken);
    enterParen();
    move(true);
    if (!expectChar(')')) {
      lambdaArgument();

      while (expectChar(',')) {
        move(true);
        lambdaArgument();
      }
    }
    if (expectChar(')')) {
      leaveParen();
      move(true);
      if (expectChar('-')) {
        move(true);
        if (expectChar('>')) {
          getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
          move(true);
          statements();
          if (this.lookhead != null && this.lookhead == Variable.END) {
            getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
            leaveLambda();

            move(true);
          } else {
            reportSyntaxError("Expect lambda 'end', but is: '" + currentTokenLexeme() + "'");
          }
        } else {
          // TODO may be a method call lambda(x,y)
          reportSyntaxError("Expect lambda body, but is: '" + currentTokenLexeme() + "'");
        }
      } else {
        // TODO may be a method call lambda(x,y)
        reportSyntaxError("Expect lambda body, but is: '" + currentTokenLexeme() + "'");
      }
    }
  }

  private void leaveLambda() {
    this.scope.lambdaDepth--;
    this.scope.depthState.removeLast();
  }

  private void enterLambda() {
    this.scope.lambdaDepth++;
    this.scope.depthState.add(DepthState.Lambda);
  }

  private String currentTokenLexeme() {
    return this.lookhead == null ? "END_OF_STRING" : this.lookhead.getLexeme();
  }


  private void lambdaArgument() {
    if (this.lookhead.getType() == TokenType.Variable) {
      if (!isJavaIdentifier(this.lookhead.getLexeme())) {
        reportSyntaxError("Illegal argument name: " + currentTokenLexeme() + ",index="
            + this.lookhead.getStartIndex());
      }
      getCodeGeneratorWithTimes().onLambdaArgument(this.lookhead);
      move(true);
    } else {
      reportSyntaxError("Expect argument name,but is: " + currentTokenLexeme() + ",index="
          + this.lookhead.getStartIndex());
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
    enterBracket();
    if (RESERVED_WORDS.contains(this.prevToken.getLexeme())) {
      reportSyntaxError(this.prevToken.getLexeme() + " could not use [] operator");
    }

    ternary();
    if (expectChar(']')) {
      leaveBracket();
      move(true);
      getCodeGeneratorWithTimes().onArrayIndexEnd(this.lookhead);
    }
  }

  private void leaveBracket() {
    this.scope.bracketDepth--;
    this.scope.depthState.removeLast();
  }

  private void enterBracket() {
    this.scope.bracketDepth++;
    this.scope.depthState.add(DepthState.Bracket);
  }


  private void checkVariableName() {
    if (!((Variable) this.lookhead).isQuote()) {
      String[] names = this.lookhead.getLexeme().split("\\.");
      for (String name : names) {
        if (!isJavaIdentifier(name)) {
          reportSyntaxError(
              "Illegal identifier " + name + ",index=" + this.lookhead.getStartIndex());
        }
      }
    }
  }

  private void method() {
    if (expectChar('(')) {

      enterParen();
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
          ternary();
          getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
          if (this.captureFuncArgs) {
            params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
          }
        }
      }
      if (expectChar(')')) {
        getCodeGeneratorWithTimes()
            .onMethodInvoke(this.lookhead.withMeta(Constants.PARAMS_META, params));
        leaveParen();
        move(true);
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
    StringBuffer sb = new StringBuffer();
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
      reportSyntaxError("invalid regular pattern");
    }
    getCodeGeneratorWithTimes().onConstant(new PatternToken(sb.toString(), startIndex));
    move(true);
  }


  private void reportSyntaxError(final String message) {
    int index =
        this.lookhead != null && this.lookhead.getStartIndex() > 0 ? this.lookhead.getStartIndex()
            : this.lexer.getCurrentIndex();

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
      if (t.getClassName().equals(this.getClass().getName())) {
        continue;
      }
      filteredTraces.add(t);
    }
    e.setStackTrace(filteredTraces.toArray(new StackTraceElement[filteredTraces.size()]));
    throw e;
  }



  public void move(final boolean analyse) {
    if (this.lookhead != null) {
      this.prevToken = this.lookhead;
      this.lookhead = this.lexer.scan(analyse);
    } else {
      reportSyntaxError("Illegal expression");
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
        reportSyntaxError("Unexpect token '" + currentTokenLexeme()
            + "', maybe forget to insert ';' to complete last expression ");
      } else {
        reportSyntaxError("Unexpect token '" + currentTokenLexeme() + "'");
      }
    }
    return getCodeGeneratorWithTimes().getResult();
  }

  private static enum StatementType {
    If, For, Ternary, Return, Empty, Let, While
  }

  private void breakStatement() {
    if (!this.scope.newLexicalScope) {
      reportSyntaxError("break only can be used in for-loop");
    }
    move(true);
    getCodeGeneratorWithTimes().onMethodName(Constants.ReducerBreakFn);
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
    if (!expectChar(';')) {
      reportSyntaxError("Missing ';' for break");
    }
    move(true);
  }

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
      reportSyntaxError("Missing ';' for continue");
    }
    move(true);
  }

  private void whileStatement() {
    move(true);

    /**
     * <pre>
     *  while(test) {
     *     body
     *  }
     *  ...statements...
     * </pre>
     *
     * =>
     *
     * <pre>
     *  __reducer(__reducer_loop, lambda() ->
     *       if(test) {
     *          body
     *       }else {
     *          break;
     *       }
     *  end, lambda()- >
     *       ...statements...
     *  end);
     * </pre>
     */

    if (expectChar('(')) {
      // move(true);

      // prepare to call __reducer(LOOP, iterator, statements)
      getCodeGeneratorWithTimes().onMethodName(Constants.ReducerFn);
      getCodeGeneratorWithTimes().onConstant(new Variable("__reducer_loop", -1));
      getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);

      // create a lambda function wraps for-loop body(iterator)
      boolean newLexicalScope = this.scope.newLexicalScope;
      this.scope.newLexicalScope = true;
      {
        getCodeGeneratorWithTimes().onLambdaDefineStart(
            this.prevToken.withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
        getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
        ifStatement(true);
        getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
        getCodeGenerator().onMethodParameter(this.lookhead);
      }

      // create a lambda function wraps statements after for-loop(statements)
      {
        getCodeGeneratorWithTimes().onLambdaDefineStart(
            this.prevToken.withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
        getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
        if (statements() == StatementType.Empty) {
          getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
        }
        getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
        getCodeGenerator().onMethodParameter(this.lookhead);
      }
      // call __reducer(seq, iterator, statements)
      getCodeGenerator().onMethodInvoke(this.lookhead);
      // restore inForLoop
      this.scope.newLexicalScope = newLexicalScope;
    } else {
      reportSyntaxError("Expect '('  after while keyword");
    }

  }

  private void letStatement() {
    move(true);
    // TODO assert variable
    getCodeGenerator().onConstant(this.lookhead);
    move(true);
    if (!expectChar('=')) {
      reportSyntaxError("expect '='");
    }
    move(true);
    ternary();
    getCodeGeneratorWithTimes().onAssignment(this.lookhead.withMeta("let", true));
    if (!expectChar(';')) {
      reportSyntaxError("Missing ';' for let statement");
    }
    move(true);
  }


  private StatementType statement() {
    if (this.lookhead == Variable.IF) {
      if (ifStatement(false)) {
        return StatementType.Return;
      } else {
        return StatementType.If;
      }
    } else if (this.lookhead == Variable.FOR) {
      forStatement();
      return StatementType.For;
    } else if (this.lookhead == Variable.RETURN) {
      returnStatement();
      return StatementType.Return;
    } else if (this.lookhead == Variable.BREAK) {
      breakStatement();
      return StatementType.Return;
    } else if (this.lookhead == Variable.CONTINUE) {
      continueStatement();
      return StatementType.Return;
    } else if (this.lookhead == Variable.LET) {
      letStatement();
      return StatementType.Let;
    } else if (this.lookhead == Variable.WHILE) {
      whileStatement();
      return StatementType.While;
    } else {
      int cgTimes = this.getCGTimes;
      ternary();
      return cgTimes == this.getCGTimes ? StatementType.Empty : StatementType.Ternary;
    }
  }

  private void forStatement() {
    move(true);

    Token<?> reducerArg = this.lookhead;
    // TODO assert reducerArg
    move(true);
    if (this.lookhead == Variable.IN) {
      move(true);
      Variable seqVar = new Variable(AviatorRuntimeJavaType.genName(), -1);
      getCodeGeneratorWithTimes().onConstant(seqVar);
      ternary();
      if (expectChar('{')) {
        move(true);
        enterBrace();
        {
          // assign seq to a temp variable.
          getCodeGeneratorWithTimes().onAssignment(this.lookhead);
          getCodeGeneratorWithTimes().onTernaryEnd(this.lookhead);
        }

        // prepare to call __reducer(seq, iterator, statements)
        getCodeGeneratorWithTimes().onMethodName(Constants.ReducerFn);
        getCodeGeneratorWithTimes().onConstant(seqVar);
        getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);

        // create a lambda function wraps for-loop body(iterator)
        boolean newLexicalScope = this.scope.newLexicalScope;
        this.scope.newLexicalScope = true;
        {
          getCodeGeneratorWithTimes().onLambdaDefineStart(
              this.prevToken.withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
          getCodeGeneratorWithTimes().onLambdaArgument(reducerArg);
          getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
          statements();
          getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
          getCodeGenerator().onMethodParameter(this.lookhead);
        }

        if (expectChar('}')) {
          move(true);
          leaveBrace();
        } else {
          reportSyntaxError("Missing '}' in for-loop");
        }

        // create a lambda function wraps statements after for-loop(statements)
        {
          getCodeGeneratorWithTimes().onLambdaDefineStart(
              this.prevToken.withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
          getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
          if (statements() == StatementType.Empty) {
            getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
          }
          getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
          getCodeGenerator().onMethodParameter(this.lookhead);
        }
        // call __reducer(seq, iterator, statements)
        getCodeGenerator().onMethodInvoke(this.lookhead);
        // restore inForLoop
        this.scope.newLexicalScope = newLexicalScope;
      } else {
        reportSyntaxError("Expect '{' in for-loop");
      }
    } else {
      reportSyntaxError("Expect 'in' keyword while using for-loop");
    }
  }


  private StatementType statements() {
    if (this.lookhead == null) {
      return StatementType.Empty;
    }

    StatementType stmtType = statement();
    ensureDepthState();
    while (expectChar(';') || stmtType == StatementType.If || stmtType == StatementType.For
        || stmtType == StatementType.Return || stmtType == StatementType.Let
        || stmtType == StatementType.While) {

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
        reportSyntaxError("unreachable code after return");
      }
    }
  }

  private boolean ifStatement(final boolean forWhile) {
    if (!forWhile) {
      move(true);
    }
    boolean ifBodyHasReturn = false;
    boolean newLexicalScope = this.scope.newLexicalScope;
    this.scope.newLexicalScope = true;
    // prepare to call __if_return(result, statements)
    getCodeGeneratorWithTimes().onMethodName(Constants.IfReturnFn);
    try {
      if (expectChar('(')) {
        enterParen();
        move(true);
        ternary();
        if (!expectChar(')')) {
          reportSyntaxError("Missing ')' to close 'if' test statement");
        }
        move(true);
        leaveParen();
        getCodeGeneratorWithTimes().onTernaryBoolean(this.lookhead);
        if (expectChar('{')) {
          move(true);
          enterBrace();
          getCodeGeneratorWithTimes().onLambdaDefineStart(
              this.prevToken.withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
          getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
          ifBodyHasReturn = statements() == StatementType.Return;
          getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
          getCodeGeneratorWithTimes().onMethodName(anonymousMethodName());
          getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
          getCodeGeneratorWithTimes().onTernaryLeft(this.lookhead);
        } else {
          reportSyntaxError("Expect '{' for if statement");
        }
        if (!expectChar('}')) {
          reportSyntaxError("Missing '}' to close 'if' body");
        }
        leaveBrace();
        move(true);
      } else {
        reportSyntaxError("Expect '(' after if for test statement");
      }
      boolean elseBodyHasReturn = elseStatement(forWhile, ifBodyHasReturn);
      return ifBodyHasReturn && elseBodyHasReturn;
    } finally {
      getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
      // create a lambda function wraps statements after if(statements)
      {
        getCodeGeneratorWithTimes().onLambdaDefineStart(
            this.prevToken.withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
        getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookhead);
        if (forWhile || statements() == StatementType.Empty) {
          getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
        }
        getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookhead);
        getCodeGenerator().onMethodParameter(this.lookhead);
      }

      // call __if_return(result, statements)
      getCodeGenerator().onMethodInvoke(this.lookhead);
      this.scope.newLexicalScope = newLexicalScope;
    }
  }

  private void leaveBrace() {
    this.scope.braceDepth--;
    this.scope.depthState.removeLast();
  }

  private void enterBrace() {
    this.scope.braceDepth++;
    this.scope.depthState.add(DepthState.Brace);
  }

  private void leaveParen() {
    this.scope.parenDepth--;
    this.scope.depthState.removeLast();
  }

  private void enterParen() {
    this.scope.parenDepth++;
    this.scope.depthState.add(DepthState.Parent);
  }

  private boolean elseStatement(final boolean forWhile, final boolean ifBodyHasReturn) {
    if (forWhile) {
      getCodeGeneratorWithTimes().onMethodName(Constants.ReducerBreakFn);
      getCodeGeneratorWithTimes().onConstant(Variable.NIL);
      getCodeGeneratorWithTimes().onMethodParameter(this.lookhead);
      getCodeGeneratorWithTimes().onMethodInvoke(this.lookhead);
      getCodeGeneratorWithTimes().onTernaryRight(this.lookhead);
      return false;
    }

    boolean hasReturn = false;
    boolean hasElse = this.lookhead == Variable.ELSE;
    if (hasElse || ifBodyHasReturn) {
      if (this.lookhead == Variable.ELSE) {
        move(true);
      }
      if (hasElse) {
        if (expectChar('{')) {
          enterBrace();
          move(true);
          hasReturn = elseBody();
          if (expectChar('}')) {
            leaveBrace();
            move(true);
          } else {
            reportSyntaxError("Missing '}' to close 'else' body");
          }
        } else {
          reportSyntaxError("Expect '{' for else statement");
        }
      } else if (ifBodyHasReturn) {
        hasReturn = elseBody();
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
    getCodeGeneratorWithTimes().onConstant(Variable.NIL);
    getCodeGeneratorWithTimes().onTernaryRight(this.lookhead);
    return false;
  }

  private boolean elseBody() {
    getCodeGeneratorWithTimes().onLambdaDefineStart(
        this.lookhead.withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
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
            reportSyntaxError("insert ')' to complete Expression");
          }
          break;
        case Bracket:
          if (this.scope.bracketDepth > 0) {
            reportSyntaxError("insert ']' to complete Expression");
          }
          break;
        case Lambda:
          if (this.scope.lambdaDepth > 0) {
            reportSyntaxError("insert 'end' to complete lambda Expression");
          }
          break;
        case Brace:
          if (this.scope.braceDepth > 0) {
            reportSyntaxError("insert '}' to complete lambda Expression");
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

  private CodeGenerator getCodeGeneratorWithTimes() {
    this.getCGTimes++;
    return this.codeGenerator;
  }

}
