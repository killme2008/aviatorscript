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
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedList;
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


/**
 * Syntex parser for expression
 *
 * @author dennis
 *
 */
public class ExpressionParser implements Parser {
  private final ExpressionLexer lexer;

  static final Set<String> RESERVED_WORDS = new HashSet<String>();
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

  private int parenDepth = 0;

  private int bracketDepth = 0;

  private int lambdaDepth = 0;

  private Deque<DepthState> depthState = new ArrayDeque<DepthState>();


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
  public ScopeInfo enterScope() {
    ScopeInfo info =
        new ScopeInfo(this.parenDepth, this.bracketDepth, this.lambdaDepth, this.depthState);
    this.parenDepth = 0;
    this.bracketDepth = 0;
    this.lambdaDepth = 0;
    this.depthState = new LinkedList<DepthState>();
    return info;
  }

  /**
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.parser.Parser#restoreScope(com.googlecode.aviator.parser.ExpressionParser.DepthInfo)
   */
  @Override
  public void restoreScope(final ScopeInfo info) {
    this.parenDepth = info.parenDepth;
    this.bracketDepth = info.bracketDepth;
    this.lambdaDepth = info.lambdaDepth;
    this.depthState = info.depthState;
  }

  public ExpressionParser(final AviatorEvaluatorInstance instance, final ExpressionLexer lexer,
      final CodeGenerator codeGenerator) {
    super();
    this.instance = instance;
    this.captureFuncArgs = instance.getOptionValue(Options.CAPTURE_FUNCTION_ARGS).bool;
    this.lexer = lexer;
    this.lookhead = this.lexer.scan();
    if (this.lookhead == null) {
      throw new ExpressionSyntaxErrorException("Blank expression");
    }
    this.codeGenerator = codeGenerator;
    this.codeGenerator.setParser(this);
  }


  public void ternary() {
    join();
    if (this.lookhead == null || expectChar(':') || expectChar(',')) {
      return;
    }
    if (expectChar('?')) {
      move(true);
      this.codeGenerator.onTernaryBoolean(this.lookhead);
      ternary();
      if (expectChar(':')) {
        move(true);
        this.codeGenerator.onTernaryLeft(this.lookhead);
        ternary();
        this.codeGenerator.onTernaryRight(this.lookhead);
      } else {
        reportSyntaxError("expect ':'");
      }
    }
  }


  public void join() {
    and();
    while (true) {
      if (isJoinToken()) {
        this.codeGenerator.onJoinLeft(this.lookhead);
        move(true);
        if (isJoinToken()) {
          move(true);
          and();
          this.codeGenerator.onJoinRight(this.lookhead);
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
        this.codeGenerator.onBitOr(this.lookhead);
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
        this.codeGenerator.onBitXor(this.lookhead);
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
        this.codeGenerator.onBitAnd(this.lookhead);
      } else {
        break;
      }
    }
  }


  public void and() {
    bitOr();
    while (true) {
      if (isAndToken()) {
        this.codeGenerator.onAndLeft(this.lookhead);
        move(true);
        if (isAndToken()) {
          move(true);
          bitOr();
          this.codeGenerator.onAndRight(this.lookhead);
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
          this.codeGenerator.onEq(this.lookhead);
        } else if (expectChar('~')) {
          // It is a regular expression
          move(true);
          rel();
          this.codeGenerator.onMatch(this.lookhead);
        } else {
          // this.back();
          // assignment
          ternary();
          this.codeGenerator.onAssignment(this.lookhead);
          // this.reportSyntaxError("Aviator doesn't support assignment");
        }
      } else if (expectChar('!')) {
        move(true);
        if (expectChar('=')) {
          move(true);
          rel();
          this.codeGenerator.onNeq(this.lookhead);
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
          this.codeGenerator.onLe(this.lookhead);
        } else {
          expr();
          this.codeGenerator.onLt(this.lookhead);
        }
      } else if (expectChar('>')) {
        move(true);
        if (expectChar('=')) {
          move(true);
          expr();
          this.codeGenerator.onGe(this.lookhead);
        } else {
          expr();
          this.codeGenerator.onGt(this.lookhead);
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
          this.codeGenerator.onShiftLeft(this.lookhead);
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
            this.codeGenerator.onUnsignedShiftRight(this.lookhead);
          } else {
            expr();
            this.codeGenerator.onShiftRight(this.lookhead);
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
        this.codeGenerator.onAdd(this.lookhead);
      } else if (expectChar('-')) {
        move(true);
        term();
        this.codeGenerator.onSub(this.lookhead);
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
        this.codeGenerator.onMult(this.lookhead);
      } else if (expectChar('/')) {
        move(true);
        unary();
        this.codeGenerator.onDiv(this.lookhead);
      } else if (expectChar('%')) {
        move(true);
        unary();
        this.codeGenerator.onMod(this.lookhead);
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
        this.codeGenerator.onNot(this.lookhead);
      }
    } else if (expectChar('-')) {
      move(true);
      // check if it is a seq function call,"!" as variable
      if (expectChar(',') || expectChar(')')) {
        back();
        factor();
      } else {
        unary();
        this.codeGenerator.onNeg(this.lookhead);
      }
    } else if (expectChar('~')) {
      move(true);
      // check if it is a seq function call,"~" as variable
      if (expectChar(',') || expectChar(')')) {
        back();
        factor();
      } else {
        unary();
        this.codeGenerator.onBitNot(this.lookhead);
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
        // May be call chain, such as "s(1)(2)(3)"
        this.parenDepth++;
        this.depthState.add(DepthState.Parent);
        this.codeGenerator.onMethodName(new DelegateToken(this.lookhead.getStartIndex(),
            this.lookhead, DelegateTokenType.Method_Name));
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
          this.codeGenerator.onMethodParameter(this.lookhead);
          if (this.captureFuncArgs) {
            params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
          }

          while (expectChar(',')) {
            move(true);
            lastTokenIndex = getLookheadStartIndex();
            ternary();
            this.codeGenerator.onMethodParameter(this.lookhead);
            if (this.captureFuncArgs) {
              params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
            }
          }
        }
        if (expectChar(')')) {
          this.parenDepth--;
          this.depthState.removeLast();
          move(true);
          this.codeGenerator.onMethodInvoke(this.lookhead, params);
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
      reportSyntaxError("invalid value");
    }
    if (expectChar('(')) {
      this.parenDepth++;
      this.depthState.add(DepthState.Parent);
      move(true);
      ternary();
      if (expectChar(')')) {
        move(true);
        this.parenDepth--;
        this.depthState.removeLast();
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
        this.codeGenerator.onConstant(prev);
      }
    } else if (expectChar('/')) {
      pattern();
    } else {
      reportSyntaxError("invalid value");
    }

  }


  private void lambda() {
    this.lambdaDepth++;
    this.depthState.add(DepthState.Lambda);
    this.codeGenerator.onLambdaDefineStart(this.prevToken);
    this.parenDepth++;
    this.depthState.add(DepthState.Parent);
    move(true);
    if (!expectChar(')')) {
      lambdaArgument();

      while (expectChar(',')) {
        move(true);
        lambdaArgument();
      }
    }
    if (expectChar(')')) {
      this.parenDepth--;
      this.depthState.removeLast();
      move(true);
      if (expectChar('-')) {
        move(true);
        if (expectChar('>')) {
          this.codeGenerator.onLambdaBodyStart(this.lookhead);
          move(true);
          statement();
          if (this.lookhead != null && this.lookhead.getType() == TokenType.Variable
              && this.lookhead.getLexeme().equals("end")) {
            this.codeGenerator.onLambdaBodyEnd(this.lookhead);
            this.lambdaDepth--;
            this.depthState.removeLast();

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

  private String currentTokenLexeme() {
    return this.lookhead == null ? "END_OF_STRING" : this.lookhead.getLexeme();
  }


  private void lambdaArgument() {
    if (this.lookhead.getType() == TokenType.Variable) {
      if (!isJavaIdentifier(this.lookhead.getLexeme())) {
        reportSyntaxError("Illegal argument name: " + currentTokenLexeme() + ",index="
            + this.lookhead.getStartIndex());
      }
      this.codeGenerator.onLambdaArgument(this.lookhead);
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
        this.codeGenerator.onArray(this.prevToken);
        move(true);
        hasArray = true;
      } else {
        move(true);
      }
      this.codeGenerator.onArrayIndexStart(this.prevToken);
      array();
    }
    if (!hasArray) {
      this.codeGenerator.onConstant(this.prevToken);
    }
  }


  private void array() {
    this.bracketDepth++;
    this.depthState.add(DepthState.Bracket);
    if (RESERVED_WORDS.contains(this.prevToken.getLexeme())) {
      throw new ExpressionSyntaxErrorException(
          this.prevToken.getLexeme() + " could not use [] operator");
    }

    ternary();
    if (expectChar(']')) {
      this.bracketDepth--;
      this.depthState.removeLast();
      move(true);
      this.codeGenerator.onArrayIndexEnd(this.lookhead);
    }
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

      this.parenDepth++;
      this.depthState.add(DepthState.Parent);
      this.codeGenerator.onMethodName(this.prevToken);
      move(true);
      int paramIndex = 0;
      List<FunctionArgument> params = null;
      if (this.captureFuncArgs) {
        params = new ArrayList<>();
      }
      int lastTokenIndex = getLookheadStartIndex();
      if (!expectChar(')')) {
        ternary();
        this.codeGenerator.onMethodParameter(this.lookhead);
        if (this.captureFuncArgs) {
          params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
        }
        while (expectChar(',')) {
          move(true);
          lastTokenIndex = getLookheadStartIndex();
          ternary();
          this.codeGenerator.onMethodParameter(this.lookhead);
          if (this.captureFuncArgs) {
            params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
          }
        }
      }
      if (expectChar(')')) {
        this.parenDepth--;
        this.depthState.removeLast();
        move(true);
        this.codeGenerator.onMethodInvoke(this.lookhead, params);
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
    this.codeGenerator.onConstant(new PatternToken(sb.toString(), startIndex));
    move(true);
  }


  private void reportSyntaxError(final String message) {
    int index =
        this.lookhead != null && this.lookhead.getStartIndex() > 0 ? this.lookhead.getStartIndex()
            : this.lexer.getCurrentIndex();
    throw new ExpressionSyntaxErrorException(
        "Syntax error:" + message + " at " + index + ", current token: " + this.lookhead
            + ". Parsing expression: `" + this.lexer.getScanString() + "^^`");
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
    statement();
    if (this.lookhead != null) {
      // The lookhead should be null, it's the end.
      reportSyntaxError("Unexpect token '" + currentTokenLexeme() + "'");
    }
    return this.codeGenerator.getResult();
  }


  private void statement() {
    ternary();
    ensureDepthState();
    while (expectChar(';')) {
      this.codeGenerator.onTernaryEnd(this.lookhead);
      move(true);
      ternary();
      ensureDepthState();
    }
  }


  private void ensureDepthState() {
    DepthState state = this.depthState.peekLast();
    if (state != null) {
      back();
      switch (state) {
        case Parent:
          if (this.parenDepth > 0) {
            reportSyntaxError("insert ')' to complete Expression");
          }
          break;
        case Bracket:
          if (this.bracketDepth > 0) {
            reportSyntaxError("insert ']' to complete Expression");
          }
          break;
        case Lambda:
          if (this.lambdaDepth > 0) {
            reportSyntaxError("insert 'end' to complete lambda Expression");
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

}
