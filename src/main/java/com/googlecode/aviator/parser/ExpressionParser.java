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

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
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

  private LinkedList<DepthState> depthState = new LinkedList<DepthState>();


  private boolean inPattern = false;

  private AviatorEvaluatorInstance instance;


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.parser.Parser#getCodeGenerator()
   */
  @Override
  public CodeGenerator getCodeGenerator() {
    return codeGenerator;
  }

  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.parser.Parser#setCodeGenerator(com.googlecode.aviator.code.
   * CodeGenerator)
   */
  @Override
  public void setCodeGenerator(CodeGenerator codeGenerator) {
    this.codeGenerator = codeGenerator;
  }

  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.parser.Parser#enterScope()
   */
  @Override
  public ScopeInfo enterScope() {
    ScopeInfo info = new ScopeInfo(parenDepth, bracketDepth, lambdaDepth, depthState);
    this.parenDepth = 0;
    this.bracketDepth = 0;
    this.lambdaDepth = 0;
    this.depthState = new LinkedList<DepthState>();
    return info;
  }

  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.parser.Parser#restoreScope(com.googlecode.aviator.parser.
   * ExpressionParser.DepthInfo)
   */
  @Override
  public void restoreScope(ScopeInfo info) {
    this.parenDepth = info.parenDepth;
    this.bracketDepth = info.bracketDepth;
    this.lambdaDepth = info.lambdaDepth;
    this.depthState = info.depthState;
  }

  public ExpressionParser(AviatorEvaluatorInstance instance, ExpressionLexer lexer,
      CodeGenerator codeGenerator) {
    super();
    this.instance = instance;
    this.lexer = lexer;
    this.lookhead = this.lexer.scan();
    if (this.lookhead == null) {
      throw new ExpressionSyntaxErrorException("Blank expression");
    }
    this.codeGenerator = codeGenerator;
    this.codeGenerator.setParser(this);
  }


  public void ternary() {
    this.join();
    if (this.lookhead == null || this.expectChar(':') || this.expectChar(',')) {
      return;
    }
    if (this.expectChar('?')) {
      this.move(true);
      this.codeGenerator.onTernaryBoolean(this.lookhead);
      this.ternary();
      if (this.expectChar(':')) {
        this.move(true);
        this.codeGenerator.onTernaryLeft(this.lookhead);
        this.ternary();
        this.codeGenerator.onTernaryRight(this.lookhead);
      } else {
        this.reportSyntaxError("expect ':'");
      }
    }
  }


  public void join() {
    this.and();
    while (true) {
      if (this.isJoinToken()) {
        this.codeGenerator.onJoinLeft(this.lookhead);
        this.move(true);
        if (this.isJoinToken()) {
          this.move(true);
          this.and();
          this.codeGenerator.onJoinRight(this.lookhead);
        } else {
          this.reportSyntaxError("expect '|'");
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
    return this.expectChar('|');
  }


  private boolean expectChar(char ch) {
    if (this.lookhead == null) {
      return false;
    }
    return this.lookhead.getType() == TokenType.Char && ((CharToken) this.lookhead).getCh() == ch;
  }


  private boolean isAndToken() {
    return this.expectChar('&');
  }


  public void bitOr() {
    this.xor();
    while (true) {
      if (this.isJoinToken()) {
        this.move(true);
        if (this.isJoinToken()) {
          this.back();
          break;
        }
        this.xor();
        this.codeGenerator.onBitOr(this.lookhead);
      } else {
        break;
      }
    }
  }


  public void xor() {
    this.bitAnd();
    while (true) {
      if (this.expectChar('^')) {
        this.move(true);
        this.bitAnd();
        this.codeGenerator.onBitXor(this.lookhead);
      } else {
        break;
      }
    }
  }


  public void bitAnd() {
    this.equality();
    while (true) {
      if (this.isAndToken()) {
        this.move(true);
        if (this.isAndToken()) {
          this.back();
          break;
        }
        this.equality();
        this.codeGenerator.onBitAnd(this.lookhead);
      } else {
        break;
      }
    }
  }


  public void and() {
    this.bitOr();
    while (true) {
      if (this.isAndToken()) {
        this.codeGenerator.onAndLeft(this.lookhead);
        this.move(true);
        if (this.isAndToken()) {
          this.move(true);
          this.bitOr();
          this.codeGenerator.onAndRight(this.lookhead);
        } else {
          this.reportSyntaxError("expect '&'");
        }
      } else {
        break;
      }
    }

  }


  public void equality() {
    this.rel();
    while (true) {
      if (this.expectChar('=')) {
        this.move(true);
        if (this.expectChar('=')) {
          this.move(true);
          this.rel();
          this.codeGenerator.onEq(this.lookhead);
        } else if (this.expectChar('~')) {
          // It is a regular expression
          this.move(true);
          this.rel();
          this.codeGenerator.onMatch(this.lookhead);
        } else {
          this.reportSyntaxError("Aviator doesn't support assignment");
        }
      } else if (this.expectChar('!')) {
        this.move(true);
        if (this.expectChar('=')) {
          this.move(true);
          this.rel();
          this.codeGenerator.onNeq(this.lookhead);
        } else {
          this.reportSyntaxError("expect '='");
        }
      } else {
        break;
      }
    }
  }


  public void rel() {
    this.shift();
    while (true) {
      if (this.expectChar('<')) {
        this.move(true);
        if (this.expectChar('=')) {
          this.move(true);
          this.expr();
          this.codeGenerator.onLe(this.lookhead);
        } else {
          this.expr();
          this.codeGenerator.onLt(this.lookhead);
        }
      } else if (this.expectChar('>')) {
        this.move(true);
        if (this.expectChar('=')) {
          this.move(true);
          this.expr();
          this.codeGenerator.onGe(this.lookhead);
        } else {
          this.expr();
          this.codeGenerator.onGt(this.lookhead);
        }
      } else {
        break;
      }
    }
  }


  public void shift() {
    this.expr();
    while (true) {
      if (this.expectChar('<')) {
        this.move(true);
        if (this.expectChar('<')) {
          this.move(true);
          this.expr();
          this.codeGenerator.onShiftLeft(this.lookhead);
        } else {
          this.back();
          break;
        }
      } else if (this.expectChar('>')) {
        this.move(true);
        if (this.expectChar('>')) {
          this.move(true);
          if (this.expectChar('>')) {
            this.move(true);
            this.expr();
            this.codeGenerator.onUnsignedShiftRight(this.lookhead);
          } else {
            this.expr();
            this.codeGenerator.onShiftRight(this.lookhead);
          }

        } else {
          this.back();
          break;
        }
      } else {
        break;
      }
    }
  }


  public void expr() {
    this.term();
    while (true) {
      if (this.expectChar('+')) {
        this.move(true);
        this.term();
        this.codeGenerator.onAdd(this.lookhead);
      } else if (this.expectChar('-')) {
        this.move(true);
        this.term();
        this.codeGenerator.onSub(this.lookhead);
      } else {
        break;
      }
    }
  }


  public void term() {
    this.unary();
    while (true) {
      if (this.expectChar('*')) {
        this.move(true);
        this.unary();
        this.codeGenerator.onMult(this.lookhead);
      } else if (this.expectChar('/')) {
        this.move(true);
        this.unary();
        this.codeGenerator.onDiv(this.lookhead);
      } else if (this.expectChar('%')) {
        this.move(true);
        this.unary();
        this.codeGenerator.onMod(this.lookhead);
      } else {
        break;
      }
    }
  }


  public void unary() {
    if (this.expectChar('!')) {
      this.move(true);
      // check if it is a seq function call,"!" as variable
      if (this.expectChar(',') || this.expectChar(')')) {
        this.back();
        this.factor();
      } else {
        this.unary();
        this.codeGenerator.onNot(this.lookhead);
      }
    } else if (this.expectChar('-')) {
      this.move(true);
      // check if it is a seq function call,"!" as variable
      if (this.expectChar(',') || this.expectChar(')')) {
        this.back();
        this.factor();
      } else {
        this.unary();
        this.codeGenerator.onNeg(this.lookhead);
      }
    } else if (this.expectChar('~')) {
      this.move(true);
      // check if it is a seq function call,"~" as variable
      if (this.expectChar(',') || this.expectChar(')')) {
        this.back();
        this.factor();
      } else {
        this.unary();
        this.codeGenerator.onBitNot(this.lookhead);
      }
    } else {
      this.factor();
    }

    while (expectChar('[') || expectChar('(')) {
      if (expectChar('[')) {
        // (...)[index]
        arrayAccess();
      } else if (expectChar('(')) {
        // May be call chain, such as "s(1)(2)(3)"
        this.parenDepth++;
        this.depthState.add(DepthState.Parent);
        this.codeGenerator.onMethodName(new DelegateToken(this.lookhead.getStartIndex(),
            this.lookhead, DelegateTokenType.Method_Name));
        this.move(true);
        if (!this.expectChar(')')) {
          this.ternary();
          this.codeGenerator.onMethodParameter(this.lookhead);
          while (this.expectChar(',')) {
            this.move(true);
            this.ternary();
            this.codeGenerator.onMethodParameter(this.lookhead);
          }
        }
        if (this.expectChar(')')) {
          this.parenDepth--;
          this.depthState.removeLast();
          this.move(true);
          this.codeGenerator.onMethodInvoke(this.lookhead);
        }
      }
    }

  }

  public static final CharToken LEFT_PAREN = new CharToken('(', -1);
  public static final CharToken RIGHT_PAREN = new CharToken(')', -1);


  public boolean isOPVariable(Token<?> token) {
    if (token.getType() != TokenType.Char) {
      return false;
    }
    CharToken charToken = (CharToken) token;

    this.move(true);
    if (this.expectChar(',') || this.expectChar(')')) {
      this.back();
      String lexeme = String.valueOf(charToken.getCh());
      if (lexeme.equals("-")) {
        lexeme = "-sub";
      }
      return this.instance.containsFunction(lexeme);
    } else {
      this.back();
      return false;
    }
  }

  public void factor() {
    if (this.lookhead == null) {
      this.reportSyntaxError("invalid value");
    }
    if (this.expectChar('(')) {
      this.parenDepth++;
      this.depthState.add(DepthState.Parent);
      this.move(true);
      this.ternary();
      if (this.expectChar(')')) {
        this.move(true);
        this.parenDepth--;
        this.depthState.removeLast();
      }

    } else if (this.lookhead.getType() == TokenType.Number
        || this.lookhead.getType() == TokenType.String
        || this.lookhead.getType() == TokenType.Variable || this.lookhead == Variable.TRUE
        || this.lookhead == Variable.FALSE || this.isOPVariable(this.lookhead)) {
      if (this.lookhead.getType() == TokenType.Variable) {
        this.checkVariableName();
      }
      // binary operation as variable for seq functions
      if (this.lookhead.getType() == TokenType.Char) {
        CharToken charToken = (CharToken) this.lookhead;
        if (!ExpressionLexer.isBinaryOP(charToken.getCh())) {
          this.reportSyntaxError("Unexpect char '" + charToken.getCh() + "'");
        }
        // make it as variable
        this.lookhead = new Variable(charToken.getLexeme(), charToken.getStartIndex());
      }
      this.move(true);
      // function
      Token<?> prev = this.prevToken;
      if (prev.getType() == TokenType.Variable && this.expectChar('(')) {
        if (prev == Variable.LAMBDA) {
          this.lambda();
        } else {
          this.method();
        }
      } else if (prev.getType() == TokenType.Variable) {
        this.arrayAccess();
      } else {
        this.codeGenerator.onConstant(prev);
      }
    } else if (this.expectChar('/')) {
      this.pattern();
    } else {
      this.reportSyntaxError("invalid value");
    }

  }


  private void lambda() {
    this.lambdaDepth++;
    this.depthState.add(DepthState.Lambda);
    this.codeGenerator.onLambdaDefineStart(this.prevToken);
    this.parenDepth++;
    this.depthState.add(DepthState.Parent);
    this.move(true);
    if (!this.expectChar(')')) {
      lambdaArgument();

      while (this.expectChar(',')) {
        this.move(true);
        lambdaArgument();
      }
    }
    if (this.expectChar(')')) {
      this.parenDepth--;
      this.depthState.removeLast();
      this.move(true);
      if (this.expectChar('-')) {
        this.move(true);
        if (this.expectChar('>')) {
          this.codeGenerator.onLambdaBodyStart(lookhead);
          this.move(true);
          this.statement();
          if (this.lookhead != null && this.lookhead.getType() == TokenType.Variable
              && this.lookhead.getLexeme().equals("end")) {
            this.codeGenerator.onLambdaBodyEnd(lookhead);
            this.lambdaDepth--;
            this.depthState.removeLast();

            this.move(true);
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
        this.reportSyntaxError("Illegal argument name: " + currentTokenLexeme() + ",index="
            + this.lookhead.getStartIndex());
      }
      this.codeGenerator.onLambdaArgument(this.lookhead);
      this.move(true);
    } else {
      this.reportSyntaxError("Expect argument name,but is: " + currentTokenLexeme() + ",index="
          + this.lookhead.getStartIndex());
    }
  }


  private void arrayAccess() {
    // check if it is a array index access
    boolean hasArray = false;
    while (this.expectChar('[')) {
      if (!hasArray) {
        this.codeGenerator.onArray(this.prevToken);
        this.move(true);
        hasArray = true;
      } else {
        this.move(true);
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

    this.ternary();
    if (this.expectChar(']')) {
      this.bracketDepth--;
      this.depthState.removeLast();
      this.move(true);
      this.codeGenerator.onArrayIndexEnd(this.lookhead);
    }
  }


  private void checkVariableName() {
    if (!((Variable) this.lookhead).isQuote()) {
      String[] names = this.lookhead.getLexeme().split("\\.");
      for (String name : names) {
        if (!isJavaIdentifier(name)) {
          this.reportSyntaxError(
              "Illegal identifier " + name + ",index=" + this.lookhead.getStartIndex());
        }
      }
    }
  }

  private void method() {
    if (this.expectChar('(')) {
      this.parenDepth++;
      this.depthState.add(DepthState.Parent);
      this.codeGenerator.onMethodName(this.prevToken);
      this.move(true);
      if (!this.expectChar(')')) {
        this.ternary();
        this.codeGenerator.onMethodParameter(this.lookhead);
        while (this.expectChar(',')) {
          this.move(true);
          this.ternary();
          this.codeGenerator.onMethodParameter(this.lookhead);
        }
      }
      if (this.expectChar(')')) {
        this.parenDepth--;
        this.depthState.removeLast();
        this.move(true);
        this.codeGenerator.onMethodInvoke(this.lookhead);
      }
    }
  }


  /**
   * Test whether a given string is a valid Java identifier.
   *
   * @param id string which should be checked
   * @return <code>true</code> if a valid identifier
   */
  public static final boolean isJavaIdentifier(String id) {
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
    this.move(true);
    this.inPattern = true;
    StringBuffer sb = new StringBuffer();
    while (this.lookhead != null) {
      while (!this.expectChar('/')) {
        sb.append(this.lookhead.getLexeme());
        this.move(false);
      }
      if (this.prevToken.getType() == TokenType.Char
          && ((CharToken) this.prevToken).getLexeme().equals("\\")) {
        sb.append("/");
        this.move(false);
        continue;
      }
      this.inPattern = false;
      break;
    }
    if (this.inPattern) {
      this.reportSyntaxError("invalid regular pattern");
    }
    this.codeGenerator.onConstant(new PatternToken(sb.toString(), startIndex));
    this.move(true);
  }


  private void reportSyntaxError(String message) {
    int index = this.lookhead != null && this.lookhead.getStartIndex() > 0
        ? this.lookhead.getStartIndex() : this.lexer.getCurrentIndex();
    throw new ExpressionSyntaxErrorException(
        "Syntax error:" + message + " at " + index + ", current token: " + this.lookhead
            + ". Parsing expression: `" + this.lexer.getScanString() + "^^`");
  }



  public void move(boolean analyse) {
    if (this.lookhead != null) {
      this.prevToken = this.lookhead;
      this.lookhead = this.lexer.scan(analyse);
    } else {
      this.reportSyntaxError("Illegal expression");
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
      this.reportSyntaxError("Unexpect token '" + currentTokenLexeme() + "'");
    }
    return this.codeGenerator.getResult();
  }


  private void statement() {
    this.ternary();
    this.ensureDepthState();
    while (this.expectChar(';')) {
      this.codeGenerator.onTernaryEnd(lookhead);
      this.move(true);
      this.ternary();
      this.ensureDepthState();
    }
  }


  private void ensureDepthState() {
    DepthState state = this.depthState.peekLast();
    if (state != null) {
      back();
      switch (state) {
        case Parent:
          if (this.parenDepth > 0) {
            this.reportSyntaxError("insert ')' to complete Expression");
          }
          break;
        case Bracket:
          if (this.bracketDepth > 0) {
            this.reportSyntaxError("insert ']' to complete Expression");
          }
          break;
        case Lambda:
          if (this.lambdaDepth > 0) {
            this.reportSyntaxError("insert 'end' to complete lambda Expression");
          }
          break;
      }
    }
  }

}
