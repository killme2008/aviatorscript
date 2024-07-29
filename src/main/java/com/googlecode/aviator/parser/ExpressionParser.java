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
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.exception.UnsupportedFeatureException;
import com.googlecode.aviator.lexer.ExpressionLexer;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.lexer.token.CharToken;
import com.googlecode.aviator.lexer.token.DelegateToken;
import com.googlecode.aviator.lexer.token.DelegateToken.DelegateTokenType;
import com.googlecode.aviator.lexer.token.NumberToken;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.lexer.token.PatternToken;
import com.googlecode.aviator.lexer.token.StringToken;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Token.TokenType;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.runtime.FunctionParam;
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
   * lookahead token
   */
  private Token<?> lookahead;

  private final ArrayDeque<Token<?>> prevTokens = new ArrayDeque<>();

  private CodeGenerator codeGenerator;

  private ScopeInfo scope;

  private int parsedTokens;


  private boolean inPattern = false;

  private final AviatorEvaluatorInstance instance;

  private final boolean captureFuncArgs;

  private final Set<Feature> featureSet;



  public Token<?> getPrevToken() {
    return this.prevTokens.peek();
  }



  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.parser.Parser#getCodeGenerator()
   */
  @Override
  public CodeGenerator getCodeGenerator() {
    return this.codeGenerator;
  }



  public Token<?> getLookahead() {
    return this.lookahead;
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
    this.lookahead = this.lexer.scan();
    if (this.lookahead != null) {
      this.parsedTokens++;
    }
    this.featureSet = this.instance.getOptionValue(Options.FEATURE_SET).featureSet;
    if (this.lookahead == null) {
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
    cg.onTernaryEnd(this.lookahead);
    if (expectChar(';')) {
      // 'return;' => 'return nil;'
      if (this.scope.newLexicalScope) {
        cg.onMethodName(Constants.ReducerReturnFn);
        cg.onConstant(Variable.NIL);
        cg.onMethodParameter(this.lookahead);
        cg.onMethodInvoke(this.lookahead);
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
        cg.onMethodParameter(this.lookahead);
        cg.onMethodInvoke(this.lookahead);
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

    if (this.lookahead == Variable.NEW) {
      newStatement();
      return true;
    }

    join();
    if (this.lookahead == null || expectChar(':') || expectChar(',')) {
      return gcTimes < this.getCGTimes;
    }
    Token<?> opToken = this.lookahead;
    if (expectChar('?')) {
      move(true);
      CodeGenerator cg = getCodeGeneratorWithTimes();
      cg.onTernaryBoolean(opToken);
      if (!ternary()) {
        reportSyntaxError("invalid token for ternary operator");
      }
      if (expectChar(':')) {
        move(true);
        cg.onTernaryLeft(this.lookahead);
        if (!ternary()) {
          reportSyntaxError("invalid token for ternary operator");
        }
        cg.onTernaryRight(this.lookahead);
      } else {
        reportSyntaxError("expect ':'");
      }
    }
    return gcTimes < this.getCGTimes;
  }


  public void join() {
    and();
    while (true) {
      Token<?> opToken = this.lookahead;
      if (expectChar('|')) {
        getCodeGeneratorWithTimes().onJoinLeft(opToken);
        move(true);
        if (expectChar('|')) {
          move(true);
          and();
          getCodeGeneratorWithTimes().onJoinRight(opToken);
        } else {
          reportSyntaxError("expect '|'");
        }
      } else {
        // Process operator alias
        String alias = this.instance.getOperatorAliasToken(OperatorType.OR);
        if (alias != null) {
          if (opToken != null && opToken.getType() == TokenType.Variable
              && opToken.getLexeme().equals(alias)) {
            CodeGenerator cg = getCodeGeneratorWithTimes();
            cg.onJoinLeft(opToken);
            move(true);
            and();
            cg.onJoinRight(opToken);
            continue;
          }
        }

        break;
      }

    }
  }


  private boolean expectChar(final char ch) {
    if (this.lookahead == null) {
      return false;
    }
    return this.lookahead.getType() == TokenType.Char && ((CharToken) this.lookahead).getCh() == ch;
  }


  public void bitOr() {
    xor();
    while (true) {
      Token<?> opToken = this.lookahead;
      if (expectChar('|')) {
        move(true);
        if (expectChar('|')) {
          back();
          break;
        }
        xor();
        getCodeGeneratorWithTimes().onBitOr(opToken);
      } else {
        break;
      }
    }
  }


  public void xor() {
    bitAnd();
    while (true) {
      Token<?> opToken = this.lookahead;
      if (expectChar('^')) {
        move(true);
        bitAnd();
        getCodeGeneratorWithTimes().onBitXor(opToken);
      } else {
        break;
      }
    }
  }


  public void bitAnd() {
    equality();
    while (true) {
      Token<?> opToken = this.lookahead;
      if (expectChar('&')) {
        move(true);
        if (expectChar('&')) {
          back();
          break;
        }
        equality();
        getCodeGeneratorWithTimes().onBitAnd(opToken);
      } else {
        break;
      }
    }
  }


  public void and() {
    bitOr();
    while (true) {
      Token<?> opToken = this.lookahead;

      if (expectChar('&')) {
        CodeGenerator cg = getCodeGeneratorWithTimes();
        cg.onAndLeft(opToken);
        move(true);
        if (expectChar('&')) {
          move(true);
          bitOr();
          cg.onAndRight(opToken);
        } else {
          reportSyntaxError("expect '&'");
        }
      } else {
        // Process operator alias
        String alias = this.instance.getOperatorAliasToken(OperatorType.AND);
        if (alias != null) {
          if (opToken != null && opToken.getType() == TokenType.Variable
              && opToken.getLexeme().equals(alias)) {
            CodeGenerator cg = getCodeGeneratorWithTimes();
            cg.onAndLeft(opToken);
            move(true);
            bitOr();
            cg.onAndRight(opToken);
            continue;
          }
        }

        break;
      }


    }

  }


  public void equality() {
    rel();
    while (true) {
      Token<?> opToken = this.lookahead;
      Token<?> prevToken = getPrevToken();
      if (expectChar('=')) {
        move(true);
        if (expectChar('=')) {
          move(true);
          rel();
          getCodeGeneratorWithTimes().onEq(opToken);
        } else if (expectChar('~')) {
          // It is a regular expression
          move(true);
          rel();
          getCodeGeneratorWithTimes().onMatch(opToken);
        } else {
          // this.back();
          // assignment

          boolean isVar = false;
          if (prevToken.getType() == TokenType.Variable) {
            isVar = true;
          } else if (prevToken.getType() == TokenType.Char
              && ((CharToken) prevToken).getCh() == ']') {
            int depth = 1;
            boolean beginSearch = false;
            boolean found = false;
            for (Token<?> t : this.prevTokens) {
              if (!beginSearch && t == prevToken) {
                beginSearch = true;
                continue;
              }

              if (beginSearch && t.getType() == TokenType.Char) {
                CharToken chToken = (CharToken) t;
                switch (chToken.getCh()) {
                  case ']':
                    depth++;
                    break;
                  case '[':
                    depth--;
                    break;
                }
                if (depth == 0) {
                  found = true;
                  continue;
                }

              }

              if (found) {
                if (t.getType() == TokenType.Variable) {
                  t.withMeta(Constants.TYPE_META, CompileTypes.Array);
                }
                break;
              }
            }

          }

          StatementType stmtType = statement();

          // try to find var(prevToken) in right statement, it's not initialized if presents.
          if (isVar) {
            checkVarIsInit(prevToken, stmtType);
          }

          ensureFeatureEnabled(Feature.Assignment);
          getCodeGeneratorWithTimes().onAssignment(opToken);
        }
      } else if (expectChar('!')) {
        move(true);
        if (expectChar('=')) {
          move(true);
          rel();
          getCodeGeneratorWithTimes().onNeq(opToken);
        } else {
          reportSyntaxError("expect '='");
        }
      } else {
        break;
      }
    }
  }



  private void checkVarIsInit(final Token<?> prevToken, StatementType stmtType) {
    boolean isInit = true;
    for (Token<?> t : this.prevTokens) {
      if (t == prevToken) {
        break;
      }
      // It's in right statement, so it's not initialized.
      if (t.getType() == TokenType.Variable && t.getLexeme().equals(prevToken.getLexeme())) {
        isInit = false;
        break;
      }
    }
    prevToken.withMeta(Constants.INIT_META, isInit);
  }


  public void rel() {
    shift();
    while (true) {
      Token<?> opToken = this.lookahead;
      if (expectChar('<')) {
        move(true);
        if (expectChar('=')) {
          move(true);
          expr();
          getCodeGeneratorWithTimes().onLe(opToken);
        } else {
          expr();
          getCodeGeneratorWithTimes().onLt(opToken);
        }
      } else if (expectChar('>')) {
        move(true);
        if (expectChar('=')) {
          move(true);
          expr();
          getCodeGeneratorWithTimes().onGe(opToken);
        } else {
          expr();
          getCodeGeneratorWithTimes().onGt(opToken);
        }
      } else {
        break;
      }
    }
  }


  public void shift() {
    expr();
    while (true) {
      Token<?> opToken = this.lookahead;
      if (expectChar('<')) {
        move(true);
        if (expectChar('<')) {
          move(true);
          expr();
          getCodeGeneratorWithTimes().onShiftLeft(opToken);
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
            getCodeGeneratorWithTimes().onUnsignedShiftRight(opToken);
          } else {
            expr();
            getCodeGeneratorWithTimes().onShiftRight(opToken);
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
      Token<?> opToken = this.lookahead;
      if (expectChar('+')) {
        move(true);
        term();
        getCodeGeneratorWithTimes().onAdd(opToken);
      } else if (expectChar('-')) {
        move(true);
        term();
        getCodeGeneratorWithTimes().onSub(opToken);
      } else {
        break;
      }
    }
  }

  public void exponent() {
    factor();
    while (true) {
      Token<?> opToken = this.lookahead;
      if (expectChar('*')) {
        move(true);
        if (expectChar('*')) {
          move(true);
          unary();
          getCodeGeneratorWithTimes().onExponent(opToken);
        } else {
          back();
          break;
        }
      } else {
        break;
      }
    }
  }


  public void term() {
    unary();
    while (true) {
      Token<?> opToken = this.lookahead;
      if (expectChar('*')) {
        move(true);
        unary();
        getCodeGeneratorWithTimes().onMult(opToken);
      } else if (expectChar('/')) {
        move(true);
        unary();
        getCodeGeneratorWithTimes().onDiv(opToken);
      } else if (expectChar('%')) {
        move(true);
        unary();
        getCodeGeneratorWithTimes().onMod(opToken);
      } else {
        break;
      }
    }
  }


  public void unary() {
    Token<?> opToken = this.lookahead;
    if (expectChar('!')) {
      move(true);
      // check if it is a seq function call,"!" as variable
      if (expectChar(',') || expectChar(')')) {
        back();
        exponent();
      } else {
        unary();
        getCodeGeneratorWithTimes().onNot(opToken);
      }
    } else if (expectChar('-')) {
      move(true);
      // check if it is a seq function call,"!" as variable
      if (expectChar(',') || expectChar(')')) {
        back();
        exponent();
      } else {
        unary();
        getCodeGeneratorWithTimes().onNeg(opToken);
      }
    } else if (expectChar('~')) {
      move(true);
      // check if it is a seq function call,"~" as variable
      if (expectChar(',') || expectChar(')')) {
        back();
        exponent();
      } else {
        unary();
        getCodeGeneratorWithTimes().onBitNot(opToken);
      }
    } else {
      exponent();
    }
  }

  private int getLookaheadStartIndex() {
    // We should calculate the lookahead token's start index, because the token may be reserved by
    // symbol table and it's start index is wrong.
    return this.lookahead != null ? (this.lexer.getCurrentIndex() - getLookaheadLexemeLength())
        : -1;
  }

  private int getLookaheadLexemeLength() {
    int len = this.lookahead.getLexeme().length();
    if (this.lookahead.getType() == TokenType.String) {
      // Must include quote symbols.
      len += 2;
    }
    return len;
  }

  private String getParamExp(final int lastTokenIndex) {
    if (lastTokenIndex >= 0 && getLookaheadStartIndex() >= 0) {
      return this.lexer.getScanString().substring(lastTokenIndex, getLookaheadStartIndex());
    } else {
      return null;
    }
  }

  public static final CharToken LEFT_PAREN = new CharToken('(', 0, -1);
  public static final CharToken RIGHT_PAREN = new CharToken(')', 0, -1);


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
    if (factor0()) {
      methodInvokeOrArrayAccess();
    }
  }



  private boolean factor0() {
    if (this.lookahead == null) {
      reportSyntaxError("illegal token");
    }
    if (this.lookahead == Variable.END) {
      return false;
    }
    if (expectChar('(')) {
      move(true);
      this.scope.enterParen();
      ternary();
      if (expectChar(')')) {
        move(true);
        this.scope.leaveParen();
      }
    } else if (this.lookahead.getType() == TokenType.Number
        || this.lookahead.getType() == TokenType.String
        || this.lookahead.getType() == TokenType.Variable || this.lookahead == Variable.TRUE
        || this.lookahead == Variable.FALSE || isOPVariable(this.lookahead)) {
      if (this.lookahead.getType() == TokenType.Variable) {
        checkVariableName(this.lookahead);
      }
      // binary operation as variable for seq functions
      if (this.lookahead.getType() == TokenType.Char) {
        CharToken charToken = (CharToken) this.lookahead;
        if (!ExpressionLexer.isBinaryOP(charToken.getCh())) {
          reportSyntaxError("unexpect char '" + charToken.getCh() + "'");
        }
        // make it as variable
        this.lookahead = this.lexer.getSymbolTable().reserve(
            new Variable(charToken.getLexeme(), charToken.getLineNo(), charToken.getStartIndex()));
      }
      move(true);
      // function
      Token<?> prev = getPrevToken();
      if (prev.getType() == TokenType.Variable && expectChar('(')) {
        if (prev == Variable.LAMBDA) {
          lambda(false);
        } else if (prev == Variable.FN) {
          lambda(true);
        } else {
          method(prev);
        }
      } else if (prev.getType() == TokenType.Variable) {
        if (!arrayAccess()) {
          getCodeGeneratorWithTimes().onConstant(prev);
        }
      } else {
        getCodeGeneratorWithTimes().onConstant(prev);
      }
    } else if (expectChar('/')) {
      pattern();
    } else if (expectChar('}')) {
      return false;
    } else {
      reportSyntaxError("invalid token");
    }
    return true;
  }


  private void lambda(final boolean fn) {
    ensureFeatureEnabled(Feature.Lambda);
    this.scope.enterLambda();
    getCodeGeneratorWithTimes().onLambdaDefineStart(getPrevToken());
    this.scope.enterParen();
    move(true);
    int paramIndex = 0;
    FunctionParam lastParam = null;
    List<FunctionParam> variadicParams = new ArrayList<>(2);
    if (!expectChar(')')) {
      lastParam = lambdaArgument(paramIndex++);
      if (lastParam.isVariadic()) {
        variadicParams.add(lastParam);
      }

      while (expectChar(',')) {
        move(true);
        lastParam = lambdaArgument(paramIndex++);
        if (lastParam.isVariadic()) {
          variadicParams.add(lastParam);
        }
      }
    }

    // assert only one variadic param and it's the last one.
    if (variadicParams.size() > 1) {
      reportSyntaxError("The variadic parameter must be the last parameter: `"
          + variadicParams.get(0).getName() + "`");
    }
    if (variadicParams.size() > 0 && variadicParams.get(0) != lastParam) {
      reportSyntaxError("The variadic parameter must be the last parameter: `"
          + variadicParams.get(0).getName() + "`");
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
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
      statements();

      if (fn) {
        if (!expectChar('}')) {
          reportSyntaxError("missing '}' to close function body");
        }
      } else {
        if (this.lookahead != Variable.END) {
          reportSyntaxError("expect lambda 'end'");
        }
      }

      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
      this.scope.leaveLambda();
      move(true);
    }
  }

  private String currentTokenLexeme() {
    return this.lookahead == null ? "END_OF_STRING" : this.lookahead.getLexeme();
  }


  private FunctionParam lambdaArgument(final int index) {
    if (expectChar('&')) {
      move(true);

      if (this.lookahead.getType() != TokenType.Variable) {
        reportSyntaxError("expect argument name, but is: " + currentTokenLexeme());
      }

      return lambdaArgument0(index, true);
    } else if (this.lookahead.getType() == TokenType.Variable) {
      return lambdaArgument0(index, false);
    } else {
      reportSyntaxError("expect argument name, but is: " + currentTokenLexeme());
      return null;
    }
  }

  private FunctionParam lambdaArgument0(final int index, final boolean isVariadic) {
    if (!isJavaIdentifier(this.lookahead.getLexeme())) {
      reportSyntaxError("illegal argument name: " + currentTokenLexeme());
    }
    final FunctionParam param = new FunctionParam(index, this.lookahead.getLexeme(), isVariadic);
    getCodeGeneratorWithTimes().onLambdaArgument(this.lookahead, param);
    move(true);
    return param;
  }


  private boolean arrayAccess() {
    // check if it is a array index access
    boolean hasArray = false;
    while (expectChar('[')) {
      if (!hasArray) {
        getCodeGeneratorWithTimes().onArray(getPrevToken());
        move(true);
        hasArray = true;
      } else {
        move(true);
      }
      getCodeGeneratorWithTimes().onArrayIndexStart(getPrevToken());
      array();
    }
    return hasArray;

  }


  private void array() {
    this.scope.enterBracket();
    if (getPrevToken() == Variable.TRUE || getPrevToken() == Variable.FALSE
        || getPrevToken() == Variable.NIL) {
      reportSyntaxError(getPrevToken().getLexeme() + " could not use [] operator");
    }
    if (!ternary()) {
      reportSyntaxError("missing index for array access");
    }
    if (expectChar(']')) {
      this.scope.leaveBracket();
      move(true);
      getCodeGeneratorWithTimes().onArrayIndexEnd(this.lookahead);
    }
  }

  private void checkVariableName(final Token<?> token) {
    if (token.getType() == TokenType.Delegate) {
      return;
    }
    if (!((Variable) token).isQuote()) {
      String[] names = token.getLexeme().split("\\.");
      for (String name : names) {
        if (!isJavaIdentifier(name)) {
          reportSyntaxError("illegal identifier: " + name);
        }
      }
    }
  }

  private void methodInvokeOrArrayAccess() {
    while (expectChar('[') || expectChar('(')) {
      if (isConstant(getPrevToken(), this.instance)) {
        break;
      }
      if (expectChar('[')) {
        arrayAccess();
      } else if (expectChar('(')) {
        method(anonymousMethodName());
      }
    }
  }

  private void method(final Token<?> methodName) {
    if (expectChar('(')) {
      this.scope.enterParen();
      checkVariableName(methodName);
      checkFunctionName(methodName, false);
      getCodeGeneratorWithTimes().onMethodName(methodName);
      move(true);
      int paramIndex = 0;
      List<FunctionArgument> params = null;
      boolean unpackArguments = false;
      if (this.captureFuncArgs) {
        params = new ArrayList<>();
      }
      int lastTokenIndex = getLookaheadStartIndex();
      if (!expectChar(')')) {

        boolean isPackArgs = false;
        if (expectChar('*')) {
          move(true);
          if (expectChar('*') || expectChar(',')) {
            // binary operation as argument
            back();
          } else {
            unpackArguments = true;
            withMetaBegin();
            isPackArgs = true;
          }
        }

        ternary();

        if (isPackArgs) {
          withMetaEnd(Constants.UNPACK_ARGS, true);
        }

        getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
        if (this.captureFuncArgs) {
          params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
        }
        while (expectChar(',')) {
          move(true);
          isPackArgs = false;
          lastTokenIndex = getLookaheadStartIndex();
          if (expectChar('*')) {
            move(true);
            if (expectChar('*') || expectChar(',')) {
              // binary operation as argument
              back();
            } else {
              unpackArguments = true;
              withMetaBegin();
              isPackArgs = true;
            }
          }

          if (!ternary()) {
            reportSyntaxError("invalid argument");
          }

          if (isPackArgs) {
            withMetaEnd(Constants.UNPACK_ARGS, true);
          }

          getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
          if (this.captureFuncArgs) {
            params.add(new FunctionArgument(paramIndex++, getParamExp(lastTokenIndex)));
          }
        }
      }
      if (unpackArguments) {
        methodName.withMeta(Constants.UNPACK_ARGS, true);
      }
      if (expectChar(')')) {
        getCodeGeneratorWithTimes()
            .onMethodInvoke(currentToken().withMeta(Constants.PARAMS_META, params));
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
    int startIndex = this.lookahead.getStartIndex();
    move(true);
    this.inPattern = true;
    StringBuilder sb = new StringBuilder();
    while (this.lookahead != null) {
      while (!expectChar('/') && this.lookahead != null) {
        sb.append(this.lookahead.getLexeme());
        move(false);
      }
      if (getPrevToken().getType() == TokenType.Char
          && ((CharToken) getPrevToken()).getLexeme().equals("\\")) {
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
    getCodeGeneratorWithTimes()
        .onConstant(new PatternToken(sb.toString(), this.lexer.getLineNo(), startIndex));
    move(true);
  }


  public void reportSyntaxError(final String message) {
    int index = isValidLookahead() ? this.lookahead.getStartIndex() : this.lexer.getCurrentIndex();

    if (this.lookahead != null) {
      this.lexer.pushback(this.lookahead);
    }

    String msg = "Syntax error: " + message + //
        " at " + index + //
        ", lineNumber: " + this.lexer.getLineNo() + //
        ", token : " + //
        this.lookahead + ",\nwhile parsing expression: `\n" + //
        this.lexer.getScanString() + "^^^\n`";

    ExpressionSyntaxErrorException e = new ExpressionSyntaxErrorException(msg);
    StackTraceElement[] traces = e.getStackTrace();
    List<StackTraceElement> filteredTraces = new ArrayList<>();
    for (StackTraceElement t : traces) {
      if (!this.instance.getOptionValue(Options.TRACE_EVAL).bool
          && t.getClassName().equals(this.getClass().getName())) {
        continue;
      }
      filteredTraces.add(t);
    }
    e.setStackTrace(filteredTraces.toArray(new StackTraceElement[filteredTraces.size()]));
    throw e;
  }

  private boolean isValidLookahead() {
    return this.lookahead != null && this.lookahead.getStartIndex() > 0;
  }

  public void move(final boolean analyse) {
    if (this.lookahead != null) {
      this.prevTokens.push(this.lookahead);
      this.lookahead = this.lexer.scan(analyse);
      if (this.lookahead != null) {
        this.parsedTokens++;
      }
    } else {
      reportSyntaxError("illegal token");
    }
  }

  public int getParsedTokens() {
    return this.parsedTokens;
  }

  public void back() {
    if (this.lookahead != null) {
      this.parsedTokens--;
    }
    this.lexer.pushback(this.lookahead);
    this.lookahead = getPrevToken();
  }


  public Expression parse(final boolean reportErrorIfNotEOF) {
    StatementType statementType = statements();
    if (this.lookahead != null && reportErrorIfNotEOF) {
      if (statementType == StatementType.Ternary) {
        reportSyntaxError("unexpect token '" + currentTokenLexeme()
            + "', maybe forget to insert ';' to complete last expression ");
      } else {
        reportSyntaxError("unexpect token '" + currentTokenLexeme() + "'");
      }
    }
    return getCodeGeneratorWithTimes().getResult(true);
  }


  public Expression parse() {
    return parse(true);
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
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
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
    getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
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
    getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);

    // create a lambda function wraps while body(iterator)
    boolean newLexicalScope = this.scope.newLexicalScope;
    this.scope.newLexicalScope = true;
    {
      getCodeGeneratorWithTimes().onLambdaDefineStart(
          getPrevToken().withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
      ifStatement(true, false);
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
      getCodeGenerator().onMethodParameter(this.lookahead);
    }

    if (expectChar(';')) {
      // the statement is ended.
      getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
    } else {
      // create a lambda function wraps statements after while(statements)
      getCodeGeneratorWithTimes().onLambdaDefineStart(
          getPrevToken().withMeta(Constants.SCOPE_META, this.scope.newLexicalScope) //
              .withMeta(Constants.INHERIT_ENV_META, true));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
      if (statements() == StatementType.Empty) {
        getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
      }
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
    }
    getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);

    // call __reducer_callcc(LOOP, iterator, statements)
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
    // restore newLexicalScope
    this.scope.newLexicalScope = newLexicalScope;

  }

  private void letStatement() {
    move(true);
    Token<?> var = this.lookahead;
    checkVariableName(var);
    getCodeGenerator().onConstant(var);
    move(true);
    if (!expectChar('=')) {
      reportSyntaxError("expect '='");
    }
    move(true);
    StatementType stmtType = statement();
    if (stmtType == StatementType.Empty) {
      reportSyntaxError("invalid value to define");
    }
    checkVarIsInit(var, stmtType);
    ensureFeatureEnabled(Feature.Assignment);
    getCodeGeneratorWithTimes().onAssignment(currentToken().withMeta(Constants.DEFINE_META, true));
    if (!expectChar(';')) {
      reportSyntaxError("missing ';' for let statement");
    }
    move(true);
  }

  private void fnStatement() {
    move(true);

    if (expectChar('(')) {
      // Anonymous function
      lambda(true);
    } else {

      checkVariableName(this.lookahead);
      checkFunctionName(this.lookahead, true);
      getCodeGeneratorWithTimes().onConstant(this.lookahead.withMeta(Constants.INIT_META, true)
          .withMeta(Constants.TYPE_META, CompileTypes.Function));
      move(true);
      if (!expectChar('(')) {
        reportSyntaxError("expect '(' after function name");
      }
      lambda(true);
      ensureFeatureEnabled(Feature.Assignment);
      getCodeGeneratorWithTimes()
          .onAssignment(currentToken().withMeta(Constants.DEFINE_META, true));
    }
  }

  private void checkFunctionName(final Token<?> token, final boolean warnOnExists) {
    String fnName = token.getLexeme();
    if (SymbolTable.isReservedKeyword(fnName)) {
      reportSyntaxError("The function name `" + fnName + "` is a reserved keyword");
    }
    if (warnOnExists && this.instance.getFuncMap().containsKey(fnName)) {
      System.out.println("[Aviator WARN] The function '" + fnName
          + "' is already exists, but is replaced with new one.");
    }
  }

  private Token<?> currentToken() {
    Token<?> token = this.lookahead;
    if (token == null) {
      token = new CharToken((char) -1, this.lexer.getLineNo(), this.lexer.getCurrentIndex());
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
          getPrevToken().withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
      hasReturn = statements() == StatementType.Return;
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
      getCodeGeneratorWithTimes().onMethodName(anonymousMethodName());
      getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);

      if (!expectChar('}')) {
        reportSyntaxError("missing '}' to close scope");
      }
      move(true);
      this.scope.leaveBrace();
      getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
    }

    if (expectChar(';')) {
      // the statement is ended.
      getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
    } else {
      // create a lambda function wraps statements after scope statement (statements)
      getCodeGeneratorWithTimes().onLambdaDefineStart(
          getPrevToken().withMeta(Constants.SCOPE_META, this.scope.newLexicalScope) //
              .withMeta(Constants.INHERIT_ENV_META, true));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
      if (statements() == StatementType.Empty) {
        getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
      }
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
    }
    getCodeGenerator().onMethodParameter(this.lookahead);

    // call __if_callcc(result, statements)
    getCodeGenerator().onMethodInvoke(this.lookahead);
    this.scope.newLexicalScope = newLexicalScope;

    return hasReturn;
  }

  private void tryStatement() {
    getCodeGeneratorWithTimes().onMethodName(Constants.TRY_VAR);
    move(true);
    if (!expectChar('{')) {
      reportSyntaxError("expect '{' after try");
    }
    move(true);

    boolean newLexicalScope = this.scope.newLexicalScope;
    this.scope.newLexicalScope = true;
    // create a lambda function wraps try body
    {

      getCodeGeneratorWithTimes().onLambdaDefineStart(
          getPrevToken().withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
      statements();
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
      getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
    }
    if (!expectChar('}')) {
      reportSyntaxError("missing '}' for try body");
    }
    move(true);
    boolean hasCatch = false;
    boolean hasFinally = false;

    while (this.lookahead == Variable.CATCH) {
      if (!hasCatch) {
        // create a handler list.
        getCodeGeneratorWithTimes().onMethodName(Constants.SEQ_LIST_VAR);
        hasCatch = true;
      }

      move(true);
      // create a lambda function wraps catch handlers
      if (!expectChar('(')) {
        reportSyntaxError("expect '(' after catch");
      }
      move(true);
      if (this.lookahead == null || this.lookahead.getType() != TokenType.Variable) {
        reportSyntaxError("invalid exception class name");
      }
      checkVariableName(this.lookahead);
      List<Token<?>> exceptionClasses = new ArrayList<>();
      exceptionClasses.add(this.lookahead);
      move(true);

      Token<?> boundVar = null;
      if (expectChar(')')) {
        // catch(e) to catch all.
        boundVar = exceptionClasses.remove(0);
        exceptionClasses.add(Constants.THROWABLE_VAR);
      } else {
        // catch multi exception
        while (expectChar('|')) {
          move(true);
          if (this.lookahead.getType() != TokenType.Variable) {
            reportSyntaxError("invalid exception class to catch");
          }
          checkVariableName(this.lookahead);
          exceptionClasses.add(this.lookahead);
          move(true);
        }
        if (this.lookahead == null || this.lookahead.getType() != TokenType.Variable) {
          reportSyntaxError("invalid bound variable name for exception");
        }
        checkVariableName(this.lookahead);
        boundVar = this.lookahead;
        move(true);
      }

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
        getCodeGeneratorWithTimes().onMethodName(Constants.CATCH_HANDLER_VAR);
        getCodeGeneratorWithTimes().onLambdaDefineStart(
            getPrevToken().withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
        getCodeGeneratorWithTimes().onLambdaArgument(boundVar,
            new FunctionParam(0, boundVar.getLexeme(), false));
        getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
        statements();
        getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
        getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
        for (Token<?> exceptionClass : exceptionClasses) {
          getCodeGeneratorWithTimes().onConstant(exceptionClass);
          getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
        }
        getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
      }
      getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
      if (!expectChar('}')) {
        reportSyntaxError("missing '}' for to complete catch block");
      }
      move(true);
    }

    if (hasCatch) {
      // Invoke seq.list to create handler list
      getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
      getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
    } else {
      getCodeGeneratorWithTimes().onConstant(Variable.NIL);
      getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
    }

    if (this.lookahead == Variable.FINALLY) {
      hasFinally = true;
      move(true);
      if (!expectChar('{')) {
        reportSyntaxError("missing '{' for finally block");
      }
      move(true);
      // create a lambda to
      getCodeGeneratorWithTimes().onLambdaDefineStart(
          getPrevToken().withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
      statements();
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
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
      getCodeGeneratorWithTimes().onLambdaDefineStart(
          getPrevToken().withMeta(Constants.SCOPE_META, this.scope.newLexicalScope) //
              .withMeta(Constants.INHERIT_ENV_META, true));
      getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
      if (statements() == StatementType.Empty) {
        getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
      }
      getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
    }

    getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);

    this.scope.newLexicalScope = newLexicalScope;
    getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
  }

  private void throwStatement() {
    getCodeGeneratorWithTimes().onMethodName(Constants.THROW_VAR);
    move(true);
    statement();
    getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
    if (!expectChar(';')) {
      reportSyntaxError("missing ';' for throw statement");
    }
  }

  private void newStatement() {
    ensureFeatureEnabled(Feature.NewInstance);
    getCodeGeneratorWithTimes().onMethodName(Constants.NEW_VAR);
    move(true);

    if (this.lookahead == null || this.lookahead.getType() != TokenType.Variable) {
      reportSyntaxError("invalid class name");
    }
    checkVariableName(this.lookahead);
    getCodeGeneratorWithTimes()
        .onConstant(this.lookahead.withMeta(Constants.TYPE_META, CompileTypes.Class));
    getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
    move(true);

    if (!expectChar('(')) {
      reportSyntaxError("missing '(' after class name");
    }

    this.scope.enterParen();
    move(true);
    if (!expectChar(')')) {
      ternary();
      getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
      while (expectChar(',')) {
        move(true);
        if (!ternary()) {
          reportSyntaxError("invalid argument");
        }
        getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
      }
    }
    if (!expectChar(')')) {
      reportSyntaxError("missing ')' for new statement");
    }
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
    move(true);
    this.scope.leaveParen();
  }

  private void className() {
    if (this.lookahead.getType() != TokenType.Variable && !expectChar('*')) {
      reportSyntaxError("expect variable name or * to use");
    }
    if (expectChar('*')) {
      wildcard();
    } else {
      checkVariableName(this.lookahead);
      getCodeGenerator().onConstant(this.lookahead.withMeta(Constants.USE_CLASS_PKG, true));
    }
    move(true);
  }

  private void useStatement() {
    getCodeGeneratorWithTimes().onMethodName(Constants.USE_VAR);
    move(true);
    className();
    getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);

    if (expectChar('*')) {
      // wildcard
      wildcard();
      getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
      move(true);
    } else if (expectChar('{')) {
      this.scope.enterBrace();
      move(true);
      className();
      getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
      while (expectChar(',')) {
        move(true);
        className();
        getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
      }
      if (!expectChar('}')) {
        reportSyntaxError("expect '}' to complete use statement");
      } else {
        move(true);
        this.scope.leaveBrace();
      }
    }

    getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
    if (!expectChar(';')) {
      reportSyntaxError("missing ';' for use statement");
    }
  }



  private void wildcard() {
    getCodeGenerator()
        .onConstant(new Variable("*", this.lookahead.getLineNo(), this.lookahead.getStartIndex()));
  }

  private StatementType statement() {
    if (this.lookahead == Variable.IF) {
      ensureFeatureEnabled(Feature.If);
      if (ifStatement(false, false)) {
        return StatementType.Return;
      } else {
        return StatementType.Other;
      }
    } else if (this.lookahead == Variable.FOR) {
      ensureFeatureEnabled(Feature.ForLoop);
      forStatement();
      return StatementType.Other;
    } else if (this.lookahead == Variable.RETURN) {
      ensureFeatureEnabled(Feature.Return);
      returnStatement();
      return StatementType.Return;
    } else if (this.lookahead == Variable.BREAK) {
      breakStatement();
      return StatementType.Return;
    } else if (this.lookahead == Variable.CONTINUE) {
      continueStatement();
      return StatementType.Return;
    } else if (this.lookahead == Variable.LET) {
      ensureFeatureEnabled(Feature.Let);
      letStatement();
      return StatementType.Other;
    } else if (this.lookahead == Variable.WHILE) {
      ensureFeatureEnabled(Feature.WhileLoop);
      whileStatement();
      return StatementType.Other;
    } else if (this.lookahead == Variable.FN) {
      ensureFeatureEnabled(Feature.Fn);
      fnStatement();
      return StatementType.Other;
    } else if (this.lookahead == Variable.TRY) {
      ensureFeatureEnabled(Feature.ExceptionHandle);
      tryStatement();
      return StatementType.Other;
    } else if (this.lookahead == Variable.THROW) {
      ensureFeatureEnabled(Feature.ExceptionHandle);
      throwStatement();
      return StatementType.Other;
    } else if (expectChar('{')) {
      ensureFeatureEnabled(Feature.LexicalScope);
      if (scopeStatement()) {
        return StatementType.Return;
      } else {
        return StatementType.Other;
      }
    } else if (this.lookahead == Variable.USE) {
      ensureFeatureEnabled(Feature.Use);
      useStatement();
      return StatementType.Other;
    } else {
      if (ternary()) {
        return StatementType.Ternary;
      } else {
        return StatementType.Empty;
      }
    }
  }

  private void withMetaBegin() {
    getCodeGeneratorWithTimes().onMethodName(Constants.WithMetaFn);
  }

  private void withMetaEnd(final Object key, final Object val) {
    getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
    getCodeGeneratorWithTimes().onConstant(value2token(key));
    getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);

    getCodeGeneratorWithTimes().onConstant(value2token(val));
    getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);

    getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
  }



  private Token<?> value2token(final Object val) {
    if (val instanceof Token) {
      return (Token<?>) val;
    } else if (val == null) {
      return Variable.NIL;
    } else if (val instanceof String) {
      return (new StringToken((String) val, this.lexer.getLineNo(), this.lookahead.getStartIndex()));
    } else if (val instanceof Number) {
      return (new NumberToken((Number) val, val.toString(), this.lexer.getLineNo(), this.lookahead.getStartIndex()));
    } else if (val instanceof Boolean) {
      return (((boolean) val) ? Variable.TRUE : Variable.FALSE);
    } else {
      throw new ExpressionRuntimeException(
          "Unsupported compiled-time metadata type: " + val.getClass());
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

    List<Token<?>> reducerArgs = new ArrayList<>(2);

    while (true) {
      if (reducerArgs.size() > 2) {
        reportSyntaxError("Too many variables in for statement: " + reducerArgs.size());
      }
      reducerArgs.add(this.lookahead);
      checkVariableName(this.lookahead);
      move(true);
      if (expectChar(',')) {
        move(true);
        continue;
      } else {
        break;
      }
    }

    if (this.lookahead == Variable.IN) {
      move(true);
      // prepare to call __reducer_callcc(seq, iterator, statements)
      {
        getCodeGeneratorWithTimes().onMethodName(Constants.ReducerFn);
        // The seq
        if (!ternary()) {
          reportSyntaxError("missing collection");
        }
        getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
      }
      if (expectChar('{')) {
        move(true);
        this.scope.enterBrace();

        boolean newLexicalScope = this.scope.newLexicalScope;
        this.scope.newLexicalScope = true;

        // create a lambda function wraps for-loop body(iterator)
        {
          withMetaBegin();
          getCodeGeneratorWithTimes().onLambdaDefineStart(
              getPrevToken().withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));

          for (Token<?> reducerArg : reducerArgs) {
            getCodeGeneratorWithTimes().onLambdaArgument(reducerArg,
                new FunctionParam(0, reducerArg.getLexeme(), false));
          }
          getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
          statements();
          getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
          withMetaEnd(Constants.ARITIES_META, Long.valueOf(reducerArgs.size()));
          getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
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
          getCodeGeneratorWithTimes().onLambdaDefineStart(
              getPrevToken().withMeta(Constants.SCOPE_META, this.scope.newLexicalScope) //
                  .withMeta(Constants.INHERIT_ENV_META, true));
          getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
          if (statements() == StatementType.Empty) {
            getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
          }
          getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
        }
        getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
        // call __reducer_callcc(seq, iterator, statements)
        getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
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
    if (this.lookahead == null) {
      return StatementType.Empty;
    }

    StatementType stmtType = statement();
    ensureDepthState();
    while (expectChar(';') || stmtType == StatementType.Other || stmtType == StatementType.Return) {

      ensureNoStatementAfterReturn(stmtType);

      if (this.lookahead != null && this.lookahead != Variable.END && !expectChar('}')) {
        getCodeGeneratorWithTimes().onTernaryEnd(this.lookahead);
      }

      if (expectChar(';')) {
        move(true);
      }

      if (this.lookahead == null) {
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
    // If the last statement is ternary,it must be ended with END TOKEN such as null token, '}',
    // 'end' keyword, or ';'
    // Otherwise report syntax error.
    if (stmtType == StatementType.Ternary) {
      if (lookahead != null && !expectChar(';') && !expectChar('}') && lookahead != Variable.END) {
        this.back();
        reportSyntaxError("unexpect token '" + currentTokenLexeme()
            + "', maybe forget to insert ';' to complete last expression ");
      }
    }

    return stmtType;
  }

  private void ensureNoStatementAfterReturn(final StatementType statementType) {
    if (statementType == StatementType.Return && this.lookahead != null) {
      if (this.lookahead != Variable.END && !expectChar('}')) {
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

      getCodeGeneratorWithTimes().onTernaryBoolean(this.lookahead);
      if (expectChar('{')) {
        move(true);
        this.scope.enterBrace();
        getCodeGeneratorWithTimes().onLambdaDefineStart(
            getPrevToken().withMeta(Constants.SCOPE_META, this.scope.newLexicalScope));
        getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
        ifBodyHasReturn = statements() == StatementType.Return;
        getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
        getCodeGeneratorWithTimes().onMethodName(anonymousMethodName());
        getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
        getCodeGeneratorWithTimes().onTernaryLeft(this.lookahead);
      } else {
        reportSyntaxError("expect '{' for " + getLoopKeyword(isWhile) + " statement");
      }
      if (!expectChar('}')) {
        reportSyntaxError("missing '}' to close " + getLoopKeyword(isWhile) + " body");
      }
      this.scope.leaveBrace();
      move(true);

      elseBodyHasReturn = elseStatement(isWhile, isElsif, ifBodyHasReturn);
      getCodeGeneratorWithTimes().onMethodParameter(this.lookahead);
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
          getCodeGeneratorWithTimes().onLambdaDefineStart(
              getPrevToken().withMeta(Constants.SCOPE_META, this.scope.newLexicalScope) //
                  .withMeta(Constants.INHERIT_ENV_META, true));
          getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
          if (statements() == StatementType.Empty) {
            getCodeGenerator().onConstant(Constants.ReducerEmptyVal);
          } else {
            if (ifBodyHasReturn && elseBodyHasReturn && !isElsif) {
              reportSyntaxError("unreachable code");
            }
          }
          getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
        }
      }
      getCodeGenerator().onMethodParameter(this.lookahead);
      // call __if_callcc(result, statements)
      getCodeGenerator().onMethodInvoke(this.lookahead);
      this.scope.newLexicalScope = newLexicalScope;
    }

    return ifBodyHasReturn && elseBodyHasReturn;
  }

  private String getLoopKeyword(final boolean isWhile) {
    return isWhile ? "while" : "if";
  }

  private boolean elseStatement(final boolean isWhile, boolean isElsif,
      final boolean ifBodyHasReturn) {
    if (isWhile) {
      // Call __reducer_break(nil)
      final CodeGenerator cg = getCodeGeneratorWithTimes();
      cg.onMethodName(Constants.ReducerBreakFn);
      cg.onConstant(Variable.NIL);
      cg.onMethodParameter(this.lookahead);
      cg.onMethodInvoke(this.lookahead);
      cg.onTernaryRight(this.lookahead);
      return false;
    }

    if (expectChar(';')) {
      return withoutElse();
    }

    boolean hasReturn = false;
    boolean hasElsif = this.lookahead == Variable.ELSIF;
    boolean hasElse = this.lookahead == Variable.ELSE;
    if (this.lookahead != null && (hasElse || hasElsif || ifBodyHasReturn)) {
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
        getCodeGenerator().onTernaryRight(this.lookahead);
      } else if (ifBodyHasReturn && !isElsif) {
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
    cg.onTernaryRight(this.lookahead);
    return false;
  }

  private boolean elseBody(final boolean inheritEnv) {
    getCodeGeneratorWithTimes().onLambdaDefineStart(this.lookahead //
        .withMeta(Constants.SCOPE_META, this.scope.newLexicalScope) //
        .withMeta(Constants.INHERIT_ENV_META, inheritEnv) //
    );
    getCodeGeneratorWithTimes().onLambdaBodyStart(this.lookahead);
    boolean hasReturn = statements() == StatementType.Return;
    getCodeGeneratorWithTimes().onLambdaBodyEnd(this.lookahead);
    getCodeGeneratorWithTimes().onMethodName(anonymousMethodName());
    getCodeGeneratorWithTimes().onMethodInvoke(this.lookahead);
    getCodeGeneratorWithTimes().onTernaryRight(this.lookahead);
    return hasReturn;
  }

  private DelegateToken anonymousMethodName() {
    return new DelegateToken(this.lookahead, DelegateTokenType.Method_Name);
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

  public static boolean isConstant(final Token<?> token, final AviatorEvaluatorInstance instance) {
    switch (token.getType()) {
      case Number:
      case Pattern:
        return true;
      case String:
        return !instance.isFeatureEnabled(Feature.StringInterpolation);
      default:
        return false;
    }
  }

  public static boolean isLiteralToken(final Token<?> token,
      final AviatorEvaluatorInstance instance) {
    switch (token.getType()) {
      case Variable:
        return token == Variable.TRUE || token == Variable.FALSE || token == Variable.NIL;
      case Char:
      case Number:
      case Pattern:
        return true;
      case String:
        return !instance.isFeatureEnabled(Feature.StringInterpolation);
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
