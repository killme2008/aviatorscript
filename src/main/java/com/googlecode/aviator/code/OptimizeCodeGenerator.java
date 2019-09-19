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
package com.googlecode.aviator.code;

import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.BaseExpression;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.LiteralExpression;
import com.googlecode.aviator.code.asm.ASMCodeGenerator;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.lexer.token.DelegateToken;
import com.googlecode.aviator.lexer.token.DelegateToken.DelegateTokenType;
import com.googlecode.aviator.lexer.token.NumberToken;
import com.googlecode.aviator.lexer.token.OperatorToken;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.lexer.token.PatternToken;
import com.googlecode.aviator.lexer.token.StringToken;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Token.TokenType;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.parser.Parser;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;
import com.googlecode.aviator.runtime.op.OperationRuntime;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorNumber;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorPattern;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorString;
import com.googlecode.aviator.utils.Env;


/**
 * Optimized code generator
 *
 * @author dennis
 *
 */
public class OptimizeCodeGenerator implements CodeGenerator {
  private final ASMCodeGenerator codeGen;

  private final List<Token<?>> tokenList = new ArrayList<Token<?>>();

  private LambdaGenerator lambdaGenerator;

  private CodeGenerator parentCodeGenerator;

  private boolean trace = false;
  private final AviatorEvaluatorInstance instance;
  // the expression parser
  private Parser parser;

  private Env compileEnv;

  /**
   * Compiled lambda functions.
   */
  private Map<String, LambdaFunctionBootstrap> lambdaBootstraps;


  public OptimizeCodeGenerator(final AviatorEvaluatorInstance instance,
      final ClassLoader classLoader, final OutputStream traceOutStream, final boolean trace) {
    this.instance = instance;
    this.codeGen =
        new ASMCodeGenerator(instance, (AviatorClassLoader) classLoader, traceOutStream, trace);
    this.trace = trace;
  }

  private Env getCompileEnv() {
    if (this.compileEnv == null) {
      this.compileEnv = new Env();
      this.compileEnv.setInstance(this.instance);
    }
    return this.compileEnv;
  }


  @Override
  public void setParser(final Parser parser) {
    this.parser = parser;
    this.codeGen.setParser(parser);
  }


  private Map<Integer, DelegateTokenType> getIndex2DelegateTypeMap(final OperatorType opType) {
    Map<Integer, DelegateTokenType> result = new HashMap<Integer, DelegateTokenType>();
    switch (opType) {
      case AND:
        result.put(2, DelegateTokenType.And_Left);
        break;
      case OR:
        result.put(2, DelegateTokenType.Join_Left);
        break;
      case TERNARY:
        result.put(4, DelegateTokenType.Ternary_Boolean);
        result.put(2, DelegateTokenType.Ternary_Left);
        break;
    }
    return result;
  }


  private int execute() {
    int exeCount = 0;
    final int size = this.tokenList.size();
    printTokenList();
    for (int i = 0; i < size; i++) {
      Token<?> token = this.tokenList.get(i);
      if (token.getType() == TokenType.Operator) {
        final OperatorToken op = (OperatorToken) token;
        final OperatorType operatorType = op.getOperatorType();
        final int operandCount = operatorType.getOperandCount();
        switch (operatorType) {
          case FUNC:
          case INDEX:
            // Could not optimize function and index call
            break;
          default:
            Map<Integer, DelegateTokenType> index2DelegateType =
                getIndex2DelegateTypeMap(operatorType);
            final int result = executeOperator(i, operatorType, operandCount, index2DelegateType);
            if (result < 0) {
              compactTokenList();
              return exeCount;
            }
            exeCount += result;
            break;
        }

      }
    }
    compactTokenList();
    return exeCount;
  }


  private int executeOperator(final int operatorIndex, final OperatorType operatorType,
      int operandCount, final Map<Integer, DelegateTokenType> index2DelegateType) {
    Token<?> token = null;
    operandCount += index2DelegateType.size();
    // check if literal expression can be executed
    boolean canExecute = true;
    // operand count
    int count = 0;
    // operand start index
    int operandStartIndex = -1;
    for (int j = operatorIndex - 1; j >= 0; j--) {
      token = this.tokenList.get(j);
      if (token == null) {
        // we must compact token list and retry executing
        return -1;
      }
      final TokenType tokenType = token.getType();
      // Check if operand is a literal operand
      if (!isLiteralOperand(token, tokenType, count + 1, index2DelegateType)) {
        canExecute = false;
        break;
      }
      count++;

      if (count == operandCount) {
        operandStartIndex = j;
        break;
      }
    }

    // if we can execute it on compile
    if (canExecute) {
      // arguments
      AviatorObject[] args = new AviatorObject[operandCount];
      int index = 0;
      for (int j = operandStartIndex; j < operatorIndex; j++) {
        token = this.tokenList.get(j);
        if (token.getType() == TokenType.Delegate) {
          this.tokenList.set(j, null);
          continue;
        }
        args[index++] = getAviatorObjectFromToken(token);
        // set argument token to null
        this.tokenList.set(j, null);

      }
      AviatorObject result = OperationRuntime.eval(getCompileEnv(), args, operatorType);
      // set result as token to tokenList for next executing
      this.tokenList.set(operatorIndex, getTokenFromOperand(result));
      return 1;
    }
    return 0;
  }


  private boolean isLiteralOperand(final Token<?> token, final TokenType tokenType, final int index,
      final Map<Integer, DelegateTokenType> index2DelegateType) {
    switch (tokenType) {
      case Variable:
        return token == Variable.TRUE || token == Variable.FALSE || token == Variable.NIL;
      case Delegate:
        DelegateTokenType targetDelegateTokenType = index2DelegateType.get(index);
        if (targetDelegateTokenType != null) {
          return targetDelegateTokenType == ((DelegateToken) token).getDelegateTokenType();
        }
        break;
      case Char:
      case Number:
      case Pattern:
      case String:
        return true;
    }
    return false;
  }


  private boolean isLiteralToken(final Token<?> token) {
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

  private boolean isConstant(final Token<?> token) {
    switch (token.getType()) {
      case Number:
      case Pattern:
      case String:
        return true;
      default:
        return false;
    }
  }


  /**
   * Get token from executing result
   *
   * @param operand
   * @return
   */
  private Token<?> getTokenFromOperand(final AviatorObject operand) {
    Token<?> token = null;
    switch (operand.getAviatorType()) {
      case JavaType:
        if (operand instanceof AviatorRuntimeJavaType) {
          Object val = operand.getValue(null);
          if (val == null) {
            token = Variable.NIL;
          } else if (val instanceof Number) {
            token = new NumberToken((Number) val, val.toString());
          } else if (val instanceof String || val instanceof Character) {
            token = new StringToken(val.toString(), -1);
          } else if (val instanceof Pattern) {
            token = new PatternToken(((Pattern) val).pattern(), -1);
          } else if (val instanceof Boolean) {
            token = (boolean) val ? Variable.TRUE : Variable.FALSE;
          } else {
            throw new CompileExpressionErrorException("Invalid operand:" + operand.desc(null));
          }
        } else {
          throw new CompileExpressionErrorException("Invalid operand:" + operand.desc(null));
        }
        break;
      case Boolean:
        token = operand.booleanValue(null) ? Variable.TRUE : Variable.FALSE;
        break;
      case Nil:
        token = Variable.NIL;
        break;
      case BigInt:
      case Decimal:
      case Double:
      case Long:
        final Number value = (Number) operand.getValue(null);
        token = new NumberToken(value, value.toString());
        break;
      case String:
        final String str = (String) operand.getValue(null);
        token = new StringToken(str, -1);
        break;
      case Pattern:
        token = new PatternToken(((AviatorPattern) operand).getPattern().pattern(), -1);
        break;
    }
    return token;
  }


  private void compactTokenList() {
    Iterator<Token<?>> it = this.tokenList.iterator();
    while (it.hasNext()) {
      if (it.next() == null) {
        it.remove();
      }
    }
  }


  private AviatorObject getAviatorObjectFromToken(final Token<?> lookhead) {
    AviatorObject result = null;
    switch (lookhead.getType()) {
      case Number:
        // load numbers
        NumberToken numberToken = (NumberToken) lookhead;
        Number num = numberToken.getNumber();
        result = AviatorNumber.valueOf(num);
        break;
      case String:
        // load string
        result = new AviatorString((String) lookhead.getValue(null));
        break;
      case Pattern:
        // load pattern
        result = new AviatorPattern((String) lookhead.getValue(null));
        break;
      case Variable:
        if (lookhead == Variable.TRUE) {
          result = AviatorBoolean.TRUE;
        } else if (lookhead == Variable.FALSE) {
          result = AviatorBoolean.FALSE;
        } else if (lookhead == Variable.NIL) {
          result = AviatorNil.NIL;
        }
        break;
      case Char:
        result = new AviatorString(String.valueOf(lookhead.getValue(null)));
        break;
    }
    return result;
  }


  @Override
  public Expression getResult() {
    // execute literal expression
    while (execute() > 0) {
      ;
    }

    Map<String, Integer/* counter */> variables = new LinkedHashMap<String, Integer>();
    Map<String, Integer/* counter */> methods = new HashMap<String, Integer>();
    Set<Token<?>> constants = new HashSet<>();
    for (Token<?> token : this.tokenList) {

      if (isConstant(token)) {
        constants.add(token);
      }

      switch (token.getType()) {
        case Variable:
          String varName = token.getLexeme();
          if (!variables.containsKey(varName)) {
            variables.put(varName, 1);
          } else {
            variables.put(varName, variables.get(varName) + 1);
          }

          break;
        case Delegate:
          DelegateToken delegateToken = (DelegateToken) token;
          if (delegateToken.getDelegateTokenType() == DelegateTokenType.Method_Name) {
            Token<?> realToken = delegateToken.getToken();
            if (realToken == null) {
              continue;
            }
            if (realToken.getType() == TokenType.Variable) {
              String methodName = token.getLexeme();
              if (!methods.containsKey(methodName)) {
                methods.put(methodName, 1);
              } else {
                methods.put(methodName, methods.get(methodName) + 1);
              }
            }
          } else if (delegateToken.getDelegateTokenType() == DelegateTokenType.Array) {
            Token<?> realToken = delegateToken.getToken();
            if (realToken.getType() == TokenType.Variable) {
              varName = token.getLexeme();
              if (!variables.containsKey(varName)) {
                variables.put(varName, 1);
              } else {
                variables.put(varName, variables.get(varName) + 1);
              }
            }
          }
          break;
      }
    }

    Expression exp = null;

    // Last token is a literal token,then return a LiteralExpression
    if (this.tokenList.size() <= 1) {
      if (this.tokenList.isEmpty()) {
        exp = new LiteralExpression(this.instance, null, new ArrayList<String>(variables.keySet()));
      } else {
        final Token<?> lastToken = this.tokenList.get(0);
        if (isLiteralToken(lastToken)) {
          exp = new LiteralExpression(this.instance,
              getAviatorObjectFromToken(lastToken).getValue(null),
              new ArrayList<String>(variables.keySet()));
        }
      }
    }

    if (exp == null) {
      // call asm to generate byte codes
      callASM(variables, methods, constants);
      // get result from asm
      exp = this.codeGen.getResult();
    }


    if (exp instanceof BaseExpression) {
      ((BaseExpression) exp).setCompileEnv(getCompileEnv());
    }
    return exp;
  }


  private void callASM(final Map<String, Integer/* counter */> variables,
      final Map<String, Integer/* counter */> methods, final Set<Token<?>> constants) {
    this.codeGen.initConstants(constants);
    this.codeGen.initVariables(variables);
    this.codeGen.initMethods(methods);
    this.codeGen.setLambdaBootstraps(this.lambdaBootstraps);
    this.codeGen.start();

    for (int i = 0; i < this.tokenList.size(); i++) {
      Token<?> token = this.tokenList.get(i);
      switch (token.getType()) {
        case Operator:
          OperatorToken op = (OperatorToken) token;

          switch (op.getOperatorType()) {
            case ADD:
              this.codeGen.onAdd(token);
              break;
            case SUB:
              this.codeGen.onSub(token);
              break;
            case MULT:
              this.codeGen.onMult(token);
              break;
            case DIV:
              this.codeGen.onDiv(token);
              break;
            case MOD:
              this.codeGen.onMod(token);
              break;
            case EQ:
              this.codeGen.onEq(token);
              break;
            case NEQ:
              this.codeGen.onNeq(token);
              break;
            case LT:
              this.codeGen.onLt(token);
              break;
            case LE:
              this.codeGen.onLe(token);
              break;
            case GT:
              this.codeGen.onGt(token);
              break;
            case GE:
              this.codeGen.onGe(token);
              break;
            case NOT:
              this.codeGen.onNot(token);
              break;
            case NEG:
              this.codeGen.onNeg(token);
              break;
            case AND:
              this.codeGen.onAndRight(token);
              break;
            case OR:
              this.codeGen.onJoinRight(token);
              break;
            case FUNC:
              this.codeGen.onMethodInvoke(token, op.getParams());
              break;
            case INDEX:
              this.codeGen.onArrayIndexEnd(token);
              break;
            case MATCH:
              this.codeGen.onMatch(token);
              break;
            case TERNARY:
              this.codeGen.onTernaryRight(token);
              break;
            case BIT_AND:
              this.codeGen.onBitAnd(token);
              break;
            case BIT_OR:
              this.codeGen.onBitOr(token);
              break;
            case BIT_XOR:
              this.codeGen.onBitXor(token);
              break;
            case BIT_NOT:
              this.codeGen.onBitNot(token);
              break;
            case SHIFT_LEFT:
              this.codeGen.onShiftLeft(token);
              break;
            case SHIFT_RIGHT:
              this.codeGen.onShiftRight(token);
              break;
            case ASSIGNMENT:
              this.codeGen.onAssignment(token);
              break;
            case U_SHIFT_RIGHT:
              this.codeGen.onUnsignedShiftRight(token);
              break;
          }
          break;
        case Delegate:
          DelegateToken delegateToken = (DelegateToken) token;
          final Token<?> realToken = delegateToken.getToken();
          switch (delegateToken.getDelegateTokenType()) {
            case And_Left:
              this.codeGen.onAndLeft(realToken);
              break;
            case Join_Left:
              this.codeGen.onJoinLeft(realToken);
              break;
            case Array:
              this.codeGen.onArray(realToken);
              break;
            case Index_Start:
              this.codeGen.onArrayIndexStart(realToken);
              break;
            case Ternary_Boolean:
              this.codeGen.onTernaryBoolean(realToken);
              break;
            case Ternary_Left:
              this.codeGen.onTernaryLeft(realToken);
              break;
            case Method_Name:
              this.codeGen.onMethodName(realToken);
              break;
            case Method_Param:
              this.codeGen.onMethodParameter(realToken);
              break;
            case Lambda_New:
              this.codeGen.genNewLambdaCode(delegateToken.getLambdaFunctionBootstrap());
              break;
            case Ternay_End:
              this.codeGen.onTernaryEnd(realToken);
              break;
          }
          break;

        default:
          this.codeGen.onConstant(token);
          break;
      }

    }
  }


  private void printTokenList() {
    if (this.trace) {
      for (Token<?> t : this.tokenList) {
        System.out.print(t.getLexeme() + " ");
      }
      System.out.println();
    }
  }


  @Override
  public void onAdd(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.ADD));

  }


  @Override
  public void onAndLeft(final Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.And_Left));
  }


  @Override
  public void onAndRight(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.AND));

  }


  @Override
  public void onConstant(final Token<?> lookhead) {
    this.tokenList.add(lookhead);
  }


  @Override
  public void onDiv(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.DIV));

  }


  @Override
  public void onArrayIndexStart(final Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Index_Start));

  }



  @Override
  public void onAssignment(final Token<?> lookhead) {
    this.tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(),
        OperatorType.ASSIGNMENT));
  }


  @Override
  public void onArrayIndexEnd(final Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.INDEX));
  }


  @Override
  public void onArray(final Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Array));

  }


  @Override
  public void onEq(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.EQ));

  }


  @Override
  public void onGe(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.GE));

  }


  @Override
  public void onGt(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.GT));

  }


  @Override
  public void onJoinLeft(final Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Join_Left));
  }


  @Override
  public void onJoinRight(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.OR));

  }


  @Override
  public void onLe(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.LE));

  }


  @Override
  public void onLt(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.LT));

  }


  @Override
  public void onMatch(final Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.MATCH));

  }


  @Override
  public void onMethodInvoke(final Token<?> lookhead, final List<FunctionArgument> params) {
    OperatorToken token =
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.FUNC);
    token.setParams(params);
    this.tokenList.add(token);

  }


  @Override
  public void onMethodName(final Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Method_Name));

  }


  @Override
  public void onMethodParameter(final Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Method_Param));

  }



  @Override
  public void onLambdaDefineStart(final Token<?> lookhead) {
    if (this.lambdaGenerator == null) {
      // TODO cache?
      this.lambdaGenerator =
          new LambdaGenerator(this.instance, this, this.parser, this.codeGen.getClassLoader());
      this.lambdaGenerator.setScopeInfo(this.parser.enterScope());
    } else {
      throw new CompileExpressionErrorException("Compile lambda error");
    }
  }


  @Override
  public void onLambdaArgument(final Token<?> lookhead) {
    this.lambdaGenerator.addArgument(lookhead.getLexeme());
  }


  @Override
  public void onLambdaBodyStart(final Token<?> lookhead) {
    this.parentCodeGenerator = this.parser.getCodeGenerator();
    this.parser.setCodeGenerator(this.lambdaGenerator);
  }


  @Override
  public void onLambdaBodyEnd(final Token<?> lookhead) {
    this.lambdaGenerator.compileCallMethod();
    LambdaFunctionBootstrap bootstrap = this.lambdaGenerator.getLmabdaBootstrap();
    if (this.lambdaBootstraps == null) {
      this.lambdaBootstraps = new HashMap<String, LambdaFunctionBootstrap>();
    }
    this.lambdaBootstraps.put(bootstrap.getName(), bootstrap);
    DelegateToken token = new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(),
        lookhead, DelegateTokenType.Lambda_New);
    token.setLambdaFunctionBootstrap(bootstrap);
    this.tokenList.add(token);
    this.parser.restoreScope(this.lambdaGenerator.getScopeInfo());
    this.lambdaGenerator = null;
    this.parser.setCodeGenerator(this.parentCodeGenerator);
  }


  @Override
  public void onMod(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.MOD));

  }


  @Override
  public void onMult(final Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.MULT));

  }


  @Override
  public void onNeg(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.NEG));

  }


  @Override
  public void onNeq(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.NEQ));

  }


  @Override
  public void onNot(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.NOT));

  }


  @Override
  public void onSub(final Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.SUB));

  }


  @Override
  public void onTernaryBoolean(final Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Ternary_Boolean));

  }


  @Override
  public void onTernaryLeft(final Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Ternary_Left));

  }


  @Override
  public void onTernaryRight(final Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.TERNARY));
  }


  @Override
  public void onTernaryEnd(final Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Ternay_End));
  }


  @Override
  public void onBitAnd(final Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.BIT_AND));

  }


  @Override
  public void onBitNot(final Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.BIT_NOT));
  }


  @Override
  public void onBitOr(final Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.BIT_OR));

  }


  @Override
  public void onShiftLeft(final Token<?> lookhead) {
    this.tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(),
        OperatorType.SHIFT_LEFT));

  }


  @Override
  public void onShiftRight(final Token<?> lookhead) {
    this.tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(),
        OperatorType.SHIFT_RIGHT));

  }


  @Override
  public void onUnsignedShiftRight(final Token<?> lookhead) {
    this.tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(),
        OperatorType.U_SHIFT_RIGHT));

  }


  @Override
  public void onBitXor(final Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.BIT_XOR));

  }

}
