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
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.LiteralExpression;
import com.googlecode.aviator.code.asm.ASMCodeGenerator;
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
import com.googlecode.aviator.runtime.op.OperationRuntime;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorNumber;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorPattern;
import com.googlecode.aviator.runtime.type.AviatorString;


/**
 * Optimized code generator
 *
 * @author dennis
 *
 */
public class OptimizeCodeGenerator implements CodeGenerator {
  private final ASMCodeGenerator asmCodeGenerator;

  private final List<Token<?>> tokenList = new ArrayList<Token<?>>();

  private boolean trace = false;


  public OptimizeCodeGenerator(ClassLoader classLoader, OutputStream traceOutStream,
      boolean trace) {
    this.asmCodeGenerator =
        new ASMCodeGenerator(AviatorEvaluator.getAviatorClassLoader(), traceOutStream, trace);
    this.trace = trace;

  }


  private Map<Integer, DelegateTokenType> getIndex2DelegateTypeMap(OperatorType opType) {
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
    this.printTokenList();
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
                this.getIndex2DelegateTypeMap(operatorType);
            final int result =
                this.executeOperator(i, operatorType, operandCount, index2DelegateType);
            if (result < 0) {
              this.compactTokenList();
              return exeCount;
            }
            exeCount += result;
            break;
        }

      }
    }
    this.compactTokenList();
    return exeCount;
  }


  private int executeOperator(int operatorIndex, final OperatorType operatorType, int operandCount,
      Map<Integer, DelegateTokenType> index2DelegateType) {
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
      if (!this.isLiteralOperand(token, tokenType, count + 1, index2DelegateType)) {
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
        args[index++] = this.getAviatorObjectFromToken(token);
        // set argument token to null
        this.tokenList.set(j, null);

      }
      // execute it now
      AviatorObject result = OperationRuntime.eval(args, operatorType);
      // set result as token to tokenList for next executing
      this.tokenList.set(operatorIndex, this.getTokenFromOperand(result));
      return 1;
    }
    return 0;
  }


  private boolean isLiteralOperand(Token<?> token, final TokenType tokenType, int index,
      Map<Integer, DelegateTokenType> index2DelegateType) {
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


  private boolean isLiteralToken(Token<?> token) {
    switch (token.getType()) {
      case Variable:
        return token == Variable.TRUE || token == Variable.FALSE || token == Variable.NIL;
      case Char:
      case Number:
      case Pattern:
      case String:
        return true;
    }
    return false;
  }


  /**
   * Get token from executing result
   *
   * @param operand
   * @return
   */
  private Token<?> getTokenFromOperand(AviatorObject operand) {
    Token<?> token = null;
    switch (operand.getAviatorType()) {
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


  private AviatorObject getAviatorObjectFromToken(Token<?> lookhead) {
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
    while (this.execute() > 0) {
      ;
    }

    Map<String, Integer/* counter */> variables = new LinkedHashMap<String, Integer>();
    Map<String, Integer/* counter */> methods = new HashMap<String, Integer>();
    for (Token<?> token : this.tokenList) {
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
    // call asm to generate byte codes
    this.callASM(variables, methods);

    // Last token is a literal token,then return a LiteralExpression
    if (this.tokenList.size() <= 1) {
      if (this.tokenList.isEmpty()) {
        return new LiteralExpression(null, new ArrayList<String>(variables.keySet()));
      }
      final Token<?> lastToken = this.tokenList.get(0);
      if (this.isLiteralToken(lastToken)) {
        return new LiteralExpression(this.getAviatorObjectFromToken(lastToken).getValue(null),
            new ArrayList<String>(variables.keySet()));
      }
    }

    // get result from asm
    return this.asmCodeGenerator.getResult();
  }


  private void callASM(Map<String, Integer/* counter */> variables,
      Map<String, Integer/* counter */> methods) {
    this.asmCodeGenerator.initVariables(variables);
    this.asmCodeGenerator.initMethods(methods);
    this.asmCodeGenerator.start();

    for (int i = 0; i < this.tokenList.size(); i++) {
      Token<?> token = this.tokenList.get(i);
      switch (token.getType()) {
        case Operator:
          OperatorToken op = (OperatorToken) token;

          switch (op.getOperatorType()) {
            case ADD:
              this.asmCodeGenerator.onAdd(token);
              break;
            case SUB:
              this.asmCodeGenerator.onSub(token);
              break;
            case MULT:
              this.asmCodeGenerator.onMult(token);
              break;
            case DIV:
              this.asmCodeGenerator.onDiv(token);
              break;
            case MOD:
              this.asmCodeGenerator.onMod(token);
              break;
            case EQ:
              this.asmCodeGenerator.onEq(token);
              break;
            case NEQ:
              this.asmCodeGenerator.onNeq(token);
              break;
            case LT:
              this.asmCodeGenerator.onLt(token);
              break;
            case LE:
              this.asmCodeGenerator.onLe(token);
              break;
            case GT:
              this.asmCodeGenerator.onGt(token);
              break;
            case GE:
              this.asmCodeGenerator.onGe(token);
              break;
            case NOT:
              this.asmCodeGenerator.onNot(token);
              break;
            case NEG:
              this.asmCodeGenerator.onNeg(token);
              break;
            case AND:
              this.asmCodeGenerator.onAndRight(token);
              break;
            case OR:
              this.asmCodeGenerator.onJoinRight(token);
              break;
            case FUNC:
              this.asmCodeGenerator.onMethodInvoke(token);
              break;
            case INDEX:
              this.asmCodeGenerator.onArrayIndexEnd(token);
              break;
            case MATCH:
              this.asmCodeGenerator.onMatch(token);
              break;
            case TERNARY:
              this.asmCodeGenerator.onTernaryRight(token);
              break;
            case BIT_AND:
              this.asmCodeGenerator.onBitAnd(token);
              break;
            case BIT_OR:
              this.asmCodeGenerator.onBitOr(token);
              break;
            case BIT_XOR:
              this.asmCodeGenerator.onBitXor(token);
              break;
            case BIT_NOT:
              this.asmCodeGenerator.onBitNot(token);
              break;
            case SHIFT_LEFT:
              this.asmCodeGenerator.onShiftLeft(token);
              break;
            case SHIFT_RIGHT:
              this.asmCodeGenerator.onShiftRight(token);
              break;
            case U_SHIFT_RIGHT:
              this.asmCodeGenerator.onUnsignedShiftRight(token);
              break;
          }
          break;
        case Delegate:
          DelegateToken delegateToken = (DelegateToken) token;
          final Token<?> realToken = delegateToken.getToken();
          switch (delegateToken.getDelegateTokenType()) {
            case And_Left:
              this.asmCodeGenerator.onAndLeft(realToken);
              break;
            case Join_Left:
              this.asmCodeGenerator.onJoinLeft(realToken);
              break;
            case Array:
              this.asmCodeGenerator.onArray(realToken);
              break;
            case Index_Start:
              this.asmCodeGenerator.onArrayIndexStart(realToken);
              break;
            case Ternary_Boolean:
              this.asmCodeGenerator.onTernaryBoolean(realToken);
              break;
            case Ternary_Left:
              this.asmCodeGenerator.onTernaryLeft(realToken);
              break;
            case Method_Name:
              this.asmCodeGenerator.onMethodName(realToken);
              break;
            case Method_Param:
              this.asmCodeGenerator.onMethodParameter(realToken);
              break;
          }
          break;

        default:
          this.asmCodeGenerator.onConstant(token);
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
  public void onAdd(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.ADD));

  }


  @Override
  public void onAndLeft(Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.And_Left));
  }


  @Override
  public void onAndRight(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.AND));

  }


  @Override
  public void onConstant(Token<?> lookhead) {
    this.tokenList.add(lookhead);
  }


  @Override
  public void onDiv(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.DIV));

  }


  @Override
  public void onArrayIndexStart(Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Index_Start));

  }


  @Override
  public void onArrayIndexEnd(Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.INDEX));
  }


  @Override
  public void onArray(Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Array));

  }


  @Override
  public void onEq(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.EQ));

  }


  @Override
  public void onGe(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.GE));

  }


  @Override
  public void onGt(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.GT));

  }


  @Override
  public void onJoinLeft(Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Join_Left));
  }


  @Override
  public void onJoinRight(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.OR));

  }


  @Override
  public void onLe(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.LE));

  }


  @Override
  public void onLt(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.LT));

  }


  @Override
  public void onMatch(Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.MATCH));

  }


  @Override
  public void onMethodInvoke(Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.FUNC));

  }


  @Override
  public void onMethodName(Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Method_Name));

  }


  @Override
  public void onMethodParameter(Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Method_Param));

  }


  @Override
  public void onMod(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.MOD));

  }


  @Override
  public void onMult(Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.MULT));

  }


  @Override
  public void onNeg(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.NEG));

  }


  @Override
  public void onNeq(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.NEQ));

  }


  @Override
  public void onNot(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.NOT));

  }


  @Override
  public void onSub(Token<?> lookhead) {
    this.tokenList
        .add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.SUB));

  }


  @Override
  public void onTernaryBoolean(Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Ternary_Boolean));

  }


  @Override
  public void onTernaryLeft(Token<?> lookhead) {
    this.tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
        DelegateTokenType.Ternary_Left));

  }


  @Override
  public void onTernaryRight(Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.TERNARY));
  }


  @Override
  public void onBitAnd(Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.BIT_AND));

  }


  @Override
  public void onBitNot(Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.BIT_NOT));
  }


  @Override
  public void onBitOr(Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.BIT_OR));

  }


  @Override
  public void onShiftLeft(Token<?> lookhead) {
    this.tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(),
        OperatorType.SHIFT_LEFT));

  }


  @Override
  public void onShiftRight(Token<?> lookhead) {
    this.tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(),
        OperatorType.SHIFT_RIGHT));

  }


  @Override
  public void onUnsignedShiftRight(Token<?> lookhead) {
    this.tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(),
        OperatorType.U_SHIFT_RIGHT));

  }


  @Override
  public void onBitXor(Token<?> lookhead) {
    this.tokenList.add(
        new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.BIT_XOR));

  }

}
