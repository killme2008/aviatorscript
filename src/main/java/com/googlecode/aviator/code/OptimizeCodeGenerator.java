/**
 *  Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 *  This library is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published
 *  by the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.code;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.LiteralExpression;
import com.googlecode.aviator.code.asm.ASMCodeGenerator;
import com.googlecode.aviator.lexer.token.DelegateToken;
import com.googlecode.aviator.lexer.token.NumberToken;
import com.googlecode.aviator.lexer.token.OperatorToken;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.lexer.token.PatternToken;
import com.googlecode.aviator.lexer.token.StringToken;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.lexer.token.DelegateToken.DelegateTokenType;
import com.googlecode.aviator.lexer.token.Token.TokenType;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorDouble;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorNil;
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


    public OptimizeCodeGenerator(ClassLoader classLoader, boolean trace) {
        asmCodeGenerator = new ASMCodeGenerator(AviatorEvaluator.getAviatorClassLoader(), trace);
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
        final int size = tokenList.size();
        printTokenList();
        for (int i = 0; i < size; i++) {
            Token<?> token = tokenList.get(i);
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
                    Map<Integer, DelegateTokenType> index2DelegateType = getIndex2DelegateTypeMap(operatorType);
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
            token = tokenList.get(j);
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
                token = tokenList.get(j);
                if (token.getType() == TokenType.Delegate) {
                    tokenList.set(j, null);
                    continue;
                }
                args[index++] = getAviatorObjectFromToken(token);
                // set argument token to null
                tokenList.set(j, null);

            }
            // execute it now
            AviatorObject result = operatorType.eval(args);
            // set result as token to tokenList for next executing
            tokenList.set(operatorIndex, getTokenFromOperand(result));
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
        case Number:
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
            if (numberToken.getNumber() instanceof Double) {
                result = AviatorDouble.valueOf(numberToken.getNumber());
            }
            else {
                result = AviatorLong.valueOf(numberToken.getNumber());
            }
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
            }
            else if (lookhead == Variable.FALSE) {
                result = AviatorBoolean.FALSE;
            }
            else if (lookhead == Variable.NIL) {
                result = AviatorNil.NIL;
            }
            break;
        case Char:
            result = new AviatorPattern(String.valueOf(lookhead.getValue(null)));
            break;
        }
        return result;
    }


    public Expression getResult() {
        // execute literal expression
        while (execute() > 0) {
            ;
        }

        // call asm to generate byte codes
        callASM();

        // Last token is a literal token,then return a LiteralExpression
        if (tokenList.size() <= 1) {
            if (tokenList.isEmpty()) {
                return new LiteralExpression(null);
            }
            final Token<?> lastToken = tokenList.get(0);
            if (isLiteralToken(lastToken)) {
                return new LiteralExpression(getAviatorObjectFromToken(lastToken).getValue(null));
            }
        }

        // get result from asm
        return asmCodeGenerator.getResult();
    }


    private void callASM() {
        for (int i = 0; i < tokenList.size(); i++) {
            Token<?> token = tokenList.get(i);
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
                    this.asmCodeGenerator.onElementEnd(token);
                    break;
                case MATCH:
                    this.asmCodeGenerator.onMatch(token);
                    break;
                case TERNARY:
                    this.asmCodeGenerator.onTernaryRight(token);
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
                case Element_Start:
                    this.asmCodeGenerator.onElementStart(realToken);
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
        if (trace) {
            for (Token<?> t : tokenList) {
                System.out.print(t.getLexeme() + " ");
            }
            System.out.println();
        }
    }


    public void onAdd(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.ADD));

    }


    public void onAndLeft(Token<?> lookhead) {
        tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
            DelegateTokenType.And_Left));
    }


    public void onAndRight(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.AND));

    }


    public void onConstant(Token<?> lookhead) {
        tokenList.add(lookhead);
    }


    public void onDiv(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.DIV));

    }


    public void onElementEnd(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.INDEX));
    }


    public void onElementStart(Token<?> lookhead) {
        tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
            DelegateTokenType.Element_Start));

    }


    public void onEq(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.EQ));

    }


    public void onGe(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.GE));

    }


    public void onGt(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.GT));

    }


    public void onJoinLeft(Token<?> lookhead) {
        tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
            DelegateTokenType.Join_Left));
    }


    public void onJoinRight(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.OR));

    }


    public void onLe(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.LE));

    }


    public void onLt(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.LT));

    }


    public void onMatch(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.MATCH));

    }


    public void onMethodInvoke(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.FUNC));

    }


    public void onMethodName(Token<?> lookhead) {
        tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
            DelegateTokenType.Method_Name));

    }


    public void onMethodParameter(Token<?> lookhead) {
        tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
            DelegateTokenType.Method_Param));

    }


    public void onMod(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.MOD));

    }


    public void onMult(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.MULT));

    }


    public void onNeg(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.NEG));

    }


    public void onNeq(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.NEQ));

    }


    public void onNot(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.NOT));

    }


    public void onSub(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.SUB));

    }


    public void onTernaryBoolean(Token<?> lookhead) {
        tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
            DelegateTokenType.Ternary_Boolean));

    }


    public void onTernaryLeft(Token<?> lookhead) {
        tokenList.add(new DelegateToken(lookhead == null ? -1 : lookhead.getStartIndex(), lookhead,
            DelegateTokenType.Ternary_Left));

    }


    public void onTernaryRight(Token<?> lookhead) {
        tokenList.add(new OperatorToken(lookhead == null ? -1 : lookhead.getStartIndex(), OperatorType.TERNARY));
    }

}
