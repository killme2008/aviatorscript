package com.googlecode.aviator.code;

import java.util.ArrayList;
import java.util.List;

import com.googlecode.aviator.Expression;
import com.googlecode.aviator.LiteralExpression;
import com.googlecode.aviator.lexer.token.Token;


public class ConstantsCollector implements CodeGenerator {

    public List<Token<?>> getConstants() {
        return constants;
    }

    private List<Token<?>> constants = new ArrayList<Token<?>>();


    @Override
    public void onShiftRight(Token<?> lookhead) {
        throw new UnsupportedOperationException();
    }


    @Override
    public void onShiftLeft(Token<?> lookhead) {
        throw new UnsupportedOperationException();
    }


    @Override
    public void onUnsignedShiftRight(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onBitOr(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onBitAnd(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onBitXor(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onBitNot(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onAdd(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onSub(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onMult(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onDiv(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onAndLeft(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onAndRight(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onTernaryBoolean(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onTernaryLeft(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onTernaryRight(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onJoinLeft(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onJoinRight(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onEq(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onMatch(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onNeq(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onLt(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onLe(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onGt(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onGe(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onMod(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onNot(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onNeg(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public Expression getResult() {
        return new LiteralExpression(this.constants, null);
    }


    @Override
    public void onConstant(Token<?> lookhead) {
        this.constants.add(lookhead);
    }


    @Override
    public void onMethodName(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onMethodParameter(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onMethodInvoke(Token<?> lookhead) {
        throw new UnsupportedOperationException();

    }


    @Override
    public void onArray(Token<?> lookhead) {
        throw new UnsupportedOperationException();
    }


    @Override
    public void onArrayIndexStart(Token<?> token) {
        throw new UnsupportedOperationException();
    }


    @Override
    public void onArrayIndexEnd(Token<?> lookhead) {
        throw new UnsupportedOperationException();
    }
}
