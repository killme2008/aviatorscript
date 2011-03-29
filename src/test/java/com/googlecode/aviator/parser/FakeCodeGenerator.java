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
package com.googlecode.aviator.parser;

import com.googlecode.aviator.Expression;
import com.googlecode.aviator.code.CodeGenerator;
import com.googlecode.aviator.lexer.token.Token;


/**
 * Fake CodeGenerator,transform infix expression to postfix expression
 * 
 * @author dennis
 * 
 */
public class FakeCodeGenerator implements CodeGenerator {
    private StringBuffer sb = new StringBuffer();

    private boolean wasFirst = true;


    public void reset() {
        this.sb = new StringBuffer();
        this.wasFirst = true;
    }


    public Expression getResult() {
        return null;
    }


    public String getPostFixExpression() {
        return this.sb.toString();
    }


    public void onAdd(Token<?> lookhead) {
        appendToken("+");
    }


    private void appendToken(String s) {
        if (wasFirst) {
            wasFirst = false;
            sb.append(s);
        }
        else {
            sb.append(" ").append(s);
        }
    }


    public void onAndLeft(Token<?> lookhead) {

    }


    public void onAndRight(Token<?> lookhead) {
        appendToken("&&");

    }


    public void onJoinRight(Token<?> lookhead) {
        appendToken("||");
    }


    public void onTernaryBoolean(Token<?> lookhead) {

    }


    public void onTernaryLeft(Token<?> lookhead) {

    }


    public void onTernaryRight(Token<?> lookhead) {
        appendToken("?:");
    }


    public void onConstant(Token<?> lookhead) {
        appendToken(lookhead.getLexeme());
    }


    public void onDiv(Token<?> lookhead) {
        appendToken("/");

    }


    public void onEq(Token<?> lookhead) {
        appendToken("==");

    }


    public void onGe(Token<?> lookhead) {
        appendToken(">=");

    }


    public void onGt(Token<?> lookhead) {
        appendToken(">");

    }


    public void onJoinLeft(Token<?> lookhead) {

    }


    public void onLe(Token<?> lookhead) {
        appendToken("<=");

    }


    public void onLt(Token<?> lookhead) {
        appendToken("<");

    }


    public void onMatch(Token<?> lookhead) {
        appendToken("=~");

    }


    public void onMod(Token<?> lookhead) {
        appendToken("%");

    }


    public void onMult(Token<?> lookhead) {
        appendToken("*");

    }


    public void onNeg(Token<?> lookhead) {
        appendToken("-");

    }


    public void onNeq(Token<?> lookhead) {
        appendToken("!=");

    }


    public void onNot(Token<?> lookhead) {
        appendToken("!");

    }


    public void onSub(Token<?> lookhead) {
        appendToken("-");
    }


    public void onMethodInvoke(Token<?> lookhead) {
        appendToken("method<invoked>");

    }


    public void onMethodName(Token<?> lookhead) {

    }


    public void onMethodParameter(Token<?> lookhead) {

    }


    public void onElementStart(Token<?> lookhead) {
         appendToken(lookhead.getLexeme());
    }


    public void onElementEnd(Token<?> lookhead) {
        appendToken("[]");

    }

}
