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
package com.googlecode.aviator.lexer;

import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.Stack;

import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.lexer.token.CharToken;
import com.googlecode.aviator.lexer.token.NumberToken;
import com.googlecode.aviator.lexer.token.StringToken;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Variable;


/**
 * Expression Lexer,scan tokens from string
 * 
 * @author dennis
 * 
 */
public class ExpressionLexer {
    // current char
    private char peek;
    // Char iterator for string
    private final CharacterIterator iterator;
    // symbol table
    private final SymbolTable symbolTable;
    // Tokens buffer
    private final Stack<Token<?>> tokenBuffer = new Stack<Token<?>>();


    public ExpressionLexer(String expression) {
        this.iterator = new StringCharacterIterator(expression);
        this.symbolTable = new SymbolTable();
        this.peek = this.iterator.current();
    }


    /**
     * Push back token
     * 
     * @param token
     */
    public void pushback(Token<?> token) {
        this.tokenBuffer.push(token);
    }


    public Token<?> scan() {
        return this.scan(true);
    }


    public void nextChar() {
        this.peek = this.iterator.next();
    }


    public void prevChar() {
        this.peek = this.iterator.previous();
    }

    static final char[] VALID_HEX_CHAR = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'a', 'B', 'b', 'C',
                                          'c', 'D', 'd', 'E', 'e', 'F', 'f' };


    public boolean isValidHexChar(char ch) {
        for (char c : VALID_HEX_CHAR) {
            if (c == ch) {
                return true;
            }
        }
        return false;
    }


    public int getCurrentIndex() {
        return this.iterator.getIndex();
    }


    public Token<?> scan(boolean analyse) {
        // If buffer is not empty,return
        if (!this.tokenBuffer.isEmpty()) {
            return this.tokenBuffer.pop();
        }
        // Skip white space or line
        for (;; this.nextChar()) {
            if (this.peek == CharacterIterator.DONE) {
                return null;
            }

            if (analyse) {
                if (this.peek == ' ' || this.peek == '\t' || this.peek == '\r') {
                    continue;
                }
                if (this.peek == '\n') {
                    throw new CompileExpressionErrorException("Aviator doesn't support newline expression at "
                            + this.iterator.getIndex());
                }
                else {
                    break;
                }
            }
            else {
                char ch = this.peek;
                int index = this.iterator.getIndex();
                this.nextChar();
                return new CharToken(ch, index);
            }

        }

        // if it is a hex digit
        if (Character.isDigit(this.peek) && this.peek == '0') {
            this.nextChar();
            if (this.peek == 'x' || this.peek == 'X') {
                this.nextChar();
                StringBuffer sb = new StringBuffer();
                int startIndex = this.iterator.getIndex() - 2;
                long value = 0L;
                do {
                    sb.append(this.peek);
                    value = 16 * value + Character.digit(this.peek, 16);
                    this.nextChar();
                } while (this.isValidHexChar(this.peek));
                return new NumberToken(value, sb.toString(), startIndex);
            }
            else {
                this.prevChar();
            }
        }

        // If it is a digit
        if (Character.isDigit(this.peek) || this.peek == '.') {
            StringBuffer sb = new StringBuffer();
            int startIndex = this.iterator.getIndex();
            long lval = 0L;
            double dval = 0d;
            boolean hasDot = false;
            double d = 10.0;
            do {
                sb.append(this.peek);
                if (this.peek == '.') {
                    if (hasDot) {
                        throw new CompileExpressionErrorException("Illegal Number at " + this.iterator.getIndex());
                    }
                    else {
                        hasDot = true;
                        this.nextChar();
                    }

                }
                else {
                    int digit = Character.digit(this.peek, 10);
                    if (!hasDot) {
                        lval = 10 * lval + digit;
                        dval = 10 * dval + digit;
                        this.nextChar();
                    }
                    else {
                        dval = dval + digit / d;
                        d = d * 10;
                        this.nextChar();
                    }
                }
            } while (Character.isDigit(this.peek) || this.peek == '.');
            Number value = lval;
            if (hasDot) {
                value = dval;
            }
            return new NumberToken(value, sb.toString(), startIndex);
        }

        // It is a variable
        if (Character.isJavaIdentifierStart(this.peek)) {
            int startIndex = this.iterator.getIndex();
            StringBuilder sb = new StringBuilder();
            do {
                sb.append(this.peek);
                this.nextChar();
            } while (Character.isJavaIdentifierPart(this.peek) || this.peek == '.');
            String lexeme = sb.toString();
            Variable variable = new Variable(lexeme, startIndex);
            // If it is a reserved word(true or false)
            if (this.symbolTable.contains(lexeme)) {
                return this.symbolTable.getVariable(lexeme);
            }
            else {
                this.symbolTable.reserve(lexeme, variable);
                return variable;
            }

        }

        if (isBinaryOP(this.peek)) {
            CharToken opToken = new CharToken(this.peek, this.iterator.getIndex());
            this.nextChar();
            return opToken;
        }
        // String
        if (this.peek == '"' || this.peek == '\'') {
            char left = this.peek;
            int startIndex = this.iterator.getIndex();
            StringBuilder sb = new StringBuilder();
            while ((this.peek = this.iterator.next()) != left) {
                if (this.peek == CharacterIterator.DONE) {
                    throw new CompileExpressionErrorException("Illegal String at " + startIndex);
                }
                else {
                    sb.append(this.peek);
                }
            }
            this.nextChar();
            return new StringToken(sb.toString(), startIndex);
        }

        Token<Character> token = new CharToken(this.peek, this.iterator.getIndex());
        this.nextChar();
        return token;
    }

    static final char[] OPS = { '=', '>', '<', '+', '-', '*', '/', '%', '!', '&', '|' };


    public static boolean isBinaryOP(char ch) {
        for (char tmp : OPS) {
            if (tmp == ch) {
                return true;
            }
        }
        return false;
    }

}
