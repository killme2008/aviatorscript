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
        peek = iterator.current();
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
        return scan(true);
    }


    public void nextChar() {
        this.peek = iterator.next();
    }


    public Token<?> scan(boolean analyse) {
        // If buffer is not empty,return
        if (!tokenBuffer.isEmpty()) {
            return tokenBuffer.pop();
        }
        // Skip white space or line
        for (;; nextChar()) {
            if (peek == CharacterIterator.DONE) {
                return null;
            }

            if (analyse) {
                if (peek == ' ' || peek == '\t') {
                    continue;
                }
                if (peek == '\n') {
                    throw new CompileExpressionErrorException("Aviator doesn't support newline expression,index="
                            + iterator.getIndex());
                }
                else {
                    break;
                }
            }
            else {
                char ch = peek;
                int index = this.iterator.getIndex();
                nextChar();
                return new CharToken(ch, index);
            }

        }

        // If it is a digit
        if (Character.isDigit(peek) || peek == '.') {
            StringBuffer sb = new StringBuffer();
            int startIndex = iterator.getIndex();
            Number value = 0L;
            boolean hasDot = false;
            double d = 10.0;
            do {
                sb.append(peek);
                if (peek == '.') {
                    if (hasDot) {
                        throw new CompileExpressionErrorException("Illegal Number, index=" + iterator.getIndex());
                    }
                    else {
                        hasDot = true;
                        value = new Double(value.longValue());
                        nextChar();
                    }

                }
                else {
                    if (!hasDot) {
                        value = 10 * value.longValue() + Character.digit(peek, 10);
                        nextChar();
                    }
                    else {
                        value = value.doubleValue() + Character.digit(peek, 10) / d;
                        d = d * 10;
                        nextChar();
                    }
                }
            } while (Character.isDigit(peek) || peek == '.');
            return new NumberToken(value, sb.toString(), startIndex);
        }

        // It is a variable
        if (Character.isJavaIdentifierStart(peek)) {
            int startIndex = iterator.getIndex();
            StringBuilder sb = new StringBuilder();
            do {
                sb.append(peek);
                nextChar();
            } while (Character.isJavaIdentifierPart(peek) || peek == '.');
            String lexeme = sb.toString();
            Variable variable = new Variable(lexeme, startIndex);
            // If it is a reserved word(true or false)
            if (symbolTable.contains(lexeme)) {
                return symbolTable.getVariable(lexeme);
            }
            else {
                symbolTable.reserve(lexeme, variable);
                return variable;
            }

        }

        if (isBinaryOP(peek)) {
            CharToken opToken = new CharToken(peek, iterator.getIndex());
            nextChar();
            return opToken;
        }
        // String
        if (peek == '"' || peek == '\'') {
            char left = peek;
            int startIndex = iterator.getIndex();
            StringBuilder sb = new StringBuilder();
            while ((peek = iterator.next()) != left) {
                if (peek == CharacterIterator.DONE) {
                    throw new CompileExpressionErrorException("Illegal String,start index=" + startIndex);
                }
                else {
                    sb.append(peek);
                }
            }
            nextChar();
            return new StringToken(sb.toString(), startIndex);
        }

        Token<Character> token = new CharToken(peek, iterator.getIndex());
        nextChar();
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
