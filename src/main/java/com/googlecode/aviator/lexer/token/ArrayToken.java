package com.googlecode.aviator.lexer.token;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;


/**
 * Array token, only created by parser.
 * 
 * @author dennis
 *
 */
public class ArrayToken implements Token<List<Object>> {
    private List<Token<?>> values;

    private List<Object> result;

    private int index;


    public ArrayToken(List<Token<?>> values, int index) {
        super();
        this.values = values;
        this.index = index;
    }


    @Override
    public List<Object> getValue(Map<String, Object> env) {
        if (result == null) {
            result = new ArrayList<Object>();
            for (Token<?> t : this.values) {
                result.add(t.getValue(env));
            }
        }
        return result;
    }

    @Override
    public com.googlecode.aviator.lexer.token.Token.TokenType getType() {
        return TokenType.Array;
    }


    @Override
    public String getLexeme() {
        StringBuilder sb = new StringBuilder("[");
        boolean wasFirst = true;
        for (Object o : this.values) {
            String literal = "nil";
            if (o != null) {
                if (o instanceof String) {
                    literal = "\"" + o.toString() + "\"";
                }
                else {
                    literal = o.toString();
                }
            }
            if (wasFirst) {
                sb.append(literal);
                wasFirst = false;
            }
            else {
                sb.append(", ").append(literal);
            }
        }
        return sb.toString();
    }


    @Override
    public int getStartIndex() {
        return this.index;
    }

}
