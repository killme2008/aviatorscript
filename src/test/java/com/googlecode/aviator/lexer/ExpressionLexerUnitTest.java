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
package com.googlecode.aviator.lexer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Token.TokenType;
import com.googlecode.aviator.lexer.token.Variable;


public class ExpressionLexerUnitTest {
  private ExpressionLexer lexer;
  private AviatorEvaluatorInstance instance;

  @Before
  public void setup() {
    this.instance = AviatorEvaluator.newInstance();
  }

  @Test
  public void testSimpleExpression() {
    this.lexer = new ExpressionLexer(this.instance, "1+2");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(1, token.getValue(null));
    assertEquals(0, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('+', token.getValue(null));
    assertEquals(1, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(2, token.getValue(null));
    assertEquals(2, token.getStartIndex());

    assertNull(this.lexer.scan());
  }


  @Test
  public void testParseBigNumber() {
    this.lexer = new ExpressionLexer(this.instance, "92233720368547758071");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(new BigInteger("92233720368547758071"), token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test
  public void testParseHexNumber() {
    this.lexer = new ExpressionLexer(this.instance, "0X0a2B");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(2603, token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test
  public void testParseBigInteger() {
    this.lexer = new ExpressionLexer(this.instance, "3N");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(new BigInteger("3"), token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test
  public void testParseScientificNotation1() {
    this.lexer = new ExpressionLexer(this.instance, "3e2");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(300.0, token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test
  public void testParseScientificNotationBiggerSmaller() {
    this.lexer = new ExpressionLexer(this.instance, "3e10");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(3e10, token.getValue(null));
    assertEquals(0, token.getStartIndex());

    this.lexer = new ExpressionLexer(this.instance, "3e100");
    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(3e100, token.getValue(null));
    assertEquals(0, token.getStartIndex());

    this.lexer = new ExpressionLexer(this.instance, "3e-100");
    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(3e-100, token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test
  public void testParseScientificNotation2() {
    this.lexer = new ExpressionLexer(this.instance, "3E2");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(300.0, token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test
  public void testParseScientificNotation3() {
    this.lexer = new ExpressionLexer(this.instance, "3e-1");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(0.3, token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test
  public void testParseScientificNotation4() {
    this.lexer = new ExpressionLexer(this.instance, "3E-2");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(0.03, token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test
  public void testParseScientificNotation5() {
    this.lexer = new ExpressionLexer(this.instance, "3E2M");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(new BigDecimal("300"), token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test
  public void testParseScientificNotation6() {
    this.lexer = new ExpressionLexer(this.instance, "3e-1M");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(new BigDecimal("0.3"), token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test(expected = CompileExpressionErrorException.class)
  public void testParseScientificNotation7() {
    this.lexer = new ExpressionLexer(this.instance, "3e3N");
    Token<?> token = this.lexer.scan();
  }


  @Test
  public void testParseScientificNotation8() {
    this.lexer = new ExpressionLexer(this.instance, "2.3456e3");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(2345.6, token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test(expected = NumberFormatException.class)
  public void testIllegalNumber1() {
    this.lexer = new ExpressionLexer(this.instance, "3EM+2");
    while (this.lexer.scan() != null) {
      ;
    }
  }


  @Test
  public void testParseBigDecimal() {
    this.lexer = new ExpressionLexer(this.instance, "3.2M");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(new BigDecimal("3.2"), token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test
  public void testParseNumbers() {
    this.lexer = new ExpressionLexer(this.instance, "3N .2M 1 2.3  4.33M");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(new BigInteger("3"), token.getValue(null));
    assertEquals(0, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(new BigDecimal("0.2"), token.getValue(null));
    assertEquals(3, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(1, token.getValue(null));
    assertEquals(7, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(2.3, token.getValue(null));
    assertEquals(9, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(new BigDecimal("4.33"), token.getValue(null));
    assertEquals(14, token.getStartIndex());
  }


  @Test
  public void testParseDoubleAsDecimal() {
    try {
      this.instance.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, true);
      this.lexer = new ExpressionLexer(this.instance, "3.2");
      Token<?> token = this.lexer.scan();
      assertEquals(TokenType.Number, token.getType());
      assertTrue(token.getValue(null) instanceof BigDecimal);
      assertEquals(new BigDecimal("3.2"), token.getValue(null));
      assertEquals(0, token.getStartIndex());
    } finally {
      this.instance.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, false);
    }
  }

  @Test
  public void testParseIntegralAsDecimal() {
    try {
      this.instance.setOption(Options.ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL, true);
      this.lexer = new ExpressionLexer(this.instance, "3");
      Token<?> token = this.lexer.scan();
      assertEquals(TokenType.Number, token.getType());
      assertTrue(token.getValue(null) instanceof BigDecimal);
      assertEquals(new BigDecimal("3"), token.getValue(null));
      assertEquals(0, token.getStartIndex());
    } finally {
      this.instance.setOption(Options.ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL, false);
    }
  }


  @Test
  public void testParseLikeHexNumber() {
    this.lexer = new ExpressionLexer(this.instance, "0344");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(344, token.getValue(null));
    assertEquals(0, token.getStartIndex());
  }


  @Test
  public void testSimpleExpression_WithHexNumber() {
    this.lexer = new ExpressionLexer(this.instance, "3+0xAF");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(3, token.getValue(null));
    assertEquals(0, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('+', token.getValue(null));
    assertEquals(1, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(175, token.getValue(null));
    assertEquals(2, token.getStartIndex());

    assertNull(this.lexer.scan());
  }


  @Test
  public void testSimpleExpression_WithSpace() {
    this.lexer = new ExpressionLexer(this.instance, " 1 + 2 ");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(1, token.getValue(null));
    assertEquals(1, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('+', token.getValue(null));
    assertEquals(3, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(2, token.getValue(null));
    assertEquals(5, token.getStartIndex());

    assertNull(this.lexer.scan());
  }


  @Test
  public void testExpression_WithDouble() {
    this.lexer = new ExpressionLexer(this.instance, "3.0+4-5.9");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(3.0, token.getValue(null));
    assertEquals(0, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('+', token.getValue(null));
    assertEquals(3, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(4, token.getValue(null));
    assertEquals(4, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('-', token.getValue(null));
    assertEquals(5, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(5.9, token.getValue(null));
    assertEquals(6, token.getStartIndex());

    assertNull(this.lexer.scan());

  }


  @Test(expected = CompileExpressionErrorException.class)
  public void testExpression_WithIllegalDouble() {
    this.lexer = new ExpressionLexer(this.instance, "3.0+4-5.9.2");
    while (this.lexer.scan() != null) {
      ;
    }

  }


  @Test
  public void testExpression_True_False() {
    this.lexer = new ExpressionLexer(this.instance, "true");
    Token<?> token = this.lexer.scan();

    assertEquals(TokenType.Variable, token.getType());
    assertTrue((Boolean) token.getValue(null));
    assertNull(this.lexer.scan());

    this.lexer = new ExpressionLexer(this.instance, "false");
    token = this.lexer.scan();

    assertEquals(TokenType.Variable, token.getType());
    assertFalse((Boolean) token.getValue(null));
    assertNull(this.lexer.scan());

  }


  @Test
  public void testQuoteVar() {
    this.lexer = new ExpressionLexer(this.instance, "#abc");
    Token<?> token = this.lexer.scan();

    assertEquals(TokenType.Variable, token.getType());
    assertEquals(token.getValue(null), "abc");
    assertTrue(((Variable) token).isQuote());
    assertNull(this.lexer.scan());

    this.lexer = new ExpressionLexer(this.instance, "#abc.array[0].d");
    token = this.lexer.scan();

    assertEquals(TokenType.Variable, token.getType());
    assertEquals(token.getValue(null), "abc.array[0].d");
    assertTrue(((Variable) token).isQuote());
    assertNull(this.lexer.scan());
  }


  @Test
  public void testExpression_Logic_Join() {
    this.lexer = new ExpressionLexer(this.instance, "a || c ");
    Token<?> token = this.lexer.scan();

    assertEquals(TokenType.Variable, token.getType());
    assertEquals("a", token.getValue(null));

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('|', token.getValue(null));
    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('|', token.getValue(null));

    token = this.lexer.scan();

    assertEquals(TokenType.Variable, token.getType());
    assertEquals("c", token.getValue(null));

    assertNull(this.lexer.scan());

  }


  @Test
  public void testExpression_Eq() {
    this.lexer = new ExpressionLexer(this.instance, "a ==c ");
    Token<?> token = this.lexer.scan();

    assertEquals(TokenType.Variable, token.getType());
    assertEquals("a", token.getValue(null));

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('=', token.getValue(null));
    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('=', token.getValue(null));

    token = this.lexer.scan();
    assertEquals("c", token.getValue(null));

  }


  @Test
  public void testExpression_Not() {
    this.lexer = new ExpressionLexer(this.instance, "!(3<=1)");
    Token<?> token = this.lexer.scan();

    assertEquals(TokenType.Char, token.getType());
    assertEquals('!', token.getValue(null));

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('(', token.getValue(null));

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(3, token.getValue(null));

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('<', token.getValue(null));

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('=', token.getValue(null));

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(1, token.getValue(null));

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals(')', token.getValue(null));

    assertNull(this.lexer.scan());

  }


  @Test
  public void testBlank_SpaceExpression() {
    this.lexer = new ExpressionLexer(this.instance, "");
    assertNull(this.lexer.scan());

    this.lexer = new ExpressionLexer(this.instance, "   ");
    assertNull(this.lexer.scan());

    this.lexer = new ExpressionLexer(this.instance, "\t");
    assertNull(this.lexer.scan());

    this.lexer = new ExpressionLexer(this.instance, "\t \t");
    assertNull(this.lexer.scan());
  }


  @Test
  public void testExpression_Neg() {
    this.lexer = new ExpressionLexer(this.instance, "-10.3");
    Token<?> token = this.lexer.scan();

    assertEquals(TokenType.Char, token.getType());
    assertEquals('-', token.getValue(null));

    token = this.lexer.scan();

    assertEquals(TokenType.Number, token.getType());
    assertEquals(10.3, token.getValue(null));
  }


  @Test
  public void testExpression_Logic_And() {
    this.lexer = new ExpressionLexer(this.instance, "a==3 && false");
    Token<?> token = this.lexer.scan();

    assertEquals(TokenType.Variable, token.getType());
    assertEquals("a", token.getValue(null));

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('=', token.getValue(null));

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('=', token.getValue(null));

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(3, token.getValue(null));

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('&', token.getValue(null));
    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('&', token.getValue(null));

    token = this.lexer.scan();

    assertEquals(TokenType.Variable, token.getType());
    assertFalse((Boolean) token.getValue(null));
    assertNull(this.lexer.scan());

  }


  @Test
  public void testExpression_WithString() {
    this.lexer = new ExpressionLexer(this.instance, "'hello world'");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.String, token.getType());
    assertEquals("hello world", token.getValue(null));
    assertEquals(0, token.getStartIndex());
    assertNull(this.lexer.scan());
  }


  @Test
  public void testExpression_WithNestedString() {
    this.lexer = new ExpressionLexer(this.instance, "'hello \"good\" world'");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.String, token.getType());
    assertEquals("hello \"good\" world", token.getValue(null));
    assertEquals(0, token.getStartIndex());
    assertNull(this.lexer.scan());
  }


  @Test(expected = CompileExpressionErrorException.class)
  public void testExpression_WithIllegalString() {
    this.lexer = new ExpressionLexer(this.instance, "'hello \" world");
    this.lexer.scan();
  }


  @Test
  public void testExpressionHasPattern() {
    this.lexer = new ExpressionLexer(this.instance, "/a\\.f\\d+/");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals("/", token.getLexeme());
    token = this.lexer.scan();
    assertEquals(TokenType.Variable, token.getType());
    assertEquals("a", token.getLexeme());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals("\\", token.getLexeme());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals(".", token.getLexeme());

    token = this.lexer.scan();
    assertEquals(TokenType.Variable, token.getType());
    assertEquals("f", token.getLexeme());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals("\\", token.getLexeme());

    token = this.lexer.scan();
    assertEquals(TokenType.Variable, token.getType());
    assertEquals("d", token.getLexeme());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals("+", token.getLexeme());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals("/", token.getLexeme());

    assertNull(this.lexer.scan());

  }


  @Test
  public void testExpressionHasPattern2() {
    this.lexer = new ExpressionLexer(this.instance, "/\\//");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals("/", token.getLexeme());
    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals("\\", token.getLexeme());
    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals("/", token.getLexeme());
    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals("/", token.getLexeme());
    token = this.lexer.scan();
    assertNull(this.lexer.scan());
  }


  @Test
  public void testExpressionWithParen() {
    this.lexer = new ExpressionLexer(this.instance, "2.0+(2+2)*99");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(2.0, token.getValue(null));
    assertEquals(0, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('+', token.getValue(null));
    assertEquals(3, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('(', token.getValue(null));
    assertEquals(4, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(2, token.getValue(null));
    assertEquals(5, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('+', token.getValue(null));
    assertEquals(6, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(2, token.getValue(null));
    assertEquals(7, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals(')', token.getValue(null));
    assertEquals(8, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('*', token.getValue(null));
    assertEquals(9, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Number, token.getType());
    assertEquals(99, token.getValue(null));
    assertEquals(10, token.getStartIndex());

    assertNull(this.lexer.scan());
  }


  @Test
  public void testNotAnsylyse() {
    this.lexer = new ExpressionLexer(this.instance, "a + b *d+'hello\n'");
    Token<?> token = this.lexer.scan();
    assertEquals(TokenType.Variable, token.getType());
    assertEquals("a", token.getValue(null));
    assertEquals(0, token.getStartIndex());

    token = this.lexer.scan();
    assertEquals(TokenType.Char, token.getType());
    assertEquals('+', token.getValue(null));
    assertEquals(2, token.getStartIndex());

    token = this.lexer.scan(false);
    assertEquals(TokenType.Char, token.getType());
    assertEquals(' ', token.getValue(null));
    assertEquals(3, token.getStartIndex());

    token = this.lexer.scan(false);
    assertEquals(TokenType.Char, token.getType());
    assertEquals('b', token.getValue(null));
    assertEquals(4, token.getStartIndex());

  }


  @Test(expected = CompileExpressionErrorException.class)
  public void testScanHasLine() {
    this.lexer = new ExpressionLexer(this.instance, "4+5>\n5");

    while (this.lexer.scan() != null) {
      ;
    }

  }


  @Test
  public void testPushBack() {
    this.lexer = new ExpressionLexer(this.instance, "13+100");
    Token<?> token = this.lexer.scan();
    assertEquals("13", token.getLexeme());
    this.lexer.pushback(token);
    token = this.lexer.scan();
    assertEquals("13", token.getLexeme());

    token = this.lexer.scan();
    assertEquals("+", token.getLexeme());

    this.lexer.pushback(token);
    token = this.lexer.scan();
    assertEquals("+", token.getLexeme());
    token = this.lexer.scan();
    assertEquals("100", token.getLexeme());
    assertNull(this.lexer.scan());

  }

}
