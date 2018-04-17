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

import static org.junit.Assert.*;
import java.util.regex.Pattern;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.lexer.ExpressionLexer;


public class ExpressionParserUnitTest {
  private ExpressionParser parser;

  private FakeCodeGenerator codeGenerator;


  @Before
  public void setUp() {
    this.codeGenerator = new FakeCodeGenerator();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalIdentifier1() {
    this.parser = new ExpressionParser(new ExpressionLexer("null"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalIdentifier2() {
    this.parser = new ExpressionParser(new ExpressionLexer("a.null"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalIdentifier3() {
    this.parser = new ExpressionParser(new ExpressionLexer("a3.2"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testParseBlankExpression1() {
    this.parser = new ExpressionParser(new ExpressionLexer(""), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalExpression1() {
    this.parser = new ExpressionParser(new ExpressionLexer("a=2"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalExpression2() {
    this.parser = new ExpressionParser(new ExpressionLexer("!=3"), this.codeGenerator);
    this.parser.parse();
  }


  @Test
  public void testBitOr() {
    this.parser = new ExpressionParser(new ExpressionLexer("3|4"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 4 |", this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testBitAnd() {
    this.parser = new ExpressionParser(new ExpressionLexer("3&4"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 4 &", this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testBitAndNot() {
    this.parser = new ExpressionParser(new ExpressionLexer("3&~1"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 1 ~ &", this.codeGenerator.getPostFixExpression());
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalExpression6() {
    this.parser = new ExpressionParser(new ExpressionLexer("a!b"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testParseBlankExpression2() {
    this.parser = new ExpressionParser(new ExpressionLexer("\t "), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testParseBlankExpression3() {
    this.parser = new ExpressionParser(new ExpressionLexer("  "), this.codeGenerator);
    this.parser.parse();
  }


  @Test
  public void testSimpleExpression() {
    this.parser = new ExpressionParser(new ExpressionLexer("1+3"), this.codeGenerator);
    this.parser.parse();
    assertEquals("1 3 +", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("1+3-2"), this.codeGenerator);
    this.parser.parse();
    assertEquals("1 3 + 2 -", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("1+3-2/5"), this.codeGenerator);
    this.parser.parse();
    assertEquals("1 3 + 2 5 / -", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("6==3"), this.codeGenerator);
    this.parser.parse();
    assertEquals("6 3 ==", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("6>=3 && c==d.a"), this.codeGenerator);
    this.parser.parse();
    assertEquals("6 3 >= c d.a == &&", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser =
        new ExpressionParser(new ExpressionLexer("6>=3 && c==d.a || 0.3<4"), this.codeGenerator);
    this.parser.parse();
    assertEquals("6 3 >= c d.a == && 0.3 4 < ||", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("!true"), this.codeGenerator);
    this.parser.parse();
    assertEquals("true !", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("!a && 3==1"), this.codeGenerator);
    this.parser.parse();
    assertEquals("a ! 3 1 == &&", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("-a+2010"), this.codeGenerator);
    this.parser.parse();
    assertEquals("a - 2010 +", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("3&2^1|4 == 5"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 2 & 1 ^ 4 5 == |", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("3^2&3|4&~1"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 2 3 & ^ 4 1 ~ & |", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser =
        new ExpressionParser(new ExpressionLexer("true || 2&1==0 ? 1 :0"), this.codeGenerator);
    this.parser.parse();
    assertEquals("true 2 1 0 == & || 1 0 ?:", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("3+4>>1"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 4 + 1 >>", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("3-4>>1"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 4 - 1 >>", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("3-4<<1==0"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 4 - 1 << 0 ==", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser = new ExpressionParser(new ExpressionLexer("3-4<<1&3"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 4 - 1 << 3 &", this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testParseExpression_WithOneParen() {
    this.parser = new ExpressionParser(new ExpressionLexer("(3+1)/5"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 1 + 5 /", this.codeGenerator.getPostFixExpression());

    this.codeGenerator.reset();
    this.parser = new ExpressionParser(new ExpressionLexer("3-(5+2)"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 5 2 + -", this.codeGenerator.getPostFixExpression());

    this.resetCodeGenerator();
    this.parser =
        new ExpressionParser(new ExpressionLexer("6>=3 && (c==d.a || 0.3<4)"), this.codeGenerator);
    this.parser.parse();
    assertEquals("6 3 >= c d.a == 0.3 4 < || &&", this.codeGenerator.getPostFixExpression());

  }


  @Test
  public void testParseExpression_WithManyParens1() {
    this.parser = new ExpressionParser(
        new ExpressionLexer("6.3-((3+1)/5+3.14)*600%(2+3-(6+(4.3-9)))"), this.codeGenerator);
    this.parser.parse();
    assertEquals("6.3 3 1 + 5 / 3.14 + 600 * 2 3 + 6 4.3 9 - + - % -",
        this.codeGenerator.getPostFixExpression());
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testParseExpression_WithIllegalParen1() {
    this.parser = new ExpressionParser(new ExpressionLexer("3+4)"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testParseExpression_WithIllegalParen2() {
    this.parser = new ExpressionParser(new ExpressionLexer("3+4)"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testParseExpression_IllegalParens3() {
    this.parser = new ExpressionParser(new ExpressionLexer("(((((3+4)))"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testParseExpression_IllegalParens4() {
    this.parser = new ExpressionParser(new ExpressionLexer("(((((3+4)))+3"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testParseExpression_IllegalParens2() {
    this.parser = new ExpressionParser(new ExpressionLexer("3-(5+6/(c+d)"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testParseExpression_IllegalParens() {
    this.parser = new ExpressionParser(new ExpressionLexer("3-(5+6/(c+d)"), this.codeGenerator);
    this.parser.parse();
  }


  @Test
  public void testParsePattern() {
    this.parser =
        new ExpressionParser(new ExpressionLexer("'456.5'=~/[\\d\\.]+/"), this.codeGenerator);
    this.parser.parse();
    assertEquals("456.5 [\\d\\.]+ =~", this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testParseComplexPattern() {
    this.parser = new ExpressionParser(
        new ExpressionLexer(
            "'killme2008@gmail.com'=~/[a-zA-Z0-9_]+[@][a-zA-Z0-9]+([\\.com]|[\\.cn])/"),
        this.codeGenerator);
    this.parser.parse();
    assertEquals("killme2008@gmail.com [a-zA-Z0-9_]+[@][a-zA-Z0-9]+([\\.com]|[\\.cn]) =~",
        this.codeGenerator.getPostFixExpression());

  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalPattern() {
    this.parser = new ExpressionParser(
        new ExpressionLexer("/[a-zA-Z0-9_]+[@][a-zA-Z0-9]+([\\.com]|[\\.cn])/cdf/"),
        this.codeGenerator);
    this.parser.parse();
  }


  @Test
  public void testParseMorePattern() {
    this.parser = new ExpressionParser(
        new ExpressionLexer("/[a-zA-Z0-9_]+[@][a-zA-Z0-9]+([\\.com]|[\\.cn])/==/hello/"),
        this.codeGenerator);
    this.parser.parse();
    assertEquals("[a-zA-Z0-9_]+[@][a-zA-Z0-9]+([\\.com]|[\\.cn]) hello ==",
        this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testParsePatternWithOtherExpression() {
    this.parser = new ExpressionParser(new ExpressionLexer(" !false || '456.5'=~/[\\d\\.]+/"),
        this.codeGenerator);
    this.parser.parse();
    assertEquals("false ! 456.5 [\\d\\.]+ =~ ||", this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testParseExpression_WithManyParens2() {
    this.parser = new ExpressionParser(
        new ExpressionLexer("5+(5+(5+a*1.02)*1.02)*1.02-600/(4*b-(c+d))"), this.codeGenerator);
    this.parser.parse();
    assertEquals("5 5 5 a 1.02 * + 1.02 * + 1.02 * + 600 4 b * c d + - / -",
        this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testCommonPattern() {
    this.matchPattern("^\\d+$");
    this.matchPattern("^[0-9]*[1-9][0-9]*$");

    this.matchPattern("^((-\\d+) ?(0+))$");
    this.matchPattern("^-[0-9]*[1-9][0-9]*$");
    this.matchPattern("^-?\\d+$");
    this.matchPattern("^\\d+(\\.\\d+)?$");
    this.matchPattern(
        "^(([0-9]+\\.[0-9]*[1-9][0-9]*) ?([0-9]*[1-9][0-9]*\\.[0-9]+) ?([0-9]*[1-9][0-9]*))$");
    this.matchPattern("^((-\\d+(\\.\\d+)?) ?(0+(\\.0+)?))$");
    this.matchPattern(
        "^(-(([0-9]+\\.[0-9]*[1-9][0-9]*) ?([0-9]*[1-9][0-9]*\\.[0-9]+) ?([0-9]*[1-9][0-9]*)))$");
    this.matchPattern("^(-?\\d+)(\\.\\d+)?$");
    this.matchPattern("^[A-Za-z]+$");
    this.matchPattern("^[A-Z]+$");
    this.matchPattern("^[a-z]+$");
    this.matchPattern("^[A-Za-z0-9]+$");
    this.matchPattern("^\\w+$");
    this.matchPattern("^[\\w-]+(\\.[\\w-]+)*@[\\w-]+(\\.[\\w-]+)+$");
    this.matchPattern("^[a-zA-z]+:\\/\\/(\\w+(-\\w+)*)(\\.(\\w+(-\\w+)*))*(\\?\\S*)?$");
    this.matchPattern("[\u4e00-\u9fa5]");
    this.matchPattern("[^\\x00-\\xff]");
    this.matchPattern("^\\x00-\\xff]");
    this.matchPattern("\\n[\\s ? ]*\\r");
    this.matchPattern("<(.*)>.*<\\/\\1>?<(.*)\\/>");
    this.matchPattern("(^\\s*)?(\\s*$)");
    this.matchPattern("[A-Z][a-z][a-z] [0-9][0-9]*, [0-9]\\{4\\}");
  }


  @Test
  public void testComplexLogicExpression() {
    this.parser = new ExpressionParser(new ExpressionLexer("a>b && (c<=d || e!=3.14) && !f"),
        this.codeGenerator);
    this.parser.parse();
    assertEquals("a b > c d <= e 3.14 != || && f ! &&", this.codeGenerator.getPostFixExpression());
  }


  private void matchPattern(String pattern) {
    this.codeGenerator.reset();
    this.parser =
        new ExpressionParser(new ExpressionLexer("/" + pattern + "/"), this.codeGenerator);
    this.parser.parse();
    assertEquals(pattern, this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testPattern_Escape() {
    Pattern.compile("http:\\/\\/www\\.google\\.com");
    this.parser = new ExpressionParser(
        new ExpressionLexer("'http://google.com'=~/http:\\/\\/www\\.google\\.com/"),
        this.codeGenerator);
    this.parser.parse();
    assertEquals("http://google.com http:\\/\\/www\\.google\\.com =~",
        this.codeGenerator.getPostFixExpression());
  }


  private void resetCodeGenerator() {
    this.codeGenerator.reset();
  }


  @Test
  public void testTernary1() {
    this.parser = new ExpressionParser(new ExpressionLexer("3>1?1:-3"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 1 > 1 3 - ?:", this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testTernary2() {
    int d = 3 > 1 ? 6 <= 7 ? 0 : 100 : 3 > 2 ? 9 : 0;
    this.parser =
        new ExpressionParser(new ExpressionLexer("3>1?6<=7?0:100:3>2?9:0"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 1 > 6 7 <= 0 100 ?: 3 2 > 9 0 ?: ?:",
        this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testTernary3() {
    this.parser =
        new ExpressionParser(new ExpressionLexer("3>1?true:false?1:0"), this.codeGenerator);
    this.parser.parse();
    assertEquals("3 1 > true false 1 0 ?: ?:", this.codeGenerator.getPostFixExpression());
    Object d = 3 > 1 ? true : false ? 1 : 0;
    System.out.println(d);

  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalTernary1() {
    this.parser = new ExpressionParser(new ExpressionLexer("3>1?true"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalTernary2() {
    this.parser = new ExpressionParser(new ExpressionLexer("3>1?true:"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalTernary3() {
    this.parser = new ExpressionParser(new ExpressionLexer("3>1?true:false?9"), this.codeGenerator);
    this.parser.parse();
  }


  @Test
  public void testTernaryWithParen1() {
    this.parser = new ExpressionParser(new ExpressionLexer("3==1?(9.0-3>5?-1:2):(false?9:0)"),
        this.codeGenerator);
    this.parser.parse();
    assertEquals("3 1 == 9.0 3 - 5 > 1 - 2 ?: false 9 0 ?: ?:",
        this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testTernaryWithParen2() {
    this.parser = new ExpressionParser(new ExpressionLexer("3==1?(100-(3+1)):(false?9:0)"),
        this.codeGenerator);
    this.parser.parse();
    assertEquals("3 1 == 100 3 1 + - false 9 0 ?: ?:", this.codeGenerator.getPostFixExpression());
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testTernaryWithIllegalParen1() {
    this.parser = new ExpressionParser(new ExpressionLexer("3==1?(100-(3+1):(false?9:0)"),
        this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testTernaryWithIllegalParen2() {
    this.parser = new ExpressionParser(new ExpressionLexer("3==1?(100-3+1)):(false?9:0)"),
        this.codeGenerator);
    this.parser.parse();
  }


  @Test
  public void testParseGroup() {
    this.parser = new ExpressionParser(new ExpressionLexer("'3.45'=~/(\\d+)\\.(\\d+)/ ? $2 : $0 "),
        this.codeGenerator);
    this.parser.parse();
    assertEquals("3.45 (\\d+)\\.(\\d+) =~ $2 $0 ?:", this.codeGenerator.getPostFixExpression());
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testParseIllegalGroup1() {
    this.parser = new ExpressionParser(
        new ExpressionLexer("'3.45'=~/(\\d+)\\.(\\d+)/ ? $2.3 : $0 "), this.codeGenerator);
    this.parser.parse();

  }


  @Test
  public void testParseFunction() {
    this.parser = new ExpressionParser(new ExpressionLexer("string.contains(\"hello\",'fuck')"),
        this.codeGenerator);
    this.parser.parse();

    assertEquals("hello fuck method<invoked>", this.codeGenerator.getPostFixExpression());

  }


  @Test
  public void testParseSeqFunction() {
    this.parser =
        new ExpressionParser(new ExpressionLexer("map(list,println)"), this.codeGenerator);
    this.parser.parse();

    assertEquals("list println method<invoked>", this.codeGenerator.getPostFixExpression());

  }


  @Test
  public void testParseReduceFunction() {
    this.parser = new ExpressionParser(new ExpressionLexer("reduce(list,-,0)"), this.codeGenerator);
    this.parser.parse();

    assertEquals("list - 0 method<invoked>", this.codeGenerator.getPostFixExpression());

  }


  @Test
  public void testParseFunctionNested() {
    this.parser = new ExpressionParser(
        new ExpressionLexer(
            "string.contains(string.substring(\"hello\",3,4),string.substring(\"hello\",1)) && 3>2"),
        this.codeGenerator);
    this.parser.parse();

    assertEquals("hello 3 4 method<invoked> hello 1 method<invoked> method<invoked> 3 2 > &&",
        this.codeGenerator.getPostFixExpression());

  }


  @Test
  public void testArrayAccess() {
    this.parser = new ExpressionParser(new ExpressionLexer("a[2]"), this.codeGenerator);
    this.parser.parse();

    assertEquals("a 2 []", this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testMultiDimensionalArrayAccess1() {
    this.parser = new ExpressionParser(new ExpressionLexer("a[2][3]"), this.codeGenerator);
    this.parser.parse();
    assertEquals("a 2 [] 3 []", this.codeGenerator.getPostFixExpression());

  }


  @Test
  public void testMultiDimensionalArrayAccess2() {

    this.parser =
        new ExpressionParser(new ExpressionLexer("a[1][2] [3]   [4]"), this.codeGenerator);
    this.parser.parse();
    assertEquals("a 1 [] 2 [] 3 [] 4 []", this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testArrayAccess_IndexIsExp() {
    this.parser = new ExpressionParser(new ExpressionLexer("a[b+c/2]"), this.codeGenerator);
    this.parser.parse();

    assertEquals("a b c 2 / + []", this.codeGenerator.getPostFixExpression());
  }


  @Test
  public void testArrayAccessNested1() {
    this.parser = new ExpressionParser(new ExpressionLexer("a[c[3]]"), this.codeGenerator);
    this.parser.parse();

    assertEquals("a c 3 [] []", this.codeGenerator.getPostFixExpression());

  }


  @Test
  public void testArrayAccessNested2() {
    this.parser = new ExpressionParser(new ExpressionLexer("a[c[3+c[y*2]]]"), this.codeGenerator);
    this.parser.parse();

    assertEquals("a c 3 c y 2 * [] + [] []", this.codeGenerator.getPostFixExpression());

  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testArrayAccess_Illegal1() {
    this.parser = new ExpressionParser(new ExpressionLexer("ab+c/2]"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testArrayAccess_Illegal2() {
    this.parser = new ExpressionParser(new ExpressionLexer("a[c[3+c[y*2]]"), this.codeGenerator);
    this.parser.parse();
  }


  // @Test(expected = ExpressionSyntaxErrorException.class)
  // public void testArrayAccess_Illegal3() {
  // this.parser = new ExpressionParser(new
  // ExpressionLexer("a[c[3+true[y*2]]]"), this.codeGenerator);
  // this.parser.parse();
  // }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testArrayAccess_Illegal4() {
    this.parser = new ExpressionParser(new ExpressionLexer("a[c3+c[y*2]]]"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testInvalidExpression1() {
    this.parser = new ExpressionParser(new ExpressionLexer("4(ss*^^%%$$$$"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testInvalidExpression2() {
    this.parser = new ExpressionParser(new ExpressionLexer("4(*)**&^^^^^^^^"), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testInvalidExpression3() {
    this.parser = new ExpressionParser(new ExpressionLexer("4("), this.codeGenerator);
    this.parser.parse();
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testInvalidExpression4() {
    this.parser = new ExpressionParser(new ExpressionLexer("4(*8^####"), this.codeGenerator);
    this.parser.parse();
  }

}
