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
        this.parser = new ExpressionParser(new ExpressionLexer("null"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testIllegalIdentifier2() {
        this.parser = new ExpressionParser(new ExpressionLexer("a.null"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testIllegalIdentifier3() {
        this.parser = new ExpressionParser(new ExpressionLexer("a3.2"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testParseBlankExpression1() {
        this.parser = new ExpressionParser(new ExpressionLexer(""), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testIllegalExpression1() {
        this.parser = new ExpressionParser(new ExpressionLexer("a=2"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testIllegalExpression2() {
        this.parser = new ExpressionParser(new ExpressionLexer("!=3"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testIllegalExpression3() {
        this.parser = new ExpressionParser(new ExpressionLexer("3|4"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testIllegalExpression4() {
        this.parser = new ExpressionParser(new ExpressionLexer("3&4"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testIllegalExpression5() {
        this.parser = new ExpressionParser(new ExpressionLexer("3&~1"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testIllegalExpression6() {
        this.parser = new ExpressionParser(new ExpressionLexer("a!b"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testParseBlankExpression2() {
        this.parser = new ExpressionParser(new ExpressionLexer("\t "), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testParseBlankExpression3() {
        this.parser = new ExpressionParser(new ExpressionLexer("  "), codeGenerator);
        this.parser.parse();
    }


    @Test
    public void testSimpleExpression() {
        this.parser = new ExpressionParser(new ExpressionLexer("1+3"), codeGenerator);
        this.parser.parse();
        assertEquals("1 3 +", this.codeGenerator.getPostFixExpression());

        resetCodeGenerator();
        this.parser = new ExpressionParser(new ExpressionLexer("1+3-2"), codeGenerator);
        this.parser.parse();
        assertEquals("1 3 + 2 -", this.codeGenerator.getPostFixExpression());

        resetCodeGenerator();
        this.parser = new ExpressionParser(new ExpressionLexer("1+3-2/5"), codeGenerator);
        this.parser.parse();
        assertEquals("1 3 + 2 5 / -", this.codeGenerator.getPostFixExpression());

        resetCodeGenerator();
        this.parser = new ExpressionParser(new ExpressionLexer("6==3"), codeGenerator);
        this.parser.parse();
        assertEquals("6 3 ==", this.codeGenerator.getPostFixExpression());

        resetCodeGenerator();
        this.parser = new ExpressionParser(new ExpressionLexer("6>=3 && c==d.a"), codeGenerator);
        this.parser.parse();
        assertEquals("6 3 >= c d.a == &&", this.codeGenerator.getPostFixExpression());

        resetCodeGenerator();
        this.parser = new ExpressionParser(new ExpressionLexer("6>=3 && c==d.a || 0.3<4"), codeGenerator);
        this.parser.parse();
        assertEquals("6 3 >= c d.a == && 0.3 4 < ||", this.codeGenerator.getPostFixExpression());

        resetCodeGenerator();
        this.parser = new ExpressionParser(new ExpressionLexer("!true"), codeGenerator);
        this.parser.parse();
        assertEquals("true !", this.codeGenerator.getPostFixExpression());

        resetCodeGenerator();
        this.parser = new ExpressionParser(new ExpressionLexer("!a && 3==1"), codeGenerator);
        this.parser.parse();
        assertEquals("a ! 3 1 == &&", this.codeGenerator.getPostFixExpression());

        resetCodeGenerator();
        this.parser = new ExpressionParser(new ExpressionLexer("-a+2010"), codeGenerator);
        this.parser.parse();
        assertEquals("a - 2010 +", this.codeGenerator.getPostFixExpression());
    }


    @Test
    public void testParseExpression_WithOneParen() {
        this.parser = new ExpressionParser(new ExpressionLexer("(3+1)/5"), codeGenerator);
        this.parser.parse();
        assertEquals("3 1 + 5 /", this.codeGenerator.getPostFixExpression());

        this.codeGenerator.reset();
        this.parser = new ExpressionParser(new ExpressionLexer("3-(5+2)"), codeGenerator);
        this.parser.parse();
        assertEquals("3 5 2 + -", this.codeGenerator.getPostFixExpression());

        resetCodeGenerator();
        this.parser = new ExpressionParser(new ExpressionLexer("6>=3 && (c==d.a || 0.3<4)"), codeGenerator);
        this.parser.parse();
        assertEquals("6 3 >= c d.a == 0.3 4 < || &&", this.codeGenerator.getPostFixExpression());

    }


    @Test
    public void testParseExpression_WithManyParens1() {
        this.parser =
                new ExpressionParser(new ExpressionLexer("6.3-((3+1)/5+3.14)*600%(2+3-(6+(4.3-9)))"), codeGenerator);
        this.parser.parse();
        assertEquals("6.3 3 1 + 5 / 3.14 + 600 * 2 3 + 6 4.3 9 - + - % -", this.codeGenerator.getPostFixExpression());
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testParseExpression_WithIllegalParen1() {
        this.parser = new ExpressionParser(new ExpressionLexer("3+4)"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testParseExpression_WithIllegalParen2() {
        this.parser = new ExpressionParser(new ExpressionLexer("3+4)"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testParseExpression_IllegalParens3() {
        this.parser = new ExpressionParser(new ExpressionLexer("(((((3+4)))"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testParseExpression_IllegalParens4() {
        this.parser = new ExpressionParser(new ExpressionLexer("(((((3+4)))+3"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testParseExpression_IllegalParens2() {
        this.parser = new ExpressionParser(new ExpressionLexer("3-(5+6/(c+d)"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testParseExpression_IllegalParens() {
        this.parser = new ExpressionParser(new ExpressionLexer("3-(5+6/(c+d)"), codeGenerator);
        this.parser.parse();
    }


    @Test
    public void testParsePattern() {
        this.parser = new ExpressionParser(new ExpressionLexer("'456.5'=~/[\\d\\.]+/"), codeGenerator);
        this.parser.parse();
        assertEquals("456.5 [\\d\\.]+ =~", this.codeGenerator.getPostFixExpression());
    }


    @Test
    public void testParseComplexPattern() {
        this.parser =
                new ExpressionParser(new ExpressionLexer(
                    "'killme2008@gmail.com'=~/[a-zA-Z0-9_]+[@][a-zA-Z0-9]+([\\.com]|[\\.cn])/"), codeGenerator);
        this.parser.parse();
        assertEquals("killme2008@gmail.com [a-zA-Z0-9_]+[@][a-zA-Z0-9]+([\\.com]|[\\.cn]) =~", codeGenerator
            .getPostFixExpression());

    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testIllegalPattern() {
        this.parser =
                new ExpressionParser(new ExpressionLexer("/[a-zA-Z0-9_]+[@][a-zA-Z0-9]+([\\.com]|[\\.cn])/cdf/"),
                    codeGenerator);
        this.parser.parse();
    }


    @Test
    public void testParseMorePattern() {
        this.parser =
                new ExpressionParser(new ExpressionLexer("/[a-zA-Z0-9_]+[@][a-zA-Z0-9]+([\\.com]|[\\.cn])/==/hello/"),
                    codeGenerator);
        this.parser.parse();
        assertEquals("[a-zA-Z0-9_]+[@][a-zA-Z0-9]+([\\.com]|[\\.cn]) hello ==", codeGenerator.getPostFixExpression());
    }


    @Test
    public void testParsePatternWithOtherExpression() {
        this.parser = new ExpressionParser(new ExpressionLexer(" !false || '456.5'=~/[\\d\\.]+/"), codeGenerator);
        this.parser.parse();
        assertEquals("false ! 456.5 [\\d\\.]+ =~ ||", this.codeGenerator.getPostFixExpression());
    }


    @Test
    public void testParseExpression_WithManyParens2() {
        this.parser =
                new ExpressionParser(new ExpressionLexer("5+(5+(5+a*1.02)*1.02)*1.02-600/(4*b-(c+d))"), codeGenerator);
        this.parser.parse();
        assertEquals("5 5 5 a 1.02 * + 1.02 * + 1.02 * + 600 4 b * c d + - / -", this.codeGenerator
            .getPostFixExpression());
    }


    @Test
    public void testCommonPattern() {
        matchPattern("^\\d+$");
        matchPattern("^[0-9]*[1-9][0-9]*$");

        matchPattern("^((-\\d+) ?(0+))$");
        matchPattern("^-[0-9]*[1-9][0-9]*$");
        matchPattern("^-?\\d+$");
        matchPattern("^\\d+(\\.\\d+)?$");
        matchPattern("^(([0-9]+\\.[0-9]*[1-9][0-9]*) ?([0-9]*[1-9][0-9]*\\.[0-9]+) ?([0-9]*[1-9][0-9]*))$");
        matchPattern("^((-\\d+(\\.\\d+)?) ?(0+(\\.0+)?))$");
        matchPattern("^(-(([0-9]+\\.[0-9]*[1-9][0-9]*) ?([0-9]*[1-9][0-9]*\\.[0-9]+) ?([0-9]*[1-9][0-9]*)))$");
        matchPattern("^(-?\\d+)(\\.\\d+)?$");
        matchPattern("^[A-Za-z]+$");
        matchPattern("^[A-Z]+$");
        matchPattern("^[a-z]+$");
        matchPattern("^[A-Za-z0-9]+$");
        matchPattern("^\\w+$");
        matchPattern("^[\\w-]+(\\.[\\w-]+)*@[\\w-]+(\\.[\\w-]+)+$");
        matchPattern("^[a-zA-z]+:\\/\\/(\\w+(-\\w+)*)(\\.(\\w+(-\\w+)*))*(\\?\\S*)?$");
        matchPattern("[\u4e00-\u9fa5]");
        matchPattern("[^\\x00-\\xff]");
        matchPattern("^\\x00-\\xff]");
        matchPattern("\\n[\\s ? ]*\\r");
        matchPattern("<(.*)>.*<\\/\\1>?<(.*)\\/>");
        matchPattern("(^\\s*)?(\\s*$)");
        matchPattern("[A-Z][a-z][a-z] [0-9][0-9]*, [0-9]\\{4\\}");
    }


    @Test
    public void testComplexLogicExpression() {
        this.parser = new ExpressionParser(new ExpressionLexer("a>b && (c<=d || e!=3.14) && !f"), codeGenerator);
        this.parser.parse();
        assertEquals("a b > c d <= e 3.14 != || && f ! &&", this.codeGenerator.getPostFixExpression());
    }


    private void matchPattern(String pattern) {
        this.codeGenerator.reset();
        this.parser = new ExpressionParser(new ExpressionLexer("/" + pattern + "/"), codeGenerator);
        this.parser.parse();
        assertEquals(pattern, this.codeGenerator.getPostFixExpression());
    }


    @Test
    public void testPattern_Escape() {
        Pattern.compile("http:\\/\\/www\\.google\\.com");
        this.parser =
                new ExpressionParser(new ExpressionLexer("'http://google.com'=~/http:\\/\\/www\\.google\\.com/"),
                    codeGenerator);
        this.parser.parse();
        assertEquals("http://google.com http:\\/\\/www\\.google\\.com =~", this.codeGenerator.getPostFixExpression());
    }


    private void resetCodeGenerator() {
        this.codeGenerator.reset();
    }


    @Test
    public void testTernary1() {
        this.parser = new ExpressionParser(new ExpressionLexer("3>1?1:-3"), codeGenerator);
        this.parser.parse();
        assertEquals("3 1 > 1 3 - ?:", this.codeGenerator.getPostFixExpression());
    }


    @Test
    public void testTernary2() {
        int d = 3 > 1 ? 6 <= 7 ? 0 : 100 : 3 > 2 ? 9 : 0;
        this.parser = new ExpressionParser(new ExpressionLexer("3>1?6<=7?0:100:3>2?9:0"), codeGenerator);
        this.parser.parse();
        assertEquals("3 1 > 6 7 <= 0 100 ?: 3 2 > 9 0 ?: ?:", this.codeGenerator.getPostFixExpression());
    }


    @Test
    public void testTernary3() {
        this.parser = new ExpressionParser(new ExpressionLexer("3>1?true:false?1:0"), codeGenerator);
        this.parser.parse();
        assertEquals("3 1 > true false 1 0 ?: ?:", codeGenerator.getPostFixExpression());
        Object d = 3 > 1 ? true : false ? 1 : 0;
        System.out.println(d);

    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testIllegalTernary1() {
        this.parser = new ExpressionParser(new ExpressionLexer("3>1?true"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testIllegalTernary2() {
        this.parser = new ExpressionParser(new ExpressionLexer("3>1?true:"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testIllegalTernary3() {
        this.parser = new ExpressionParser(new ExpressionLexer("3>1?true:false?9"), codeGenerator);
        this.parser.parse();
    }


    @Test
    public void testTernaryWithParen1() {
        this.parser = new ExpressionParser(new ExpressionLexer("3==1?(9.0-3>5?-1:2):(false?9:0)"), codeGenerator);
        this.parser.parse();
        assertEquals("3 1 == 9.0 3 - 5 > 1 - 2 ?: false 9 0 ?: ?:", this.codeGenerator.getPostFixExpression());
    }


    @Test
    public void testTernaryWithParen2() {
        this.parser = new ExpressionParser(new ExpressionLexer("3==1?(100-(3+1)):(false?9:0)"), codeGenerator);
        this.parser.parse();
        assertEquals("3 1 == 100 3 1 + - false 9 0 ?: ?:", this.codeGenerator.getPostFixExpression());
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testTernaryWithIllegalParen1() {
        this.parser = new ExpressionParser(new ExpressionLexer("3==1?(100-(3+1):(false?9:0)"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testTernaryWithIllegalParen2() {
        this.parser = new ExpressionParser(new ExpressionLexer("3==1?(100-3+1)):(false?9:0)"), codeGenerator);
        this.parser.parse();
    }


    @Test
    public void testParseGroup() {
        this.parser = new ExpressionParser(new ExpressionLexer("'3.45'=~/(\\d+)\\.(\\d+)/ ? $2 : $0 "), codeGenerator);
        this.parser.parse();
        assertEquals("3.45 (\\d+)\\.(\\d+) =~ $2 $0 ?:", this.codeGenerator.getPostFixExpression());
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testParseIllegalGroup1() {
        this.parser =
                new ExpressionParser(new ExpressionLexer("'3.45'=~/(\\d+)\\.(\\d+)/ ? $2.3 : $0 "), codeGenerator);
        this.parser.parse();

    }


    @Test
    public void testParseFunction() {
        this.parser = new ExpressionParser(new ExpressionLexer("string.contains(\"hello\",'fuck')"), codeGenerator);
        this.parser.parse();

        assertEquals("hello fuck method<invoked>", codeGenerator.getPostFixExpression());

    }


    @Test
    public void testParseSeqFunction() {
        this.parser = new ExpressionParser(new ExpressionLexer("map(list,println)"), codeGenerator);
        this.parser.parse();

        assertEquals("list println method<invoked>", codeGenerator.getPostFixExpression());

    }


    @Test
    public void testParseReduceFunction() {
        this.parser = new ExpressionParser(new ExpressionLexer("reduce(list,-,0)"), codeGenerator);
        this.parser.parse();

        assertEquals("list - 0 method<invoked>", codeGenerator.getPostFixExpression());

    }


    @Test
    public void testParseFunctionNested() {
        this.parser =
                new ExpressionParser(new ExpressionLexer(
                    "string.contains(string.substring(\"hello\",3,4),string.substring(\"hello\",1)) && 3>2"),
                    codeGenerator);
        this.parser.parse();

        assertEquals("hello 3 4 method<invoked> hello 1 method<invoked> method<invoked> 3 2 > &&", codeGenerator
            .getPostFixExpression());

    }


    @Test
    public void testArrayAccess() {
        this.parser = new ExpressionParser(new ExpressionLexer("a[2]"), codeGenerator);
        this.parser.parse();

        assertEquals("a 2 []", codeGenerator.getPostFixExpression());
    }


    @Test
    public void testArrayAccess_IndexIsExp() {
        this.parser = new ExpressionParser(new ExpressionLexer("a[b+c/2]"), codeGenerator);
        this.parser.parse();

        assertEquals("a b c 2 / + []", codeGenerator.getPostFixExpression());
    }


    @Test
    public void testArrayAccessNested1() {
        this.parser = new ExpressionParser(new ExpressionLexer("a[c[3]]"), codeGenerator);
        this.parser.parse();

        assertEquals("a c 3 [] []", codeGenerator.getPostFixExpression());

    }


    @Test
    public void testArrayAccessNested2() {
        this.parser = new ExpressionParser(new ExpressionLexer("a[c[3+c[y*2]]]"), codeGenerator);
        this.parser.parse();

        assertEquals("a c 3 c y 2 * [] + [] []", codeGenerator.getPostFixExpression());

    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testArrayAccess_Illegal1() {
        this.parser = new ExpressionParser(new ExpressionLexer("ab+c/2]"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testArrayAccess_Illegal2() {
        this.parser = new ExpressionParser(new ExpressionLexer("a[c[3+c[y*2]]"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testArrayAccess_Illegal3() {
        this.parser = new ExpressionParser(new ExpressionLexer("a[c[3+true[y*2]]]"), codeGenerator);
        this.parser.parse();
    }


    @Test(expected = ExpressionSyntaxErrorException.class)
    public void testArrayAccess_Illegal4() {
        this.parser = new ExpressionParser(new ExpressionLexer("a[c3+c[y*2]]]"), codeGenerator);
        this.parser.parse();
    }

}
