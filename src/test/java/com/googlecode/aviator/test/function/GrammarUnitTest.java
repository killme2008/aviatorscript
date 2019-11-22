/**
 * Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 * <p>
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * <p>
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * <p>
 * You should have received a copy of the GNU Lesser General Public License along with this program;
 * if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 **/
package com.googlecode.aviator.test.function;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;
import junit.framework.Assert;

/**
 * Aviator grammar test
 *
 * @author dennis
 */
public class GrammarUnitTest {

  @Test
  public void testIssue77() {
    AviatorEvaluator.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, true);
    assertTrue((boolean) AviatorEvaluator.execute("'一二三'=~/.*三/"));
    AviatorEvaluator.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, false);
  }


  @Test
  public void testIssue162() {
    Object val = AviatorEvaluator.execute("2017122615550747128008704");
    assertTrue(val instanceof BigInteger);
    assertEquals(new BigInteger("2017122615550747128008704"), val);

    val = AviatorEvaluator.execute("-2017122615550747128008704");
    assertTrue(val instanceof BigInteger);
    assertEquals(new BigInteger("-2017122615550747128008704"), val);
  }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIssue177() {
    AviatorEvaluator.compile("$age >30 ($age < 20)");
  }

  @Test
  public void testIssue175() {
    Map<String, Object> env = new HashMap<>();
    env.put("date1", new Date());
    env.put("date2", null);
    assertTrue((boolean) AviatorEvaluator.execute("date1>date2", env));
  }

  @Test
  public void testIssue87() {
    AviatorEvaluator.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, true);
    assertEquals(1, (long) AviatorEvaluator.execute("long(1.2)"));
    AviatorEvaluator.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, false);
  }

  @Test
  public void testIssue92() {
    HashMap<String, Object> env = new HashMap<>();
    assertEquals("\\", AviatorEvaluator.execute("'\\\\'", env));
  }

  // 增加测试用例
  @Test
  public void testIntegralIntoDecimal() {
    AviatorEvaluator.setOption(Options.ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL, true);
    assertEquals(1.5D, ((BigDecimal) AviatorEvaluator.execute("3/2")).doubleValue());
    AviatorEvaluator.setOption(Options.ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL, false);
  }

  /**
   * 类型测试
   */
  @Test
  public void testType() {
    assertTrue(AviatorEvaluator.execute("1") instanceof Long);
    assertTrue(AviatorEvaluator.execute("3.2") instanceof Double);
    assertTrue(AviatorEvaluator.execute(Long.MAX_VALUE + "") instanceof Long);
    assertTrue(AviatorEvaluator.execute("3.14159265") instanceof Double);

    assertEquals("hello world", AviatorEvaluator.execute("'hello world'"));
    assertEquals("hello world", AviatorEvaluator.execute("\"hello world\""));
    assertEquals("hello \" world", AviatorEvaluator.execute("'hello \" world'"));
    assertEquals("hello 'world'", AviatorEvaluator.execute("\"hello 'world'\""));
    assertEquals("hello 'world' 'dennis'", AviatorEvaluator.execute("\"hello 'world' 'dennis'\""));

    assertTrue((Boolean) AviatorEvaluator.execute("true"));
    assertFalse((Boolean) AviatorEvaluator.execute("false"));

    assertEquals("\\w+\\d?\\..*", AviatorEvaluator.execute("/\\w+\\d?\\..*/").toString());
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("_a", 3);
    assertEquals(3, AviatorEvaluator.execute("_a", env));
    long now = System.currentTimeMillis();
    env.put("currentTime", now);
    assertEquals(now, AviatorEvaluator.execute("currentTime", env));

  }

  public class Foo {
    int a;

    public Foo() {

    }

    public Foo(final int a) {
      super();
      this.a = a;
    }

    public int getA() {
      return this.a;
    }

    public void setA(final int a) {
      this.a = a;
    }

  }

  public class Bar extends Foo {
    int b;

    public Bar() {

    }

    public Bar(final int a, final int b) {
      super(a);
      this.b = b;
    }

    public int getB() {
      return this.b;
    }

    public void setB(final int b) {
      this.b = b;
    }

  }

  /**
   * 类型转换
   */
  @Test
  public void testTypeConversation() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("foo", new Foo(100));
    env.put("bar", new Bar(99, 999));
    env.put("date", new Date());

    Map<String, Object> map = new HashMap<String, Object>();
    map.put("key", "aviator");
    env.put("tmap", map);
    env.put("bool", Boolean.FALSE);

    // long op long=long
    assertTrue(AviatorEvaluator.execute("3+3") instanceof Long);
    assertTrue(AviatorEvaluator.execute("3+3/2") instanceof Long);
    assertTrue(AviatorEvaluator.execute("foo.a+bar.a", env) instanceof Long);
    assertEquals(1098L, AviatorEvaluator.execute("bar.a+bar.b", env));

    // double op double=double
    assertTrue(AviatorEvaluator.execute("3.2+3.3") instanceof Double);
    assertTrue(AviatorEvaluator.execute("3.01+3.1/2.1") instanceof Double);
    assertTrue(AviatorEvaluator.execute("3.19+3.1/2.9-1.0/(6.0002*7.7+8.9)") instanceof Double);

    // double + long=double
    assertTrue(AviatorEvaluator.execute("3+0.02") instanceof Double);
    assertTrue(AviatorEvaluator.execute("3+0.02-100") instanceof Double);
    assertTrue(AviatorEvaluator.execute("3+3/2-1/(6*7+8.0)") instanceof Double);
    assertTrue(AviatorEvaluator.execute("foo.a+3.2-1000", env) instanceof Double);

    // object + string =string
    assertEquals("hello world", AviatorEvaluator.execute("'hello '+ 'world'"));
    assertEquals("hello aviator", AviatorEvaluator.execute("'hello '+tmap.key", env));
    assertEquals("true aviator", AviatorEvaluator.execute("true+' '+tmap.key", env));
    assertEquals("100aviator", AviatorEvaluator.execute("foo.a+tmap.key", env));
    assertEquals("\\d+hello", AviatorEvaluator.execute("/\\d+/+'hello'"));
    assertEquals("3.2aviator", AviatorEvaluator.execute("3.2+tmap.key", env));
    assertEquals("false is false", AviatorEvaluator.execute("bool+' is false'", env));

  }

  @Test
  public void testNotOperandLimit() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("bool", false);

    assertFalse((Boolean) AviatorEvaluator.execute("!true"));
    assertTrue((Boolean) AviatorEvaluator.execute("!bool", env));

    try {
      AviatorEvaluator.execute("!3");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("!3.3");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("!/\\d+/");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("!'hello'");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }

  }

  @Test
  public void testNegOperandLimit() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("d", -3.3);

    assertEquals(-3L, AviatorEvaluator.execute("-3"));
    assertEquals(3.3, (Double) AviatorEvaluator.execute("-d", env), 0.001);

    try {
      AviatorEvaluator.execute("-true");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("-'hello'");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("-/\\d+/");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
  }

  @Test
  public void testAddOperandsLimit() {
    Map<String, Object> env = createEnv();

    assertEquals(6, AviatorEvaluator.execute("1+2+3"));
    assertEquals(2.7, (Double) AviatorEvaluator.execute("6+d", env), 0.001);
    assertEquals("hello aviator", AviatorEvaluator.execute("'hello '+s", env));
    assertEquals("-3.3aviator", AviatorEvaluator.execute("d+s", env));
    assertEquals("trueaviator", AviatorEvaluator.execute("bool+s", env));
    assertEquals("1aviator3", AviatorEvaluator.execute("1+s+3", env));

    Foo foo = new Foo(2);
    env.put("foo", foo);
    assertEquals(6, AviatorEvaluator.execute("1+foo.a+3", env));
    try {
      AviatorEvaluator.execute("foo+s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("d+bool", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("1+bool+3", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("/\\d+/+100", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }

  }

  @Test
  public void testSubOperandsLimit() {
    Map<String, Object> env = createEnv();
    assertEquals(3, AviatorEvaluator.execute("6-1-2", env));
    assertEquals(2.86, (Double) AviatorEvaluator.execute("6-3.14"), 0.001);
    assertEquals(4.3, (Double) AviatorEvaluator.execute("1-d", env), 0.001);
    assertEquals(0.0, (Double) AviatorEvaluator.execute("d-d", env), 0.001);
    assertEquals(1003.3, (Double) AviatorEvaluator.execute("a-d", env), 0.001);
    doArithOpIllegalOperands("-");
  }

  @Test
  public void testMultOperandsLimit() {
    Map<String, Object> env = createEnv();
    assertEquals(300, AviatorEvaluator.execute("100*3", env));
    assertEquals(18.84, (Double) AviatorEvaluator.execute("6*3.14"), 0.001);
    assertEquals(-9.9, (Double) AviatorEvaluator.execute("d*3", env), 0.001);
    assertEquals(10.89, (Double) AviatorEvaluator.execute("d*d", env), 0.001);
    assertEquals(-3300, (Double) AviatorEvaluator.execute("a*d", env), 0.001);
    doArithOpIllegalOperands("*");
  }

  @Test
  public void testDivOperandsLimit() {
    Map<String, Object> env = createEnv();
    assertEquals(33, AviatorEvaluator.execute("100/3", env));
    assertEquals(1.9108, (Double) AviatorEvaluator.execute("6/3.14"), 0.001);
    assertEquals(-1.1, (Double) AviatorEvaluator.execute("d/3", env), 0.001);
    assertEquals(1.0, (Double) AviatorEvaluator.execute("d/d", env), 0.001);
    assertEquals(-303.030, (Double) AviatorEvaluator.execute("a/d", env), 0.001);
    doArithOpIllegalOperands("/");
  }

  @Test
  public void testModOperandsLimit() {
    Map<String, Object> env = createEnv();
    assertEquals(1, AviatorEvaluator.execute("100%3", env));
    assertEquals(2.86, (Double) AviatorEvaluator.execute("6%3.14"), 0.001);
    assertEquals(-0.29999, (Double) AviatorEvaluator.execute("d%3", env), 0.001);
    assertEquals(0.0, (Double) AviatorEvaluator.execute("d%d", env), 0.001);
    assertEquals(1000 % -3.3, (Double) AviatorEvaluator.execute("a%d", env), 0.001);
    doArithOpIllegalOperands("%");
  }

  private void doArithOpIllegalOperands(final String op) {
    try {
      AviatorEvaluator.execute("1" + op + "/\\d+/");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("true" + op + "true");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("'hello world'" + op + "'hello'");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("1" + op + "s");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }

    try {
      AviatorEvaluator.execute("bool" + op + "d");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("a" + op + "s");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("s" + op + "1000");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("bool" + op + "90.0");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("/hello/" + op + "/good/");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
  }

  @Test
  public void testMatch() {
    assertTrue((Boolean) AviatorEvaluator.execute("'10'=~/^\\d+$/"));
    assertTrue((Boolean) AviatorEvaluator.execute("'99'=~/^\\d+$/"));
    assertTrue((Boolean) AviatorEvaluator.execute("'0'=~/^\\d+$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'-3'=~/^\\d+$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'-0'=~/^\\d+$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'aviator'=~/^\\d+$/"));

    assertTrue((Boolean) AviatorEvaluator.execute("'10'=~/^[0-9]*[1-9][0-9]*$/"));
    assertTrue((Boolean) AviatorEvaluator.execute("'1'=~/^[0-9]*[1-9][0-9]*$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'0'=~/^[0-9]*[1-9][0-9]*$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'-3'=~/^[0-9]*[1-9][0-9]*$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'aviator'=~/^[0-9]*[1-9][0-9]*$/"));

    assertTrue((Boolean) AviatorEvaluator.execute("'-10'=~/^((-\\d+)|(0+))$/"));
    assertTrue((Boolean) AviatorEvaluator.execute("'-99'=~/^((-\\d+)|(0+))$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'99'=~/^((-\\d+)|(0+))$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'1'=~/^((-\\d+)|(0+))$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'aviator'=~/^((-\\d+)|(0+))$/"));

    // ^-?\d+$
    assertTrue((Boolean) AviatorEvaluator.execute("'-10'=~/^-?\\d+$/"));
    assertTrue((Boolean) AviatorEvaluator.execute("'0'=~/^-?\\d+$/"));
    assertTrue((Boolean) AviatorEvaluator.execute("'10'=~/^-?\\d+$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'aviator'=~/^-?\\d+$/"));

    assertTrue((Boolean) AviatorEvaluator.execute("'abc'=~/^[A-Za-z]+$/"));
    assertTrue((Boolean) AviatorEvaluator.execute("'ABC'=~/^[A-Za-z]+$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'123'=~/^[A-Za-z]+$/"));

    assertFalse((Boolean) AviatorEvaluator.execute("'abc'=~/^[A-Z]+$/"));
    assertTrue((Boolean) AviatorEvaluator.execute("'ABC'=~/^[A-Z]+$/"));

    assertTrue((Boolean) AviatorEvaluator.execute("'abc'=~/^[a-z]+$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'ABC'=~/^[a-z]+$/"));

    assertTrue((Boolean) AviatorEvaluator.execute(
        "'0595-97357355'=~/^((\\+?[0-9]{2,4}\\-[0-9]{3,4}\\-)|([0-9]{3,4}\\-))?([0-9]{7,8})(\\-[0-9]+)?$/"));
    assertTrue((Boolean) AviatorEvaluator.execute(
        "'0595-3749306-020'=~/^((\\+?[0-9]{2,4}\\-[0-9]{3,4}\\-)|([0-9]{3,4}\\-))?([0-9]{7,8})(\\-[0-9]+)?$/"));
    assertFalse((Boolean) AviatorEvaluator.execute(
        "'0595-abc'=~/^((\\+?[0-9]{2,4}\\-[0-9]{3,4}\\-)|([0-9]{3,4}\\-))?([0-9]{7,8})(\\-[0-9]+)?$/"));

    assertTrue((Boolean) AviatorEvaluator.execute("'455729032'=~/^[1-9]*[1-9][0-9]*$/"));
    assertFalse((Boolean) AviatorEvaluator.execute("'45d729032'=~/^[1-9]*[1-9][0-9]*$/"));
    assertTrue(
        (Boolean) AviatorEvaluator.execute("'<html>hello</html>'=~/<(.*)>.*<\\/\\1>|<(.*) \\/>/"));
    assertTrue((Boolean) AviatorEvaluator.execute(
        "'127.0.0.1'=~/^(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5]).(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5]).(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5]).(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5])$/"));

    assertFalse((Boolean) AviatorEvaluator.execute(
        "'127.0.0.'=~/^(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5]).(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5]).(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5]).(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5])$/"));
  }

  @Test
  public void testMatchWithFlags() {

    assertTrue((Boolean) AviatorEvaluator.execute("'abc'=~/(?i)^[a-z]+$/"));
    assertTrue((Boolean) AviatorEvaluator.execute("'ABC'=~/(?i)^[a-z]+$/"));
  }

  @Test
  public void testComparePattern() {
    Map<String, Object> env = createEnv();

    assertTrue((Boolean) AviatorEvaluator.execute("p1==p1", env));
    assertTrue((Boolean) AviatorEvaluator.execute("p1>=p1", env));
    assertTrue((Boolean) AviatorEvaluator.execute("p1<=p1", env));
    assertTrue((Boolean) AviatorEvaluator.execute("p1<p2", env));
    assertTrue((Boolean) AviatorEvaluator.execute("p2>p1", env));
    assertFalse((Boolean) AviatorEvaluator.execute("p1>=p2", env));
    assertFalse((Boolean) AviatorEvaluator.execute("p2<=p1", env));
    assertTrue((Boolean) AviatorEvaluator.execute("/aviator/>/abc/", env));
    assertFalse((Boolean) AviatorEvaluator.execute("/aviator/</abc/", env));
    try {
      AviatorEvaluator.execute("3>/abc/");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("'abc'!=/abc/");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("3.999==p1", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("false==p1", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("p2<=bool", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
  }

  @Test
  public void testCompareString() {
    Map<String, Object> env = createEnv();
    assertTrue((Boolean) AviatorEvaluator.execute("'b'>'a'"));
    assertTrue((Boolean) AviatorEvaluator.execute("'b'>='a'"));
    assertTrue((Boolean) AviatorEvaluator.execute("'b'!='a'"));
    assertFalse((Boolean) AviatorEvaluator.execute("'b'<'a'"));
    assertFalse((Boolean) AviatorEvaluator.execute("'b'<='a'"));

    assertTrue((Boolean) AviatorEvaluator.execute("s==s", env));
    assertTrue((Boolean) AviatorEvaluator.execute("s>'abc'", env));
    assertFalse((Boolean) AviatorEvaluator.execute("s<'abc'", env));
    assertFalse((Boolean) AviatorEvaluator.execute("s<='abc'", env));
    assertTrue((Boolean) AviatorEvaluator.execute("s!='abc'", env));
    assertTrue((Boolean) AviatorEvaluator.execute("s>'abc'", env));
    assertTrue((Boolean) AviatorEvaluator.execute("s==s", env));
    assertTrue((Boolean) AviatorEvaluator.execute("nil<'a'", env));
    assertTrue((Boolean) AviatorEvaluator.execute("'a'>nil", env));
    assertTrue((Boolean) AviatorEvaluator.execute("'a'> not_exists", env));
    assertTrue((Boolean) AviatorEvaluator.execute("not_exists<'a'", env));

    try {
      AviatorEvaluator.execute("bool>s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("true<'abc'");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("s>bool", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("100=='hello'");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("s!=d", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("/\\d+/<=s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("'hello'==/[a-zA-Z]/");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
  }

  @Test
  public void testCompareNumber() {
    Map<String, Object> env = createEnv();
    assertTrue((Boolean) AviatorEvaluator.execute("3>1"));
    assertTrue((Boolean) AviatorEvaluator.execute("3>=1"));
    assertTrue((Boolean) AviatorEvaluator.execute("3!=1"));
    assertFalse((Boolean) AviatorEvaluator.execute("3<1"));
    assertFalse((Boolean) AviatorEvaluator.execute("3<=1"));
    assertFalse((Boolean) AviatorEvaluator.execute("3==1"));

    assertTrue((Boolean) AviatorEvaluator.execute("3>=3"));
    assertTrue((Boolean) AviatorEvaluator.execute("3<=3"));
    assertTrue((Boolean) AviatorEvaluator.execute("d<0", env));
    assertTrue((Boolean) AviatorEvaluator.execute("a>3", env));
    assertTrue((Boolean) AviatorEvaluator.execute("d>=d", env));
    assertFalse((Boolean) AviatorEvaluator.execute("d>0", env));
    assertFalse((Boolean) AviatorEvaluator.execute("d>=0", env));
    assertFalse((Boolean) AviatorEvaluator.execute("a<3", env));
    assertFalse((Boolean) AviatorEvaluator.execute("d>=3", env));
    assertFalse((Boolean) AviatorEvaluator.execute("a<=3", env));

    assertTrue((Boolean) AviatorEvaluator.execute("a>=a", env));
    assertTrue((Boolean) AviatorEvaluator.execute("a>d", env));
    assertTrue((Boolean) AviatorEvaluator.execute("d<a", env));

    try {
      AviatorEvaluator.execute("bool>3", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("true<100");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("d>bool", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("100=='hello'");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("'good'>a", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("/\\d+/>3");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("4.9==/[a-zA-Z]/");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
  }

  @Test
  public void testLogicOr() {
    assertTrue((Boolean) AviatorEvaluator.execute("true||true"));
    assertTrue((Boolean) AviatorEvaluator.execute("true||false"));
    assertTrue((Boolean) AviatorEvaluator.execute("false||true"));
    assertFalse((Boolean) AviatorEvaluator.execute("false||false"));
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", Boolean.FALSE);
    env.put("b", Boolean.TRUE);
    env.put("s", "hello");
    env.put("c", 3.3);

    assertTrue((Boolean) AviatorEvaluator.execute("b||b", env));
    assertTrue((Boolean) AviatorEvaluator.execute("b||a", env));
    assertTrue((Boolean) AviatorEvaluator.execute("a||b", env));
    assertFalse((Boolean) AviatorEvaluator.execute("a||a", env));

    try {
      AviatorEvaluator.execute("3 || true");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("false || 3");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("c || 3", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("false || c", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }

    try {
      AviatorEvaluator.execute("false || s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("c || s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("/test/ || s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    // 测试短路
    assertTrue((Boolean) AviatorEvaluator.execute("true || s"));
    assertTrue((Boolean) AviatorEvaluator.execute("true || c"));
    assertTrue((Boolean) AviatorEvaluator.execute("true || 3"));
    assertTrue((Boolean) AviatorEvaluator.execute("true || /hello/"));
  }

  @Test
  public void testLogicAnd() {
    assertTrue((Boolean) AviatorEvaluator.execute("true&&true"));
    assertFalse((Boolean) AviatorEvaluator.execute("true&&false"));
    assertFalse((Boolean) AviatorEvaluator.execute("false && true"));
    assertFalse((Boolean) AviatorEvaluator.execute("false    &&false"));
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", Boolean.FALSE);
    env.put("b", Boolean.TRUE);
    env.put("s", "hello");
    env.put("c", 3.3);

    assertTrue((Boolean) AviatorEvaluator.execute("b&&  b", env));
    assertFalse((Boolean) AviatorEvaluator.execute("b    &&a", env));
    assertFalse((Boolean) AviatorEvaluator.execute("a&&b", env));
    assertFalse((Boolean) AviatorEvaluator.execute("a    &&    a", env));

    try {
      AviatorEvaluator.execute("3 && true");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("true && 3");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("c && 3", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("true && c", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }

    try {
      AviatorEvaluator.execute("true && s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("c&& s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      AviatorEvaluator.execute("/test/ && s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    // 测试短路
    assertFalse((Boolean) AviatorEvaluator.execute("false && s"));
    assertFalse((Boolean) AviatorEvaluator.execute("false &&  c"));
    assertFalse((Boolean) AviatorEvaluator.execute("false &&  3"));
    assertFalse((Boolean) AviatorEvaluator.execute("false &&  /hello/"));

  }

  /*
   * 测试三元表达式
   */
  @Test
  public void testTernaryOperator() {
    Map<String, Object> env = new HashMap<String, Object>();
    int i = 0;
    float f = 3.14f;
    String email = "killme2008@gmail.com";
    char ch = 'a';
    boolean t = true;
    env.put("i", i);
    env.put("f", f);
    env.put("email", email);
    env.put("ch", ch);
    env.put("t", t);

    assertEquals(1, AviatorEvaluator.execute("2>1?1:0"));
    assertEquals(0, AviatorEvaluator.execute("2<1?1:0"));
    assertEquals(f, (Float) AviatorEvaluator.execute("false?i:f", env), 0.001);
    assertEquals(i, AviatorEvaluator.execute("true?i:f", env));
    assertEquals("killme2008",
        AviatorEvaluator.execute("email=~/([\\w0-9]+)@\\w+\\.\\w+/ ? $1:'unknow'", env));

    assertEquals(f, (Float) AviatorEvaluator.execute("ch!='a'?i:f", env), 0.001);
    assertEquals(i, AviatorEvaluator.execute("ch=='a'?i:f", env));
    assertEquals(email, AviatorEvaluator.execute("t?email:ch", env));

    // 多层嵌套
    assertEquals(ch, AviatorEvaluator.execute("t? i>0? f:ch : email", env));

    assertEquals(email, AviatorEvaluator.execute("!t? i>0? f:ch : f>3?email:ch", env));

    // 使用括号
    assertEquals(email, AviatorEvaluator.execute("!t? (i>0? f:ch) :( f>3?email:ch)", env));
    // 测试错误情况
    try {
      AviatorEvaluator.execute("f?1:0", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("'hello'?1:0");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("/test/?1:0");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      AviatorEvaluator.execute("!t? (i>0? f:ch) : f>3?email:ch)", env);
      Assert.fail();
    } catch (ExpressionSyntaxErrorException e) {

    }
    try {
      AviatorEvaluator.execute("!t? (i>0? f:ch : (f>3?email:ch)", env);
      Assert.fail();
    } catch (ExpressionSyntaxErrorException e) {

    }
  }

  /**
   * 测试nil
   */
  @Test
  public void testNilObject() {
    assertTrue((Boolean) AviatorEvaluator.execute("a==nil"));
    assertTrue((Boolean) AviatorEvaluator.execute("nil==a"));

    assertFalse((Boolean) AviatorEvaluator.execute("3==nil"));
    assertTrue((Boolean) AviatorEvaluator.execute("3!=nil"));

    assertFalse((Boolean) AviatorEvaluator.execute("3.5==nil"));
    assertTrue((Boolean) AviatorEvaluator.execute("3.5!=nil"));

    assertFalse((Boolean) AviatorEvaluator.execute("true==nil"));
    assertTrue((Boolean) AviatorEvaluator.execute("true!=nil"));

    assertFalse((Boolean) AviatorEvaluator.execute("false==nil"));
    assertTrue((Boolean) AviatorEvaluator.execute("false!=nil"));

    assertTrue((Boolean) AviatorEvaluator.execute("nil==nil"));
    assertFalse((Boolean) AviatorEvaluator.execute("nil!=nil"));

    assertFalse((Boolean) AviatorEvaluator.execute("'a'==nil"));
    assertTrue((Boolean) AviatorEvaluator.execute("'a'!=nil"));

    assertFalse((Boolean) AviatorEvaluator.execute("/\\d+/==nil"));
    assertTrue((Boolean) AviatorEvaluator.execute("/\\d+/!=nil"));

    Map<String, Object> env = createEnv();
    assertFalse((Boolean) AviatorEvaluator.execute("p1==nil", env));
    assertTrue((Boolean) AviatorEvaluator.execute("p1>nil", env));

    assertFalse((Boolean) AviatorEvaluator.execute("d==nil", env));
    assertTrue((Boolean) AviatorEvaluator.execute("d>nil", env));

    assertFalse((Boolean) AviatorEvaluator.execute("s==nil", env));
    assertTrue((Boolean) AviatorEvaluator.execute("s>nil", env));

    assertFalse((Boolean) AviatorEvaluator.execute("bool==nil", env));
    assertTrue((Boolean) AviatorEvaluator.execute("bool>nil", env));

    assertFalse((Boolean) AviatorEvaluator.execute("a==nil", env));
    assertTrue((Boolean) AviatorEvaluator.execute("a>nil", env));

    // null == null
    assertTrue((Boolean) AviatorEvaluator.execute("a==b"));
    assertFalse((Boolean) AviatorEvaluator.execute("'s'==a"));
    assertTrue((Boolean) AviatorEvaluator.execute("'s'>=a"));
    assertTrue((Boolean) AviatorEvaluator.execute("'s'>a"));
    assertTrue((Boolean) AviatorEvaluator.execute("bool>unknow", env));

  }

  @Test
  public void testIndex() {
    Map<String, Object> env = new HashMap<String, Object>();
    Integer[] a = new Integer[10];
    for (int i = 0; i < a.length; i++) {
      a[i] = i;
    }
    List<String> list = new ArrayList<String>();
    list.add("hello");
    list.add("world");
    env.put("a", a);
    env.put("list", list);
    final HashSet<Integer> set = new HashSet<Integer>();
    set.add(99);
    env.put("set", set);

    for (int i = 0; i < a.length; i++) {
      assertEquals(a[i], AviatorEvaluator.execute("a[" + i + "]", env));
      assertEquals(a[i] + i, AviatorEvaluator.execute("a[" + i + "]+" + i, env));
      assertEquals(a[i] + i, AviatorEvaluator.execute("a[a[a[" + i + "]-10+10]]+" + i, env));
    }

    assertEquals("hello", AviatorEvaluator.execute("list[0]", env));
    assertEquals(5, AviatorEvaluator.execute("string.length(list[0])", env));
    assertEquals("world", AviatorEvaluator.execute("list[1]", env));
    assertEquals("hello world", AviatorEvaluator.execute("list[0]+' '+list[1]", env));

    try {
      AviatorEvaluator.execute("set[0]+' '+set[0]", env);
      fail();
    } catch (ExpressionRuntimeException e) {
      // e.printStackTrace();
    }
  }

  @Test
  public void testArrayIndexAccessWithMethod() {
    assertEquals("a", AviatorEvaluator.exec("string.split('a,b,c,d',',')[0]"));
    assertEquals("b", AviatorEvaluator.exec("string.split('a,b,c,d',',')[1]"));
    assertEquals("c", AviatorEvaluator.exec("string.split('a,b,c,d',',')[2]"));
    assertEquals("d", AviatorEvaluator.exec("string.split('a,b,c,d',',')[3]"));
    try {
      AviatorEvaluator.exec("string.split('a,b,c,d',',')[4]");
      fail();
    } catch (ArrayIndexOutOfBoundsException e) {

    }

    assertEquals("a", AviatorEvaluator.exec("string.split(s,',')[0]", "a,b,c,d"));
    assertEquals("b", AviatorEvaluator.exec("string.split(s,',')[1]", "a,b,c,d"));
    assertEquals("c", AviatorEvaluator.exec("string.split(s,',')[2]", "a,b,c,d"));
    assertEquals("d", AviatorEvaluator.exec("string.split(s,',')[3]", "a,b,c,d"));
  }

  @Test
  public void testArrayIndexAccessWithMethodAndBracket() {
    assertEquals("a", AviatorEvaluator.exec("(string.split('a,b,c,d',','))[0]"));
    assertEquals("b", AviatorEvaluator.exec("(string.split('a,b,c,d',','))[1]"));
    assertEquals("c", AviatorEvaluator.exec("(string.split('a,b,c,d',','))[2]"));
    assertEquals("d", AviatorEvaluator.exec("(string.split('a,b,c,d',','))[3]"));
    try {
      AviatorEvaluator.exec("(string.split('a,b,c,d',','))[4]");
      fail();
    } catch (ArrayIndexOutOfBoundsException e) {

    }
    assertEquals("a", AviatorEvaluator.exec("(string.split(s,','))[0]", "a,b,c,d"));
    assertEquals("b", AviatorEvaluator.exec("(string.split(s,','))[1]", "a,b,c,d"));
    assertEquals("c", AviatorEvaluator.exec("(string.split(s,','))[2]", "a,b,c,d"));
    assertEquals("d", AviatorEvaluator.exec("(string.split(s,','))[3]", "a,b,c,d"));
  }

  @Test(expected = CompileExpressionErrorException.class)
  public void testIllegalArrayIndexAccessWithMethod1() {
    AviatorEvaluator.exec("string.split('a,b,c,d',',')[0");
  }

  @Test(expected = CompileExpressionErrorException.class)
  public void testIllegalArrayIndexAccessWithMethod2() {
    AviatorEvaluator.exec("string.split('a,b,c,d',','[0]");
  }

  @Test
  public void testFunctionCall() {
    Map<String, Object> env = createEnv();
    assertEquals(10, AviatorEvaluator.execute(" string.length('hello') + string.length('hello') "));
    assertEquals(3.0, (Double) AviatorEvaluator
        .execute(" string.length('hello')>5? math.abs(d):math.log10(a) ", env), 0.001);
    assertEquals(3.3, (Double) AviatorEvaluator
        .execute(" string.length('hello')==5? math.abs(d):math.log10(a) ", env), 0.001);

    assertEquals(3.3,
        (Double) AviatorEvaluator.execute(
            "string.contains(p1,'A-Z')? d<0? math.abs(d): math.sqrt(a) : string.length(p2)  ", env),
        0.001);

    Date date = new Date(111, 8, 11);
    env.put("date", date);
    assertEquals("2011-09-11", AviatorEvaluator.execute("date_to_string(date,'yyyy-MM-dd')", env));
    assertEquals(date, AviatorEvaluator.execute("string_to_date('2011-09-11','yyyy-MM-dd')"));

    assertArrayEquals(new String[] {"a", "b", "c"},
        (String[]) AviatorEvaluator.execute("string.split('a,b,c',',')"));
    env.put("tmps", new String[] {"hello", "aviator"});
    assertEquals("hello aviator", AviatorEvaluator.execute("string.join(tmps,\" \")", env));
  }

  private Map<String, Object> createEnv() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("d", -3.3);
    env.put("s", "aviator");
    env.put("bool", true);
    env.put("a", 1000);
    env.put("p1", "[a-z-A-Z]+");
    env.put("p2", "\\d+\\.\\d+");
    return env;
  }

  public class StringGetFromJsonMapFunction extends AbstractFunction {

    @Override
    public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
        final AviatorObject arg2) {
      return new AviatorString(null);
    }

    @Override
    public String getName() {
      return "string.getFromJsonMap";
    }
  }

  @Test
  public void testNullPointerException() {
    AviatorEvaluator.addFunction(new StringGetFromJsonMapFunction());
    String exp = "string.getFromJsonMap(mapJson, key) == 'abc'";
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("mapJson", "{\"key1\":\"value1\", \"key2\":\"value2\"}");
    env.put("key", "key3");
    assertEquals(false, AviatorEvaluator.execute(exp, env));
  }

  @Test
  public void testNil() {
    AviatorEvaluator.addFunction(new StringGetFromJsonMapFunction());
    String exp = "string.getFromJsonMap(mapJson, key) == nil";
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("mapJson", "{\"key1\":\"value1\", \"key2\":\"value2\"}");
    env.put("key", "key3");
    assertEquals(true, AviatorEvaluator.execute(exp, env));
  }

  @Test
  public void testStatement() {
    assertEquals(4, AviatorEvaluator.execute("5;3;4"));
    assertEquals(4, AviatorEvaluator.execute("5;1+2;4"));
    assertEquals(4, AviatorEvaluator.exec("5;1+2;a", 4));
    assertEquals(4, AviatorEvaluator.exec("(3+a)*4;1+2;println(b);a", 4, 5));

    Map<String, Object> env = createEnv();
    assertEquals(3.0, (Double) AviatorEvaluator
        .execute("d+10*a ; string.length('hello')>5? math.abs(d):math.log10(a) ", env), 0.001);
  }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalStatement1() {
    assertEquals(4, AviatorEvaluator.execute("5;3+;4"));
  }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalStatement2() {
    assertEquals(4,
        AviatorEvaluator.execute("d+10*a ; string.length('hello')>5? math.abs(d);4:math.log10(a)"));
  }


  @Test
  public void testScientificNotationBiggerSmaller() {
    assertEquals(4E81 / 3E9, AviatorEvaluator.execute("4E81/3E9"));
    assertEquals(1E9 / 2E101, AviatorEvaluator.execute("1E9/2E101"));

    assertEquals(4E81 / 3E9, AviatorEvaluator.exec("a/3E9", 4E81));
    assertEquals(1E9 / 2E101, AviatorEvaluator.exec("1E9/b", 2E101));
  }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void test4J() {
    System.out.println(AviatorEvaluator.execute("4(ss*^^%%$$$$"));
  }

  @Test
  public void testAssignment1() {
    assertEquals(3, AviatorEvaluator.execute("a=1; a+2"));
    assertEquals(5, AviatorEvaluator.execute("a=3; b=2; a+b"));
    assertEquals(20.0, AviatorEvaluator.execute("a=3; b=2; c=a+b; c*4.0"));
    assertEquals(6, AviatorEvaluator.execute("square = lambda(x) -> x *2 end; square(3)"));
    assertEquals(1, AviatorEvaluator.execute("a=5;b=4.2 ; c= a > b? 1: 0; c"));
    assertEquals(6, AviatorEvaluator.execute("add_n = lambda(x) -> lambda(y) -> x + y end end ; "
        + "add_1 = add_n(1) ; " + "add_2 = add_n(2) ;" + " add_1(add_2(3))"));
  }

  @Test
  public void testAssignment() {
    AviatorEvaluator.setOption(Options.USE_USER_ENV_AS_TOP_ENV_DIRECTLY, true);
    AviatorEvaluator.setOption(Options.PUT_CAPTURING_GROUPS_INTO_ENV, true);
    Map<String, Object> env = new HashMap<>();
    env.put("b", 4);
    Object v = AviatorEvaluator.execute("'hello@4'=~/(.*)@(.*)/ ? a=$2:'not match'", env);
    assertEquals("4", v);
    assertTrue(env.containsKey("$2"));
    assertTrue(env.containsKey("a"));
    assertEquals("4", env.get("a"));
  }
}
