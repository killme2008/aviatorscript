package com.googlecode.aviator.spring;

import static org.junit.Assert.assertEquals;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import com.googlecode.aviator.AviatorEvaluator;

public class SringContextFunctionLoaderTest {

  private SringContextFunctionLoader loader;

  @Before
  public void setUp() {
    ApplicationContext ctx = new ClassPathXmlApplicationContext("context.xml");
    loader = new SringContextFunctionLoader(ctx);
    AviatorEvaluator.addFunctionLoader(loader);
  }

  @Test
  public void testAdd() {
    assertEquals(100, AviatorEvaluator.exec("springAdd(x,y)", 1, 99));
  }

  @AfterClass
  public static void tearDown() {
    AviatorEvaluator.addFunctionLoader(null);
  }

}
