package com.googlecode.aviator.example;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import com.googlecode.aviator.AviatorEvaluator;


public class BigNumberExample {

  public static void main(String[] args) {
    Object rt = AviatorEvaluator.exec("9223372036854775807100.356M * 2");
    System.out.println(rt + "  " + rt.getClass());

    rt = AviatorEvaluator.exec("92233720368547758074+1000");
    System.out.println(rt + "  " + rt.getClass());

    BigInteger a = new BigInteger(String.valueOf(Long.MAX_VALUE) + String.valueOf(Long.MAX_VALUE));
    BigDecimal b = new BigDecimal("3.2");
    BigDecimal c = new BigDecimal("9999.99999");

    rt = AviatorEvaluator.exec("a+10000000000000000000", a);
    System.out.println(rt + "  " + rt.getClass());

    rt = AviatorEvaluator.exec("b+c*2", b, c);
    System.out.println(rt + "  " + rt.getClass());

    rt = AviatorEvaluator.exec("a*b/c", a, b, c);
    System.out.println(rt + "  " + rt.getClass());

    // set math context
    AviatorEvaluator.setMathContext(MathContext.DECIMAL64);
    rt = AviatorEvaluator.exec("a*b/c", a, b, c);
    System.out.println(rt + "  " + rt.getClass());
  }
}
