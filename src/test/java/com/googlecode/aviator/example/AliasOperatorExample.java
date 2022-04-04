package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.lexer.token.OperatorType;

public class AliasOperatorExample {
  public static void main(final String[] args) {

    AviatorEvaluator.getInstance().aliasOperator(OperatorType.AND, "and");
    AviatorEvaluator.getInstance().aliasOperator(OperatorType.OR, "or");

    System.out.println(AviatorEvaluator.execute("1==1 and 2==3"));
    System.out.println(AviatorEvaluator.execute("true or false"));
    System.out.println(AviatorEvaluator.execute("true && 1==1 or false"));
  }
}
