package com.googlecode.aviator;

import com.googlecode.aviator.annotation.Import;

@Import(ns = "print")
public class PrintModule {
  public static void print(final Float value) {
    System.out.println("float: " + value);
  }

  public static void print(final Long value) {
    System.out.println("long: " + value);
  }

  public static void print(final String value) {
    System.out.println("str: " + value);
  }
}
