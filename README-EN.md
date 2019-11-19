# Aviator

[![Build Status](https://travis-ci.org/killme2008/aviator.svg?branch=master)](https://travis-ci.org/killme2008/aviator)
[![Code Quality: Java](https://img.shields.io/lgtm/grade/java/g/killme2008/aviator.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/killme2008/aviator/context:java)
[![Maven Central](https://img.shields.io/maven-central/v/com.googlecode.aviator/aviator.svg?label=maven%20central)](https://search.maven.org/search?q=g:com.googlecode.aviator%20AND%20aviator)

📖 English Documentation | [📖 中文文档](README.md)

----------------------------------------

`Aviator` is a lightweight, high performance expression evaluator for `java`.
It compiles expression to byte code and evaluate it on the fly.

# Feature Intro

1. Support almost all operators, including arithmetic operators, relational operators, logical operators, bitwise operators, regular expression matching operators(`=~`), ternary expressions(`?:`).
2. Support operator precedence, or use parentheses to specify precedence explicitly.
3. Support [assignment](https://github.com/killme2008/aviator/wiki/4.0-%E5%8A%9F%E8%83%BD%E8%AF%A6%E7%BB%86%E8%A7%A3%E6%9E%90#%E8%B5%8B%E5%80%BC).
4. Logical operators support short-circuit.
5. Support for rich types such as `nil`, integers and floats, strings, regular expressions, dates, variables, etc.; Supports automatic type conversion and promotion. 
6. [Support `lambda` anonymous functions and closures](https://github.com/killme2008/aviator/wiki/4.0-%E5%8A%9F%E8%83%BD%E8%AF%A6%E7%BB%86%E8%A7%A3%E6%9E%90#lambda-%E5%8C%BF%E5%90%8D%E5%87%BD%E6%95%B0).
7. A powerful set of commonly used [function libraries](https://github.com/killme2008/aviator/wiki/%E5%86%85%E7%BD%AE%E5%87%BD%E6%95%B0) is built-in. 
8. Support [user-customized function](https://github.com/killme2008/aviator/wiki#%E8%87%AA%E5%AE%9A%E4%B9%89%E5%87%BD%E6%95%B0), easy to extend,  support function call point [parameter list capture](https://github.com/killme2008/aviator/wiki/%E5%AE%8C%E6%95%B4%E9%80%89%E9%A1%B9%E5%88%97%E8%A1%A8%E8%AF%B4%E6%98%8E#capture_function_args) and [import java class methods as custom functions](https://github.com/killme2008/aviator/wiki#%E4%BD%BF%E7%94%A8Java%E7%B1%BB%E6%96%B9%E6%B3%95%E4%BD%9C%E4%B8%BA%E8%87%AA%E5%AE%9A%E4%B9%89%E5%87%BD%E6%95%B0).
9. Support [operator overload](https://github.com/killme2008/aviator/wiki#%E9%87%8D%E8%BD%BD%E8%BF%90%E7%AE%97%E7%AC%A6).
10. Support [big numbers(`BigInteger`) and high precision(`BigDecimal`) operations](https://github.com/killme2008/aviator/wiki#%E5%A4%A7%E6%95%B0%E8%AE%A1%E7%AE%97%E5%92%8C%E7%B2%BE%E5%BA%A6). 
11. Support [multi-line expression and customized evaluator instance](https://github.com/killme2008/aviator/wiki/4.0-%E5%8A%9F%E8%83%BD%E8%AF%A6%E7%BB%86%E8%A7%A3%E6%9E%90). 
12. Lightweight and high performance, offer a wide range of [customization options](https://github.com/killme2008/aviator/wiki/%E5%AE%8C%E6%95%B4%E9%80%89%E9%A1%B9%E5%88%97%E8%A1%A8%E8%AF%B4%E6%98%8E). 
13. Function missing mechanism just like ruby's method msising.

See [home page](http://fnil.net/aviator) for details.

# News

* [4.2.6](https://github.com/killme2008/aviator/releases/tag/aviator-4.2.6),  Fixed NPE when comparing java objects, #175
* [4.2.5](https://github.com/killme2008/aviator/releases/tag/aviator-4.2.5),  an usefull feature: function missing.
* [4.2.4](https://github.com/killme2008/aviator/releases/tag/aviator-4.2.4),  adds annotations for importing java class methods and some new features.

# Dependency

```xml
<dependency>
  <groupId>com.googlecode.aviator</groupId>
  <artifactId>aviator</artifactId>
  <version>{version}</version>
</dependency>
```

Check available versions at [search.maven.org](https://search.maven.org/search?q=g:com.googlecode.aviator%20AND%20a:aviator&core=gav).

# Quick Start

```java
int[] a = ...;
Map<String, Object> env = new HashMap<String, Object>();
env.put("a", a);

AviatorEvaluator.execute("1 + 2 + 3");
AviatorEvaluator.execute("a[1] + 100", env);
AviatorEvaluator.execute("'a[1]=' + a[1]", env);

// get the length of the array
AviatorEvaluator.execute("count(a)", env);

// compute the sum of the array
AviatorEvaluator.execute("reduce(a, +, 0)", env);

// check each element of array is between 0 <= e < 10.
AviatorEvaluator.execute("seq.every(a, seq.and(seq.ge(0), seq.lt(10)))", env);

// compute the sum by Lambda
AviatorEvaluator.execute("reduce(a, lambda(x,y) -> x + y end, 0)", env);

// import string instance methods
AviatorEvaluator.addInstanceFunctions("s", String.class);
AviatorEvaluator.execute("s.indexOf('hello', 'l')");
AviatorEvaluator.execute("s.replaceAll('hello', 'l', 'x')");

// import static methods
AviatorEvaluator.addStaticFunctions("sutil", StringUtils.class);
AviatorEvaluator.execute("sutil.isBlank('hello')");

// Call any public instance methods by reflection
AviatorEvaluator.setFunctionMissing(JavaMethodReflectionFunctionMissing.getInstance());
// Calling String#indexOf by reflection
System.out.println(AviatorEvaluator.execute("indexOf('hello world', 'w')"));
// Calling Long#floatValue by reflection
System.out.println(AviatorEvaluator.execute("floatValue(3)"));
// Calling BigDecimal#add by reflection
System.out.println(AviatorEvaluator.execute("add(3M, 4M)"));
```

See [user guide](https://github.com/killme2008/aviator/wiki) for details.


# Links

* downloads: <https://github.com/killme2008/aviator/releases>
* documents: <https://github.com/killme2008/aviator/wiki>
* javadoc: <http://fnil.net/aviator/apidocs/>
* author:  dennis (killme2008@gmail.com)
* spring boot: <https://github.com/mengxiangrui007/spring-boot-rule-jsr94>
