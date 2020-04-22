# Aviator

[![Build Status](https://travis-ci.org/killme2008/aviator.svg?branch=master)](https://travis-ci.org/killme2008/aviator)
[![Code Quality: Java](https://img.shields.io/lgtm/grade/java/g/killme2008/aviator.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/killme2008/aviator/context:java)
[![Maven Central](https://img.shields.io/maven-central/v/com.googlecode.aviator/aviator.svg?label=maven%20central)](https://search.maven.org/search?q=g:com.googlecode.aviator%20AND%20aviator)

📖 English Documentation | [📖 中文文档](README.md)

----------------------------------------

`Aviator` is a lightweight, high performance scripting language hosted on the JVM.
It compiles script to java byte code and evaluate it on the fly.

# Feature Intro

1. Suppport number,string, boolean and regular expression etc. [basic types](https://www.yuque.com/boyan-avfmj/aviatorscript/lvabnw)，support all java operators and their priorities.
2. [Function](https://www.yuque.com/boyan-avfmj/aviatorscript/gl2p0q) is first-class, supports closure and functional programming.
2. Supports bigint/decmal for big integer and big decimal operations, using normal arithmetic operators `+-*/` by [operator overloading](https://www.yuque.com/boyan-avfmj/aviatorscript/ydllav#5hq4k).
3. Full-featured scripting language syntax, such as multi statements, conditional statement ,`for/while` loop, lexical scope and exception handling.
4. Processing collections/array conveniently by [sequence abstract](https://www.yuque.com/boyan-avfmj/aviatorscript/yc4l93) and [functional programming](https://www.yuque.com/boyan-avfmj/aviatorscript/ksghfc).
5. Lightweight [module system](https://www.yuque.com/boyan-avfmj/aviatorscript/rqra81)。
6. [Call Java methods](https://www.yuque.com/boyan-avfmj/aviatorscript/xbdgg2) conveniently，supports Java [Scripting API](https://www.yuque.com/boyan-avfmj/aviatorscript/bds23b)。
7. A wide range of customization options to be used as a secure runtime scripting sandbox or full-featured scripting language.
8. Lightweight and high performance.

# News

* [5.0.0-RC2](https://github.com/killme2008/aviator/releases/tag/aviator-5.0.0-RC2), try/catch/finally/throw statement and new stament etc.
* [5.0.0-RC1](https://github.com/killme2008/aviator/releases/tag/aviator-5.0.0-RC1), almost production-ready version before releasing 5.0

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

See [user guide](https://www.yuque.com/boyan-avfmj/aviatorscript) for details.


# Links

* downloads: <https://github.com/killme2008/aviator/releases>
* documents: <https://www.yuque.com/boyan-avfmj/aviatorscript>
* javadoc: <http://fnil.net/aviator/apidocs/>
* author:  dennis (killme2008@gmail.com)
* spring boot: <https://github.com/mengxiangrui007/spring-boot-rule-jsr94>
