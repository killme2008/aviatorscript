# Aviator

[![Build Status](https://travis-ci.org/killme2008/aviator.svg?branch=master)](https://travis-ci.org/killme2008/aviator)
[![Code Quality: Java](https://img.shields.io/lgtm/grade/java/g/killme2008/aviator.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/killme2008/aviator/context:java)
[![Maven Central](https://img.shields.io/maven-central/v/com.googlecode.aviator/aviator.svg?label=maven%20central)](https://search.maven.org/search?q=g:com.googlecode.aviator%20AND%20aviator)

[📖 English Documentation](README-EN.md) | 📖 中文文档

----------------------------------------

`Aviator`是一个轻量级、高性能的`Java`表达式执行引擎，它动态地将表达式编译成字节码并运行。


# 特性介绍

1. 支持绝大多数运算操作符，包括算术操作符、关系运算符、逻辑操作符、位运算符、正则匹配操作符（`=~`）、三元表达式（`?:`）。
2. 支持操作符优先级和括号强制设定优先级。
3. 支持[赋值](https://github.com/killme2008/aviator/wiki/4.0-%E5%8A%9F%E8%83%BD%E8%AF%A6%E7%BB%86%E8%A7%A3%E6%9E%90#%E8%B5%8B%E5%80%BC)。
4. 逻辑运算符支持短路运算。
5. 支持丰富类型，例如`nil`、整数和浮点数、字符串、正则表达式、日期、变量等，支持自动类型转换和提升。
6. [支持`lambda`匿名函数和闭包](https://github.com/killme2008/aviator/wiki/4.0-%E5%8A%9F%E8%83%BD%E8%AF%A6%E7%BB%86%E8%A7%A3%E6%9E%90#lambda-%E5%8C%BF%E5%90%8D%E5%87%BD%E6%95%B0)。
7. 内置一套强大的常用[函数库](https://github.com/killme2008/aviator/wiki/%E5%86%85%E7%BD%AE%E5%87%BD%E6%95%B0)。
8. 可[自定义函数](https://github.com/killme2008/aviator/wiki#%E8%87%AA%E5%AE%9A%E4%B9%89%E5%87%BD%E6%95%B0)，易于扩展，支持函数调用点[参数列表捕获](https://github.com/killme2008/aviator/wiki/%E5%AE%8C%E6%95%B4%E9%80%89%E9%A1%B9%E5%88%97%E8%A1%A8%E8%AF%B4%E6%98%8E#capture_function_args)和 [Java 方法批量导入](https://github.com/killme2008/aviator/wiki#%E4%BD%BF%E7%94%A8Java%E7%B1%BB%E6%96%B9%E6%B3%95%E4%BD%9C%E4%B8%BA%E8%87%AA%E5%AE%9A%E4%B9%89%E5%87%BD%E6%95%B0)。
9. 可[重载运算符](https://github.com/killme2008/aviator/wiki#%E9%87%8D%E8%BD%BD%E8%BF%90%E7%AE%97%E7%AC%A6)。
10. 支持[大数运算（`BigInteger`）和高精度运算（`BigDecimal`）](https://github.com/killme2008/aviator/wiki#%E5%A4%A7%E6%95%B0%E8%AE%A1%E7%AE%97%E5%92%8C%E7%B2%BE%E5%BA%A6)。
11. 支持[多行表达式以及定制化求值器实例](https://github.com/killme2008/aviator/wiki/4.0-%E5%8A%9F%E8%83%BD%E8%AF%A6%E7%BB%86%E8%A7%A3%E6%9E%90)。
12. 小巧并性能优秀，提供大量的定制[选项](https://github.com/killme2008/aviator/wiki/%E5%AE%8C%E6%95%B4%E9%80%89%E9%A1%B9%E5%88%97%E8%A1%A8%E8%AF%B4%E6%98%8E)。
13. [Function missing 机制](https://github.com/killme2008/aviator/wiki/%E8%B0%83%E7%94%A8-Java-%E6%96%B9%E6%B3%95%E5%92%8C-Function-Missing)，类似 Ruby 的 method missing。

更多详情，请访问[主页](http://fnil.net/aviator)。

# News

* [4.2.6](https://github.com/killme2008/aviator/releases/tag/aviator-4.2.6),  Fixed NPE when comparing java objects, #175
* [4.2.5](https://github.com/killme2008/aviator/releases/tag/aviator-4.2.5),  Function missing 机制，可通过反射调用任意 java public 实例方法，无需导入。
* [4.2.4](https://github.com/killme2008/aviator/releases/tag/aviator-4.2.4),  adds annotations for importing java class methods and some new features.


# Dependency

```xml
<dependency>
  <groupId>com.googlecode.aviator</groupId>
  <artifactId>aviator</artifactId>
  <version>{version}</version>
</dependency>
```

可以在 [search.maven.org](https://search.maven.org/search?q=g:com.googlecode.aviator%20AND%20a:aviator&core=gav) 查看可用的版本。

# 快速开始

```java
int[] a = ...;
Map<String, Object> env = new HashMap<String, Object>();
env.put("a", a);

AviatorEvaluator.execute("1 + 2 + 3");
AviatorEvaluator.execute("a[1] + 100", env);
AviatorEvaluator.execute("'a[1]=' + a[1]", env);

// 求数组长度
AviatorEvaluator.execute("count(a)", env);

// 求数组总和
AviatorEvaluator.execute("reduce(a, +, 0)", env);

// 检测数组每个元素都在 0 <= e < 10 之间
AviatorEvaluator.execute("seq.every(a, seq.and(seq.ge(0), seq.lt(10)))", env);

// Lambda 求和
AviatorEvaluator.execute("reduce(a, lambda(x,y) -> x + y end, 0)", env);

// 导入 String 类实例方法作为自定义函数
AviatorEvaluator.addInstanceFunctions("s", String.class);
AviatorEvaluator.execute("s.indexOf('hello', 'l')");
AviatorEvaluator.execute("s.replaceAll('hello', 'l', 'x')");

// 导入静态方法作为自定义函数
AviatorEvaluator.addStaticFunctions("sutil", StringUtils.class);
AviatorEvaluator.execute("sutil.isBlank('hello')");

// 启用基于反射的 function missing 机制，调用任意 public 实例方法，无需导入
AviatorEvaluator.setFunctionMissing(JavaMethodReflectionFunctionMissing.getInstance());
// 调用 String#indexOf
System.out.println(AviatorEvaluator.execute("indexOf('hello world', 'w')"));
// 调用 Long#floatValue
System.out.println(AviatorEvaluator.execute("floatValue(3)"));
// 调用 BigDecimal#add
System.out.println(AviatorEvaluator.execute("add(3M, 4M)"));
```

更详细的请阅读[用户指南](https://github.com/killme2008/aviator/wiki)。

# Links

* downloads: <https://github.com/killme2008/aviator/releases>
* documents: <https://github.com/killme2008/aviator/wiki>
* javadoc: <http://fnil.net/aviator/apidocs/>
* author:  dennis (killme2008@gmail.com)
* spring boot: <https://github.com/mengxiangrui007/spring-boot-rule-jsr94>
