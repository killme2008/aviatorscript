[![Build Status](https://travis-ci.org/killme2008/aviator.svg?branch=master)](https://travis-ci.org/killme2008/aviator)
[![Code Quality: Java](https://img.shields.io/lgtm/grade/java/g/killme2008/aviator.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/killme2008/aviator/context:java)
[![Total Alerts](https://img.shields.io/lgtm/alerts/g/killme2008/aviator.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/killme2008/aviator/alerts)


```java
int[] a = ...;
Map<String, Object> env = new HashMap<String, Object>();
env.put("a", a);

AviatorEvaluator.execute("1 + 2 + 3");
AviatorEvaluator.execute("a[1] + 100", env);
AviatorEvaluator.execute("'a[1]=' + a[1]", env);
//求数组长度
AviatorEvaluator.execute("count(a)", env);
//求数组总和
AviatorEvaluator.execute("reduce(a, +, 0)", env);
//检测数组每个元素都在 0 <= e < 10 之间。
AviatorEvaluator.execute("seq.every(a, seq.and(seq.ge(0), seq.lt(10)))", env);
//Lambda 求和
AviatorEvaluator.execute("reduce(a, lambda(x,y) -> x + y end, 0)", env);
```

# Intro

Aviator is a lightweight,high performance expression evaluator for java.
Aviator compiles expression to byte code and evaluate it on the fly.

Aviator是一个轻量级、高性能的Java表达式执行引擎，它动态地将表达式编译成字节码并运行。更详细的请阅读[开发指南](https://github.com/killme2008/aviator/wiki)。

特性包括：

1. 支持绝大多数运算操作符，包括算术操作符、关系运算符、逻辑操作符、位运算符、正则匹配操作符(=~)、三元表达式(?:)
2. 支持操作符优先级和括号强制设定优先级。
3. 支持赋值。
4. 逻辑运算符支持短路运算。
5. 支持丰富类型，例如nil、整数和浮点数、字符串、正则表达式、日期、变量等，支持自动类型转换。
6. 支持 lambda 匿名函数和闭包。
7. 内置一套强大的常用函数库
8. 可自定义函数，易于扩展
9. 可重载操作符
10. 支持大数运算(BigInteger)和高精度运算(BigDecimal)。
11. 小巧并性能优秀

更多详情，请访问[主页](http://fnil.net/aviator)

# News

* [4.1.1](https://github.com/killme2008/aviator/releases/tag/aviator-4.1.1)，性能优化等。
* [4.1.0](https://github.com/killme2008/aviator/releases/tag/aviator-4.1.0)，支持赋值语句、修复 bug 等。
* [4.0.0-RC 发布](https://github.com/killme2008/aviator/releases/tag/aviator-4.0.0-RC)。支持 lambda 函数和闭包、多求值器实例创建和多行表达式等。
* [3.3.0 发布](https://github.com/killme2008/aviator/releases/tag/aviator-3.3.0)。支持运算符重载和跟踪执行过程等。


# Dependency

      <dependency>
          <groupId>com.googlecode.aviator</groupId>
          <artifactId>aviator</artifactId>
          <version>{version}</version>
      </dependency>

# Links

 * downloads: https://github.com/killme2008/aviator/releases
 * documents: https://github.com/killme2008/aviator/wiki
 * javadoc: http://fnil.net/aviator/apidocs/
 * author:  dennis (killme2008@gmail.com)
 * spring boot: https://github.com/mengxiangrui007/spring-boot-rule-jsr94
