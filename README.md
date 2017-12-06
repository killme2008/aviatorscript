[![Build Status](https://travis-ci.org/killme2008/aviator.svg?branch=master)](https://travis-ci.org/killme2008/aviator)


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
```

# Intro

Aviator is a lighweith,high performance expression evaluator for java.
Aviator compiles expresson to byte code and evaluate it on the fly.

Aviator是一个轻量级、高性能的Java表达式执行引擎，它动态地将表达式编译成字节码并运行。更详细的请阅读[开发指南](https://github.com/killme2008/aviator/wiki)。

更多详情，请访问[主页](http://fnil.net/aviator)

# News

* [3.2.0 发布](https://github.com/killme2008/aviator/releases/tag/aviator-3.2.0)。改善编译性能，大多数涉及动态变量的表达式的编译性能都有 50%-60% 的提升；**不再兼容 JDK6 及以下版本**。
* [3.1.1 发布](https://github.com/killme2008/aviator/releases/tag/aviator-3.1.1)。修复 AviatorString 比较运算符存在的 NPE 异常。
* [3.1.0 发布](https://github.com/killme2008/aviator/releases/tag/aviator-3.1.0)。更丰富的逻辑组合函数，深度嵌套访问的引用变量支持。


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
