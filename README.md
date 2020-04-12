# AviatorScript

[![Build Status](https://travis-ci.org/killme2008/aviator.svg?branch=master)](https://travis-ci.org/killme2008/aviator)
[![Maven Central](https://img.shields.io/maven-central/v/com.googlecode.aviator/aviator.svg?label=maven%20central)](https://search.maven.org/search?q=g:com.googlecode.aviator%20AND%20aviator)

[📖 English Documentation](README-EN.md) | 📖 中文文档

----------------------------------------

`AviatorScript` 是一门高性能、轻量级寄宿于 JVM 之上的脚本语言。

# 特性介绍

1. 支持数字、字符串、正则表达式、布尔值、正则表达式等[基本类型](https://www.yuque.com/boyan-avfmj/aviatorscript/lvabnw)，完整支持所有 Java 运算符及优先级等。
2. [函数](https://www.yuque.com/boyan-avfmj/aviatorscript/gl2p0q)是一等公民，支持[闭包和函数式编程](https://www.yuque.com/boyan-avfmj/aviatorscript/ksghfc)。
2. 内置 [bigint](https://www.yuque.com/boyan-avfmj/aviatorscript/lvabnw#a0Ifn)/[decmal](https://www.yuque.com/boyan-avfmj/aviatorscript/lvabnw#QbV7z) 类型用于大整数和高精度运算，支持[运算符重载](https://www.yuque.com/boyan-avfmj/aviatorscript/ydllav#5hq4k)得以让这些类型使用普通的算术运算符 `+-*/ `参与运算。
3. 完整的脚本语法支持，包括多行数据、条件语句、循环语句和词法作用域等。
4. [函数式编程](https://www.yuque.com/boyan-avfmj/aviatorscript/ksghfc)结合 [Sequence 抽象](https://www.yuque.com/boyan-avfmj/aviatorscript/yc4l93)，便捷处理任何集合。
5. 轻量化的[模块系统](https://www.yuque.com/boyan-avfmj/aviatorscript/rqra81)。
6. 多种方式，方便地[调用 Java 方法](https://www.yuque.com/boyan-avfmj/aviatorscript/xbdgg2)，完整支持 Java [脚本 API](https://www.yuque.com/boyan-avfmj/aviatorscript/bds23b)。
7. 丰富的定制选项，可作为安全的语言沙箱和全功能语言使用。
8. 轻量化，高性能，通过直接将脚本翻译成 JVM 字节码，AviatorScript 的基础性能较好。

# News


* [5.0.0-RC1](https://github.com/killme2008/aviator/releases/tag/aviator-5.0.0-RC1), almost production-ready version before releasing 5.0

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

更详细的请阅读[用户指南](https://www.yuque.com/boyan-avfmj/aviatorscript)。

# Links

* downloads: <https://github.com/killme2008/aviator/releases>
* documents: <https://www.yuque.com/boyan-avfmj/aviatorscript>
* javadoc: <http://fnil.net/aviator/apidocs/>
* author:  dennis (killme2008@gmail.com)
* spring boot: <https://github.com/mengxiangrui007/spring-boot-rule-jsr94>
