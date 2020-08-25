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
3. 完整的脚本语法支持，包括多行数据、条件语句、循环语句、词法作用域和异常处理等。
4. [函数式编程](https://www.yuque.com/boyan-avfmj/aviatorscript/ksghfc)结合 [Sequence 抽象](https://www.yuque.com/boyan-avfmj/aviatorscript/yc4l93)，便捷处理任何集合。
5. 轻量化的[模块系统](https://www.yuque.com/boyan-avfmj/aviatorscript/rqra81)。
6. 多种方式，方便地[调用 Java 方法](https://www.yuque.com/boyan-avfmj/aviatorscript/xbdgg2)，完整支持 Java [脚本 API](https://www.yuque.com/boyan-avfmj/aviatorscript/bds23b)（方便从 Java 调用脚本）。
7. 丰富的定制选项，可作为安全的语言沙箱和全功能语言使用。
8. 轻量化，高性能，通过直接将脚本翻译成 JVM 字节码，AviatorScript 的基础性能较好。

使用场景包括：
1. 规则判断及规则引擎
2. 公式计算
3. 动态脚本控制
4. 集合数据 ELT 等
……

# News

* [5.1.4](https://github.com/killme2008/aviator/releases/tag/aviator-5.1.4)，修复编译字符串插值可能存在的重复编译问题（导致 FGC）等。
* [5.1.3](https://github.com/killme2008/aviator/releases/tag/aviator-5.1.3)，支持幂运算符 `**`，以及 `EnvProcessor` 处理器等。
* [5.1.2](https://github.com/killme2008/aviator/releases/tag/aviator-5.1.2), 移除内部变量 `__exp__` 对用户 env 的污染
* [5.1.1](https://github.com/killme2008/aviator/releases/tag/aviator-5.1.1), 修复 == 和 != 运算符无法和 `a.b.c` 语法糖一起可能返回错误结果的 bug，推荐升级

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

1. 下载 [aviator](https://raw.githubusercontent.com/killme2008/aviator/master/bin/aviator) shell 到某个目录（最好是在系统的 `PATH` 环境变量内），比如 `~/bin/aviator`:

```sh
$ wget https://raw.githubusercontent.com/killme2008/aviator/master/bin/aviator
$ chmod u+x aviator
```

2. 执行  `aviator`  命令，将自动下载最新文档版本 aviator jar 到  `~/.aviatorscript`  下的安装目录并运行：

```sh
$ aviator
Downloading AviatorScript now...
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   153  100   153    0     0    111      0  0:00:01  0:00:01 --:--:--   111
100 1373k  100 1373k    0     0   689k      0  0:00:01  0:00:01 --:--:--  689k
Usage: java com.googlecode.aviator.Main [file] [args]
     : java com.googlecode.aviator.Main -e [script]
     : java com.googlecode.aviator.Main -v
```

3. 将下面这个脚本保存为文件  `hello.av`:

```js
p("Hello, AviatorScript!");

let a = tuple(1, 2, 3, 4, 5);

p("sum of a is: " + reduce(a, +, 0));

let date = new java.util.Date();
p("The year is: "+ getYear(date));
p("The month is: "+ getMonth(date));
```

4. 执行脚本：

```sh
$ aviator hello.av
Hello, AviatorScript!
sum of a is: 15
The year is: 120
The month is: 3
```


更详细的请阅读[用户指南](https://www.yuque.com/boyan-avfmj/aviatorscript/cpow90)。

# Links

* downloads: <https://github.com/killme2008/aviator/releases>
* documents: <https://www.yuque.com/boyan-avfmj/aviatorscript>
* javadoc: <http://fnil.net/aviator/apidocs/>
* author:  dennis (killme2008@gmail.com)
* spring boot: <https://github.com/mengxiangrui007/spring-boot-rule-jsr94>
