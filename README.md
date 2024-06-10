# AviatorScript

![Build Status](https://github.com/killme2008/aviatorscript/actions/workflows/maven.yml/badge.svg)
[![Maven Central](https://img.shields.io/maven-central/v/com.googlecode.aviator/aviator.svg?label=maven%20central)](https://search.maven.org/search?q=g:com.googlecode.aviator%20AND%20aviator)

[📖 English Documentation](README-EN.md) | 📖 中文文档

----------------------------------------

`AviatorScript` 是一门高性能、轻量级寄宿于 JVM （包括 Android 平台）之上的脚本语言。

**It's not a game, it's a programming language. Please refrain from sending me any more unsolicited emails.RTFM**

# 特性介绍

1. 支持数字、字符串、正则表达式、布尔值、正则表达式等[基本类型](https://www.yuque.com/boyan-avfmj/aviatorscript/lvabnw)，完整支持所有 Java 运算符及优先级等。
2. [函数](https://www.yuque.com/boyan-avfmj/aviatorscript/gl2p0q)是一等公民，支持[闭包和函数式编程](https://www.yuque.com/boyan-avfmj/aviatorscript/ksghfc)。
3. 内置 [bigint](https://www.yuque.com/boyan-avfmj/aviatorscript/lvabnw#a0Ifn)/[decimal](https://www.yuque.com/boyan-avfmj/aviatorscript/lvabnw#QbV7z) 类型用于大整数和高精度运算，支持[运算符重载](https://www.yuque.com/boyan-avfmj/aviatorscript/ydllav#5hq4k)得以让这些类型使用普通的算术运算符 `+-*/ `参与运算。
4. 完整的脚本语法支持，包括多行数据、条件语句、循环语句、词法作用域和异常处理等。
5. [函数式编程](https://www.yuque.com/boyan-avfmj/aviatorscript/ksghfc)结合 [Sequence 抽象](https://www.yuque.com/boyan-avfmj/aviatorscript/yc4l93)，便捷处理任何集合。
6. 轻量化的[模块系统](https://www.yuque.com/boyan-avfmj/aviatorscript/rqra81)。
7. 多种方式，方便地[调用 Java 方法](https://www.yuque.com/boyan-avfmj/aviatorscript/xbdgg2)，完整支持 Java [脚本 API](https://www.yuque.com/boyan-avfmj/aviatorscript/bds23b)（方便从 Java 调用脚本）。
8. 丰富的[定制选项](https://www.yuque.com/boyan-avfmj/aviatorscript/yr1oau)，可作为安全的语言沙箱和全功能语言使用。
9. 动态编译和执行、轻量化、高性能，ASM 模式下通过直接将脚本编译成 JVM 字节码，[解释模式](https://www.yuque.com/boyan-avfmj/aviatorscript/ok8agx)可运行于 Android 等非标 Java 平台。
10. 支持[编译结果序列化](https://github.com/killme2008/aviatorscript/blob/master/src/test/java/com/googlecode/aviator/example/SerializeExample.java)，方便缓存加速等。支持[执行超时设置](https://github.com/killme2008/aviatorscript/blob/master/src/test/java/com/googlecode/aviator/example/TimeoutExample.java)，避免破坏性脚本耗尽资源。

使用场景包括：
1. 规则判断及规则引擎
2. 公式计算
3. 动态脚本控制
4. 集合数据 ELT 等
……

**推荐使用版本 5.2.6 及以上**

# News

* [5.4.3](https://github.com/killme2008/aviatorscript/releases/tag/aviator-5.4.3)，增加安全沙箱一键启用方法以及修复 bug 等。
* [5.4.2](https://github.com/killme2008/aviatorscript/releases/tag/aviator-5.4.2)，增加 `getFunctionNames` 方法用于获取函数列表以及设置求值超时时间等。
* [5.4.1](https://github.com/killme2008/aviatorscript/releases/tag/aviator-5.4.1)，修复递归函数无法工作的 bug，修复函数无法序列化的 bug 等。

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
p("The month is: #{getMonth(date)}");
```

一个更复杂的计算器例子（求值算术表达式字符串）参见[calculator.av](https://github.com/killme2008/aviatorscript/blob/master/examples/calculator.av)。

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

* Releases: <https://github.com/killme2008/aviator/releases>
* Documents: <https://www.yuque.com/boyan-avfmj/aviatorscript>
* Changelog: <https://www.yuque.com/boyan-avfmj/aviatorscript/bggwx2>
* Javadoc: <http://fnil.net/aviator/apidocs/>
* Spring boot rule: <https://github.com/mengxiangrui007/spring-boot-rule-jsr94>
* Idea plugin: <https://github.com/yanchangyou/aviatorscript-ideaplugin>
