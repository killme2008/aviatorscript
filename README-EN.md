# AviatorScript

![Build Status](https://github.com/killme2008/aviatorscript/actions/workflows/maven.yml/badge.svg)
[![Maven Central](https://img.shields.io/maven-central/v/com.googlecode.aviator/aviator.svg?label=maven%20central)](https://search.maven.org/search?q=g:com.googlecode.aviator%20AND%20aviator)

📖 English Documentation | [📖 中文文档](README.md)

----------------------------------------

`AviatorScript` is a lightweight, high performance scripting language hosted on the JVM (and Android platform).
It compiles script to java byte code and evaluate it on the fly.

**It's not a game, it's a programming language. Please refrain from sending me any more unsolicited emails.RTFM**

# Feature Intro

1. Supports basic types such as numbers, strings, regular expressions, booleans, and more. Full support for all Java operators and precedence, etc. [Basic Types](https://www.yuque.com/boyan-avfmj/aviatorscript/lvabnw).
2. [Functions](https://www.yuque.com/boyan-avfmj/aviatorscript/gl2p0q) are first-class, supporting [closures and functional programming](https://www.yuque.com/boyan-avfmj/aviatorscript/ksghfc).
3. Built-in [bigint](https://www.yuque.com/boyan-avfmj/aviatorscript/lvabnw#a0Ifn)/[decimal](https://www.yuque.com/boyan-avfmj/aviatorscript/lvabnw#QbV7z) types for large integers and high-precision calculations. Supports [operator overloading](https://www.yuque.com/boyan-avfmj/aviatorscript/ydllav#5hq4k) to allow these types to use common arithmetic operators `+-*/`.
4. Full script syntax support, including multiline data, conditional statements, loop statements, lexical scoping, and exception handling.
5. [Functional programming](https://www.yuque.com/boyan-avfmj/aviatorscript/ksghfc) combined with [Sequence abstraction](https://www.yuque.com/boyan-avfmj/aviatorscript/yc4l93) for convenient collection handling.
6. Lightweight [module system](https://www.yuque.com/boyan-avfmj/aviatorscript/rqra81).
7. Multiple ways to easily [call Java methods](https://www.yuque.com/boyan-avfmj/aviatorscript/xbdgg2), with full support for the Java [script API](https://www.yuque.com/boyan-avfmj/aviatorscript/bds23b) (facilitating script calls from Java).
8. Rich [customization options](https://www.yuque.com/boyan-avfmj/aviatorscript/yr1oau), usable as a secure language sandbox and a fully featured language.
9. Dynamic compilation and execution, lightweight, and high performance. In ASM mode, it directly compiles scripts into JVM bytecode. [Interpretation mode](https://www.yuque.com/boyan-avfmj/aviatorscript/ok8agx) can run on non-standard Java platforms like Android.
10. Supports [serialization of compiled results](https://github.com/killme2008/aviatorscript/blob/master/src/test/java/com/googlecode/aviator/example/SerializeExample.java), facilitating caching and compiling acceleration. Supports [execution timeout settings](https://github.com/killme2008/aviatorscript/blob/master/src/test/java/com/googlecode/aviator/example/TimeoutExample.java) to prevent resource exhaustion by disruptive scripts.


**Recommend version 5.2.6 and above.**

# News

* [5.4.3](https://github.com/killme2008/aviatorscript/releases/tag/aviator-5.4.3)，add the `enableSandboxMode` to enable sandbox mode etc.
* [5.4.2](https://github.com/killme2008/aviatorscript/releases/tag/aviator-5.4.2)，add the `getFunctionNames` method to retrieve a list of functions and set the evaluation timeout, etc.
* [5.4.1](https://github.com/killme2008/aviatorscript/releases/tag/aviator-5.4.1)，Fixed recursive function can't work, fixed function can't be serialized etc.

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

1. Download [aviator](https://raw.githubusercontent.com/killme2008/aviator/master/bin/aviator)  shell script to a directory in system `PATH` environment variable，such as  `~/bin/aviator`:

```sh
$ wget https://raw.githubusercontent.com/killme2008/aviator/master/bin/aviator
$ chmod u+x aviator
```

2. Execute  `aviator`   command，it will download the latest aviator jar to  `~/.aviatorscript`  directory：

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

3. Save the script below in file `hello.av`:

```js
p("Hello, AviatorScript!");

let a = tuple(1, 2, 3, 4, 5);

p("sum of a is: " + reduce(a, +, 0));

let date = new java.util.Date();
p("The year is: "+ getYear(date));
p("The month is: #{getMonth(date)}");
```

4. Execute the script with `aviator`  command：

```sh
$ aviator hello.av
Hello, AviatorScript!
sum of a is: 15
The year is: 120
The month is: 3
```

A complex example is [calculator.av](https://github.com/killme2008/aviatorscript/blob/master/examples/calculator.av) which evaluates arithmetic expression in string.

Read [user guide](https://www.yuque.com/boyan-avfmj/aviatorscript/cpow90?translate=en) for details.


# Links

* Releases: <https://github.com/killme2008/aviator/releases>
* Documents: <https://www.yuque.com/boyan-avfmj/aviatorscript>
* Changelog: <https://www.yuque.com/boyan-avfmj/aviatorscript/bggwx2>
* Javadoc: <http://fnil.net/aviator/apidocs/>
* Spring boot rule: <https://github.com/mengxiangrui007/spring-boot-rule-jsr94>
* Idea plugin: <https://github.com/yanchangyou/aviatorscript-ideaplugin>
