# AviatorScript

[![Build Status](https://travis-ci.org/killme2008/aviatorscript.svg?branch=master)](https://travis-ci.org/killme2008/aviatorscript)
[![Code Quality: Java](https://img.shields.io/lgtm/grade/java/g/killme2008/aviator.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/killme2008/aviator/context:java)
[![Maven Central](https://img.shields.io/maven-central/v/com.googlecode.aviator/aviator.svg?label=maven%20central)](https://search.maven.org/search?q=g:com.googlecode.aviator%20AND%20aviator)

📖 English Documentation | [📖 中文文档](README.md)

----------------------------------------

`AviatorScript` is a lightweight, high performance scripting language hosted on the JVM (and Android platform).
It compiles script to java byte code and evaluate it on the fly.

# Feature Intro

1. Suppport number,string, boolean and regular expression etc. [basic types](https://www.yuque.com/boyan-avfmj/aviatorscript/lvabnw?translate=en)，support all java operators and their priorities.
2. [Function](https://www.yuque.com/boyan-avfmj/aviatorscript/gl2p0q?translate=en) is first-class, supports closure and functional programming.
3. Supports bigint/decmal for big integer and big decimal operations, using normal arithmetic operators `+-*/` by [operator overloading](https://www.yuque.com/boyan-avfmj/aviatorscript/ydllav?translate=en#5hq4k).
4. Full-featured scripting language syntax, such as multi statements, conditional statement ,`for/while` loop, lexical scope and exception handling.
5. Processing collections/array conveniently by [sequence abstract](https://www.yuque.com/boyan-avfmj/aviatorscript/yc4l93?translate=en) and [functional programming](https://www.yuque.com/boyan-avfmj/aviatorscript/ksghfc?translate=en).
6. Lightweight [module system](https://www.yuque.com/boyan-avfmj/aviatorscript/rqra81?translate=en)。
7. [Call Java methods](https://www.yuque.com/boyan-avfmj/aviatorscript/xbdgg2?translate=en) conveniently，supports Java [Scripting API](https://www.yuque.com/boyan-avfmj/aviatorscript/bds23b?translate=en)。
8. A wide range of customization options to be used as a secure runtime scripting sandbox or full-featured scripting language.
9. Lightweight and high performance. ASM mode compile script into JVM bytecode on fly ,and [interpreter mode](https://www.yuque.com/boyan-avfmj/aviatorscript/ok8agx?translate=en) make it run on Android platform etc.
10. Supports compiled expression serialization, easy to cache etc.


**Recommend version 5.2.6 and above.**

# News

* [5.4.0](https://github.com/killme2008/aviatorscript/releases/tag/aviator-5.4.0)，Fixed `elsif` parser errors，supports expression serialization([example](https://github.com/killme2008/aviatorscript/blob/master/src/test/java/com/googlecode/aviator/example/SerializeExample.java)) etc.
* [5.3.3](https://github.com/killme2008/aviatorscript/releases/tag/aviator-5.3.3)，fixed potential memory leak, wrong value captured by function etc.
* [5.3.2](https://github.com/killme2008/aviatorscript/releases/tag/aviator-5.3.2)，some bugs fixed, recommended to upgrade.

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
