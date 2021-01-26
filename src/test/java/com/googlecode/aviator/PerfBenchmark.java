package com.googlecode.aviator;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import org.beetl.core.Configuration;
import org.beetl.core.GroupTemplate;
import org.beetl.core.resource.StringTemplateResourceLoader;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@BenchmarkMode(Mode.Throughput)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class PerfBenchmark {

  static class Data {
    private String ikey;
    private Integer ivalue;

    public Data(final String ikey, final int ivalue) {
      setIkey(ikey);
      this.ivalue = ivalue;
    }

    public Integer getIvalue() {
      return this.ivalue;
    }

    public void setIvalue(final Integer ivalue) {
      this.ivalue = ivalue;
    }

    public String getIkey() {
      return this.ikey;
    }

    public void setIkey(final String ikey) {
      this.ikey = ikey;
    }
  }


  private final Map<String, Object> paras = new HashMap<>();

  private GroupTemplate gt;

  private Invocable arithInv;
  private Invocable objectInv;
  private Invocable condInv;

  private Expression arithExp;
  private Expression objectExp;
  private Expression condExp;

  @Setup
  public void init() {
    this.paras.put("A", new Data("false", 23342423));
    this.paras.put("B", new Data("false", 435454));
    this.paras.put("C", new Data("true", 121243));
    this.paras.put("D", new Data("false", 23));

    initScript();
    initBeetl();
    initAviator();
  }

  @Benchmark
  public void testArith() throws Exception {
    Object result =
        (((Data) this.paras.get("A")).getIvalue() + ((Data) this.paras.get("B")).getIvalue()
            - ((Data) this.paras.get("C")).getIvalue()) * ((Data) this.paras.get("D")).getIvalue();
  }

  @Benchmark
  public void testObject() throws Exception {
    Map<String, Object> result = new HashMap<>(4);
    result.put("f1", ((Data) this.paras.get("A")).getIvalue());
    result.put("f2",
        ((Data) this.paras.get("A")).getIvalue() + ((Data) this.paras.get("B")).getIvalue());
    result.put("f3", ((Data) this.paras.get("C")).getIvalue());
    result
        .put("f4",
            (((Data) this.paras.get("A")).getIvalue() + ((Data) this.paras.get("B")).getIvalue()
                - ((Data) this.paras.get("C")).getIvalue())
                * ((Data) this.paras.get("D")).getIvalue());
  }

  @Benchmark
  public void testCond() throws Exception {
    Object result = 0;
    if (((Data) this.paras.get("A")).getIkey().equals("true")) {
      result = ((Data) this.paras.get("A")).getIvalue();
    } else if (((Data) this.paras.get("B")).getIkey().equals("true")) {
      result = ((Data) this.paras.get("B")).getIvalue();
    } else if (((Data) this.paras.get("C")).getIkey().equals("true")) {
      result = ((Data) this.paras.get("C")).getIvalue();
    } else if (((Data) this.paras.get("D")).getIkey().equals("true")) {
      result = ((Data) this.paras.get("D")).getIvalue();
    }
  }

  @Benchmark
  public void testArithByScript() throws Exception {
    Object result = this.arithInv.invokeFunction("testArith", this.paras);
  }

  @Benchmark
  public void testObjectByScript() throws Exception {
    Object result = this.objectInv.invokeFunction("testObject", this.paras);
  }

  @Benchmark
  public void testCondByScript() throws Exception {
    Object result = this.condInv.invokeFunction("testCond", this.paras);
  }

  @Benchmark
  public void testArithByBeetl() {
    Map result = this.gt.runScript("return (A.ivalue+B.ivalue-C.ivalue)*D.ivalue;", this.paras);
  }

  @Benchmark
  public void testObjectByBeetl() {
    Map result = this.gt.runScript(
        "var object = {f1: A.ivalue, f2: A.ivalue+B.ivalue, f3: C.ivalue, f4: (A.ivalue+B.ivalue-C.ivalue)*D.ivalue}; ",
        this.paras);
  }

  @Benchmark
  public void testCondByBeetl() {
    Map result = this.gt.runScript(
        "if(A.ikey=='true'){return A.ivalue;}else if(B.ikey=='true'){return B.ivalue;}else if(C.ikey=='true'){return C.ivalue;}else if(D.ikey=='true'){return D.ivalue;}else{return 0;}",
        this.paras);
  }

  @Benchmark
  public void testArithByAviator() {
    Object result = this.arithExp.execute(this.paras);
  }

  @Benchmark
  public void testObjectByAviator() {
    Object result = this.objectExp.execute(this.paras);
  }

  @Benchmark
  public void testCondByAviator() {
    Object result = this.condExp.execute(this.paras);
  }

  private void initScript() {
    ScriptEngineManager manager = new ScriptEngineManager();
    try {
      ScriptEngine engine = manager.getEngineByName("js");
      engine.eval(
          "function testArith(paras){return (paras.A.ivalue+paras.B.ivalue-paras.C.ivalue)*paras.D.ivalue;}");
      this.arithInv = (Invocable) engine;

      engine = manager.getEngineByName("js");
      engine.eval(
          "function testObject(paras){var object={f1: paras.A.ivalue, f2: paras.A.ivalue+paras.B.ivalue, f3: paras.C.ivalue, f4: (paras.A.ivalue+paras.B.ivalue-paras.C.ivalue)*paras.D.ivalue}; return object;}");
      this.objectInv = (Invocable) engine;

      engine = manager.getEngineByName("js");
      engine.eval(
          "function testCond(paras){if(paras.A.ikey=='true'){return paras.A.ivalue;}else if(paras.B.ikey=='true'){return paras.B.ivalue;}else if(paras.C.ikey=='true'){return paras.C.ivalue;}else if(paras.D.ikey=='true'){return paras.D.ivalue;}else{return 0;}}");
      this.condInv = (Invocable) engine;
    } catch (ScriptException e) {
      e.printStackTrace();
    }
    System.out.println("JS准备工作就绪！");
  }

  private void initBeetl() {
    try {
      Configuration cfg = Configuration.defaultConfiguration();
      this.gt = new GroupTemplate(new StringTemplateResourceLoader(), cfg);
    } catch (IOException e) {
      throw new RuntimeException("初始化Beetl资源加载器失败", e);
    }
    System.out.println("Beetl准备工作就绪！");
  }

  private void initAviator() {
    this.arithExp = AviatorEvaluator.getInstance().compile("(A.ivalue+B.ivalue-C.ivalue)*D.ivalue");
    this.objectExp = AviatorEvaluator.getInstance().compile(
        "let object=seq.map('f1', A.ivalue, 'f2', A.ivalue+B.ivalue, 'f3', C.ivalue, 'f4', (A.ivalue+B.ivalue-C.ivalue)*D.ivalue); return object;");
    this.condExp = AviatorEvaluator.getInstance().compile(
        "if(A.ikey=='true'){return A.ivalue;}elsif(B.ikey=='true'){return B.ivalue;}elsif(C.ikey=='true'){return C.ivalue;}elsif(D.ikey=='true'){return D.ivalue;}else{return 0;}");
    System.out.println("Aviator准备工作就绪！");
  }

  public static void main(final String[] args) throws Exception {
    Options opt = new OptionsBuilder().include(PerfBenchmark.class.getSimpleName()).build();
    new Runner(opt).run();
  }

}
