package com.googlecode.aviator;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;
import com.googlecode.aviator.code.interpreter.ir.LoadIR;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.parser.VariableMeta;
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.internal.ReducerResult;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Constants;
import com.googlecode.aviator.utils.Env;

public class InterpretExpression extends BaseExpression {

  private static final long serialVersionUID = -3831400781523526582L;

  private List<IR> instruments;

  private boolean unboxObject;

  private Map<VariableMeta, AviatorJavaType> variables =
      new IdentityHashMap<VariableMeta, AviatorJavaType>();

  private Map<Token<?>, AviatorObject> constantPool = new IdentityHashMap<>();


  public InterpretExpression(final AviatorEvaluatorInstance instance, final List<VariableMeta> vars,
      final Set<Token<?>> constants, final SymbolTable symbolTable, final List<IR> instruments,
      final boolean unboxObject) {
    super(instance, vars, symbolTable);
    this.instruments = instruments;
    this.unboxObject = unboxObject;
    loadVars(vars);
    loadConstants(constants, instruments);
  }

  private void loadVars(final List<VariableMeta> vars) {
    for (VariableMeta v : vars) {
      this.variables.put(v, new AviatorJavaType(v.getName(), this.symbolTable));
    }
  }

  private void loadConstants(final Set<Token<?>> constants, final List<IR> instruments) {
    final Env env = new Env();
    env.setInstance(this.instance);
    InterpretContext ctx = new InterpretContext(this, instruments, env);
    for (Token<?> token : constants) {
      final LoadIR loadConstantIR = new LoadIR(this.sourceFile, token, null, false);
      loadConstantIR.evalWithoutDispatch(ctx);
      this.constantPool.put(token, ctx.pop());
    }
  }

  public AviatorJavaType loadVar(final VariableMeta v) {
    return this.variables.get(v);
  }

  public AviatorObject loadConstant(final Token<?> token) {
    return this.constantPool.get(token);
  }

  public void printInstruments() {
    traceInstruments(Collections.<String, Object>emptyMap(), null, true);
  }

  private void traceInstruments(final Map<String, Object> env, final String name,
      final boolean traceLambda) {
    int pc = 0;
    RuntimeUtils.printlnTrace(env, (name == null ? this.sourceFile : name) + " instruments: ");
    for (IR ir : this.instruments) {
      RuntimeUtils.printlnTrace(env, "    " + (pc++) + " " + ir.toString());
    }
    RuntimeUtils.printlnTrace(env, "    " + pc + " return");

    if (this.lambdaBootstraps != null) {
      final List<LambdaFunctionBootstrap> bootstraps =
          new ArrayList<>(this.lambdaBootstraps.values());
      Collections.sort(bootstraps);
      for (LambdaFunctionBootstrap bootstrap : bootstraps) {
        final Expression exp = bootstrap.getExpression();
        if (exp instanceof InterpretExpression) {
          InterpretExpression iexp = (InterpretExpression) exp;
          iexp.traceInstruments(env, bootstrap.getName(), traceLambda);
        } else {
          RuntimeUtils.printlnTrace(env, bootstrap.getName() + " instruments: " + exp);
        }
      }
    }
  }

  @Override
  public Object executeDirectly(final Map<String, Object> env) {
    final boolean trace = RuntimeUtils.isTracedEval(env);
    if (trace) {
      traceInstruments(env, null, false);
      RuntimeUtils.printlnTrace(env, "Execute instruments: ");
    }

    InterpretContext ctx = new InterpretContext(this, this.instruments, (Env) env);
    ctx.dispatch(false);

    // while ((ir = ctx.getPc()) != null) {
    // if (trace) {
    // RuntimeUtils.printlnTrace(env, " " + ir + " " + ctx.descOperandsStack());
    // }
    // ir.eval(ctx);
    // if (ir instanceof JumpIR) {
    // if (ir != ctx.getPc()) {
    // // jump successfully, we don't move pc to next.
    // continue;
    // }
    // }
    // if (!ctx.next()) {
    // break;
    // }
    // }
    if (trace) {
      RuntimeUtils.printlnTrace(env, "    return    " + ctx.descOperandsStack());
    }


    assert (ctx.getOperands().size() <= 1);
    AviatorObject result = ctx.peek();
    if (result == null) {
      return null;
    }

    if (this.unboxObject) {
      return result.getValue(env);
    } else {
      return result.deref(env);
    }
  }


  @SuppressWarnings("unchecked")
  private void readObject(ObjectInputStream input) throws ClassNotFoundException, IOException {
    super.customReadObject(input);
    this.instruments = (List<IR>) input.readObject();
    this.unboxObject = input.readBoolean();
    this.variables = (Map<VariableMeta, AviatorJavaType>) input.readObject();
    this.constantPool = (Map<Token<?>, AviatorObject>) input.readObject();
  }

  private void writeObject(ObjectOutputStream output) throws IOException {
    super.customWriteObject(output);
    output.writeObject(this.instruments);
    output.writeBoolean(this.unboxObject);
    output.writeObject(this.variables);
    output.writeObject(this.constantPool);
  }
}
