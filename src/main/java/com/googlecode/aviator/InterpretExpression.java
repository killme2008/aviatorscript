package com.googlecode.aviator;

import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;
import com.googlecode.aviator.code.interpreter.ir.JumpIR;
import com.googlecode.aviator.code.interpreter.ir.LoadIR;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.parser.VariableMeta;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Env;

public class InterpretExpression extends BaseExpression {

  private final List<IR> instruments;

  private final boolean unboxObject;

  private final Map<VariableMeta, AviatorJavaType> variables =
      new IdentityHashMap<VariableMeta, AviatorJavaType>();

  private final Map<Token<?>, AviatorObject> constantPool = new IdentityHashMap<>();


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
    InterpretContext ctx = new InterpretContext(this, instruments, getCompileEnv());
    for (Token<?> token : constants) {
      final LoadIR loadConstantIR = new LoadIR(this.sourceFile, token, null, false);
      loadConstantIR.eval(ctx);
      this.constantPool.put(token, ctx.pop());
    }
  }

  public AviatorJavaType loadVar(final VariableMeta v) {
    return this.variables.get(v);
  }

  public AviatorObject loadConstant(final Token<?> token) {
    return this.constantPool.get(token);
  }

  @Override
  public Object executeDirectly(final Map<String, Object> env) {
    final boolean trace = RuntimeUtils.isTracedEval(env);
    if (trace) {
      int pc = 0;
      RuntimeUtils.printlnTrace(env, "Expression instruments: ");
      for (IR ir : this.instruments) {
        RuntimeUtils.printlnTrace(env, "    " + (pc++) + " " + ir.toString());
      }
      RuntimeUtils.printlnTrace(env, "    " + pc + " return");
      RuntimeUtils.printlnTrace(env, "Execute instruments: ");
    }


    InterpretContext ctx = new InterpretContext(this, this.instruments, (Env) env);
    IR ir = null;
    while ((ir = ctx.getPc()) != null) {
      if (trace) {
        RuntimeUtils.printlnTrace(env, "    " + ir + "    " + ctx.descOperandsStack());
      }
      ir.eval(ctx);
      if (ir instanceof JumpIR) {
        if (ir != ctx.getPc()) {
          // jump successfully, we don't move pc to next.
          continue;
        }
      }
      if (!ctx.next()) {
        break;
      }
    }
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
}
