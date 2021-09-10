package com.googlecode.aviator;

import java.util.List;
import java.util.Map;
import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;
import com.googlecode.aviator.code.interpreter.ir.JumpIR;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.parser.VariableMeta;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Env;

public class InterpretExpression extends BaseExpression {

  private final List<IR> instruments;

  public InterpretExpression(final AviatorEvaluatorInstance instance, final List<VariableMeta> vars,
      final SymbolTable symbolTable, final List<IR> instruments) {
    super(instance, vars, symbolTable);
    this.instruments = instruments;

  }

  @Override
  public Object executeDirectly(final Map<String, Object> env) {


    if (RuntimeUtils.isTracedEval(env)) {
      int lineNo = 0;
      for (IR ir : this.instruments) {
        System.out.println((lineNo++) + " " + ir.toString());
      }
    }

    InterpretContext ctx = new InterpretContext(this, this.instruments, (Env) env);
    IR ir = null;
    while ((ir = ctx.getPc()) != null) {
      // System.out.println(ir + " " + ctx.getOperands());
      ir.eval(ctx);
      if (ir instanceof JumpIR) {
        if (ir != ctx.getPc()) {
          continue;
        }
      }
      if (!ctx.next()) {
        break;
      }
    }
    final AviatorObject top = ctx.peek();
    if (top == null) {
      return null;
    }
    return top.getValue(env);
  }
}
