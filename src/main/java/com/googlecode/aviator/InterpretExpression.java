package com.googlecode.aviator;

import java.util.List;
import java.util.Map;
import com.googlecode.aviator.code.interpreter.Context;
import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.parser.VariableMeta;
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
    Context ctx = new Context(this, this.instruments, (Env) env);
    IR ir = null;
    while ((ir = ctx.getPc()) != null) {
      ir.eval(ctx);
      if (!ctx.next()) {
        break;
      }
    }
    return ctx.peek().getValue(env);
  }
}
