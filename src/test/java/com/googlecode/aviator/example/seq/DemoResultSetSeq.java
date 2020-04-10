package com.googlecode.aviator.example.seq;

import java.sql.ResultSet;
import org.mockito.Mockito;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;

public class DemoResultSetSeq {

  public static void main(final String[] args) throws Exception {
    // Mock a result set.
    ResultSet resultSet = Mockito.mock(ResultSet.class);
    Mockito.when(resultSet.next()).thenReturn(true).thenReturn(true).thenReturn(false);
    Mockito.when(resultSet.getString("username")).thenReturn("dennis").thenReturn("catty");
    Mockito.when(resultSet.getInt("age")).thenReturn(30).thenReturn(20);

    // Use it in aviator
    Expression exp = AviatorEvaluator.getInstance().compileScript("examples/result_set_seq.av");
    exp.execute(exp.newEnv("results", new ResultSetSequence(resultSet)));

  }
}
