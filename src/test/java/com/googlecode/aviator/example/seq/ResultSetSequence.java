package com.googlecode.aviator.example.seq;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import com.googlecode.aviator.runtime.type.Collector;
import com.googlecode.aviator.runtime.type.Sequence;
import com.googlecode.aviator.runtime.type.seq.ListCollector;
import com.googlecode.aviator.utils.Reflector;

/**
 * A sequence wraps java.seql.ResultSet
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ResultSetSequence implements Sequence<Map<String, Object>> {
  private final ResultSet resultSet;



  public ResultSetSequence(final ResultSet resultSet) {
    super();
    this.resultSet = resultSet;
  }

  @Override
  public Iterator<Map<String, Object>> iterator() {
    return new Iterator<Map<String, Object>>() {

      @Override
      public boolean hasNext() {
        try {
          return ResultSetSequence.this.resultSet.next();
        } catch (SQLException e) {
          throw Reflector.sneakyThrow(e);
        }
      }

      @Override
      public Map<String, Object> next() {
        try {
          Map<String, Object> user = new HashMap<>();
          user.put("username", ResultSetSequence.this.resultSet.getString("username"));
          user.put("age", ResultSetSequence.this.resultSet.getInt("age"));
          return user;
        } catch (SQLException e) {
          throw Reflector.sneakyThrow(e);
        }
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException();
      }

    };
  }

  @Override
  public Collector newCollector(final int size) {
    return new ListCollector(false);
  }

  @Override
  public int hintSize() {
    // if we don't known the exact row number, return 0.
    return 0;
  }

}
