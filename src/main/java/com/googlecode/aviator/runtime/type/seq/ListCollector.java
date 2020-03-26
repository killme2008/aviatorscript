package com.googlecode.aviator.runtime.type.seq;

import java.util.ArrayList;
import java.util.List;
import com.googlecode.aviator.runtime.type.Collector;

public class ListCollector implements Collector {
  private static final int INIT_CAP = 10;
  @SuppressWarnings("rawtypes")
  List list;
  boolean returnArray;

  public ListCollector(final boolean returnArray) {
    this(INIT_CAP, returnArray);
  }

  public ListCollector(final int size, final boolean returnArray) {
    this.list = new ArrayList<>(size > 0 ? size : INIT_CAP);
    this.returnArray = returnArray;
  }

  @SuppressWarnings("unchecked")
  @Override
  public void add(final Object e) {
    this.list.add(e);
  }

  @Override
  public Object getRawContainer() {
    if (this.returnArray) {
      return this.list.toArray();
    } else {
      return this.list;
    }
  }
}
