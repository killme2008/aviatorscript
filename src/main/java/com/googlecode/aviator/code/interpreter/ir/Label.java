package com.googlecode.aviator.code.interpreter.ir;

public class Label {
  public String name;
  public int i;

  public Label(final int i) {
    this.i = i;
    this.name = "L" + i;
  }

  @Override
  public String toString() {
    return this.name;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + this.i;
    return result;
  }

  @Override
  public boolean equals(final Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    Label other = (Label) obj;
    if (this.i != other.i) {
      return false;
    }
    return true;
  }

}
