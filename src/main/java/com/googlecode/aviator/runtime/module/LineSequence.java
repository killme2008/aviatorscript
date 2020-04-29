package com.googlecode.aviator.runtime.module;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Iterator;
import java.util.NoSuchElementException;
import com.googlecode.aviator.runtime.type.Collector;
import com.googlecode.aviator.runtime.type.Sequence;
import com.googlecode.aviator.runtime.type.seq.ListCollector;
import com.googlecode.aviator.utils.Reflector;

/**
 * Cast reader into a sequence of text lines in file.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
class LineSequence implements Sequence<String> {
  private final BufferedReader reader;

  public LineSequence(final BufferedReader reader) {
    super();
    this.reader = reader;
  }

  @Override
  public Iterator<String> iterator() {
    return new Iterator<String>() {
      String line;
      boolean eof;

      @Override
      public String next() {
        if (this.eof) {
          throw new NoSuchElementException();
        }
        return this.line;
      }

      private void readLine() {
        try {
          this.line = LineSequence.this.reader.readLine();
        } catch (IOException e) {
          throw Reflector.sneakyThrow(e);
        }
      }

      @Override
      public boolean hasNext() {
        if (this.eof) {
          return false;
        } else {
          readLine();
          this.eof = (this.line == null);
          return !this.eof;
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
    return new ListCollector(size, false);
  }

  @Override
  public int hintSize() {
    return 0;
  }
}
