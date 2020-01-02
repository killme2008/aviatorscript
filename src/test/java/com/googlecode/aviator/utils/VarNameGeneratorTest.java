package com.googlecode.aviator.utils;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class VarNameGeneratorTest {
  private VarNameGenerator gen;

  @Test
  public void testGen() {
    this.gen = new VarNameGenerator();
    assertEquals("A_0", this.gen.gen());
    assertEquals("A_1", this.gen.gen());
    assertEquals("A_2", this.gen.gen());
  }

  @Test
  public void testGenOverflow() {
    this.gen = new VarNameGenerator(Long.MAX_VALUE - 3);
    assertEquals("A_9223372036854775804", this.gen.gen());
    assertEquals("A_9223372036854775805", this.gen.gen());
    assertEquals("A_9223372036854775806", this.gen.gen());
    assertEquals("A_9223372036854775807", this.gen.gen());
    assertEquals("A_0", this.gen.gen());
    assertEquals("A_1", this.gen.gen());
    assertEquals("A_2", this.gen.gen());
  }
}
