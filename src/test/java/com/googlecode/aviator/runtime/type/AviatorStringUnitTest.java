/**
 * Copyright (C) 2010 dennis zhuang (killmimport static org.junit.Assert.*;
 *
 * import java.text.SimpleDateFormat; import java.util.Date; import java.util.HashMap; import
 * java.util.Map;
 *
 * import org.junit.Test;
 *
 * import com.googlecode.aviator.exception.ExpressionRuntimeException; icense, or (at your option)
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program;
 * if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.runtime.type;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;
import com.googlecode.aviator.exception.ExpressionRuntimeException;

public class AviatorStringUnitTest {
  @Test
  public void testAddNumber() {
    final AviatorString s = new AviatorString("hello ");
    final AviatorNumber n = AviatorNumber.valueOf(30L);
    assertEquals("hello 30", s.add(n, null).getValue(null));
  }

  @Test
  public void testAddBoolean() {
    final AviatorString s = new AviatorString("hello ");
    final AviatorBoolean n = AviatorBoolean.TRUE;
    assertEquals("hello true", s.add(n, null).getValue(null));
  }

  @Test
  public void testAddString() {
    final AviatorString s = new AviatorString("hello ");
    final AviatorString n = new AviatorString("world");
    assertEquals("hello world", s.add(n, null).getValue(null));
  }

  @Test
  public void testAddJavaType() {
    final AviatorString s = new AviatorString("hello ");
    AviatorJavaType javaType = new AviatorJavaType("s");
    assertEquals("hello world", s.add(javaType, this.createEnvWith("s", "world")).getValue(null));

    javaType = new AviatorJavaType("true");
    assertEquals("hello true", s.add(javaType, this.createEnvWith(null, null)).getValue(null));

    javaType = new AviatorJavaType("a");
    assertEquals("hello 400", s.add(javaType, this.createEnvWith("a", 400)).getValue(null));

    javaType = new AviatorJavaType("a");
    assertEquals("hello 3.4", s.add(javaType, this.createEnvWith("a", 3.4f)).getValue(null));
  }

  private Map<String, Object> createEnvWith(String name, Object obj) {
    final Map<String, Object> env = new HashMap<String, Object>();
    if (name != null) {
      env.put(name, obj);
    }
    env.put("true", Boolean.TRUE);
    env.put("false", Boolean.FALSE);
    return env;
  }

  @Test
  public void testAddPattern() {
    final AviatorString s = new AviatorString("hello ");
    final AviatorPattern n = new AviatorPattern("[\\d\\.]+");
    assertEquals("hello [\\d\\.]+", s.add(n, null).getValue(null));
  }

  @Test
  public void testCompareString() {
    final AviatorString s = new AviatorString("hello ");
    AviatorString n = new AviatorString("world");
    assertTrue(s.compare(n, null) < 0);
    assertEquals(0, s.compare(s, null));

    n = new AviatorString("awt");
    assertFalse(s.compare(n, null) < 0);
  }

  @Test
  public void testCompareNullString() {
    final AviatorString s = new AviatorString(null);
    AviatorString n = new AviatorString("world");
    assertTrue(s.compare(n, null) < 0);
    assertEquals(0, s.compare(s, null));

    n = new AviatorString("awt");
    assertTrue(s.compare(n, null) < 0);

    AviatorJavaType js = new AviatorJavaType("a");
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", "null");
    assertTrue(s.compare(js, env) < 0);
    assertTrue(js.compare(s, env) > 0);

    AviatorRuntimeJavaType jr = new AviatorRuntimeJavaType("a");
    assertTrue(s.compare(jr, env) < 0);
    assertTrue(jr.compare(s, env) > 0);

    // both null
    assertTrue(s.compare(js, null) == 0);
    assertTrue(s.compare(new AviatorRuntimeJavaType(null), null) == 0);
  }

  @Test
  public void testCompareJavaString() {
    final AviatorString s = new AviatorString("hello ");
    assertEquals("hello ", s.getLexeme());
    AviatorJavaType n = new AviatorJavaType("s");
    assertTrue(s.compare(n, this.createEnvWith("s", "world")) < 0);
    assertEquals(0, s.compare(s, this.createEnvWith("s", "world")));

    n = new AviatorJavaType("s");
    assertFalse(s.compare(n, this.createEnvWith("s", "awt")) < 0);
  }

  @Test
  public void testCompareJavaChar() {
    final AviatorString s = new AviatorString("hello ");
    AviatorJavaType n = new AviatorJavaType("s");
    assertTrue(s.compare(n, this.createEnvWith("s", 'w')) < 0);
    assertEquals(0, s.compare(s, this.createEnvWith("s", 'w')));

    n = new AviatorJavaType("s");
    assertFalse(s.compare(n, this.createEnvWith("s", 'a')) < 0);
  }

  @Test(expected = ExpressionRuntimeException.class)
  public void testCompareBoolean() {
    final AviatorString s = new AviatorString("hello ");
    final AviatorBoolean n = AviatorBoolean.TRUE;
    s.compare(n, null);
  }

  @Test(expected = ExpressionRuntimeException.class)
  public void testComparePattern() {
    final AviatorString s = new AviatorString("hello ");
    final AviatorPattern n = new AviatorPattern("\\d+");
    s.compare(n, null);
  }

  @Test(expected = ExpressionRuntimeException.class)
  public void testCompareNumber() {
    final AviatorString s = new AviatorString("hello ");
    final AviatorNumber n = AviatorNumber.valueOf(3.4f);
    s.compare(n, null);
  }

  @Test(expected = ExpressionRuntimeException.class)
  public void testCompareJavaNumber() {
    final AviatorString s = new AviatorString("hello ");
    final AviatorJavaType n = new AviatorJavaType("f");
    s.compare(n, this.createEnvWith("f", 3.4f));
  }

  @Test(expected = ExpressionRuntimeException.class)
  public void testNot() {
    new AviatorString("hello").not(null);

  }

  @Test(expected = ExpressionRuntimeException.class)
  public void testNeg() {
    new AviatorString("hello").neg(null);

  }

  @Test
  public void testCompareJavaNull() {
    assertEquals(1, new AviatorString("").compare(new AviatorJavaType("a"), null));
    assertEquals(1, new AviatorString(" ").compare(new AviatorJavaType("a"), null));
    assertEquals(1, new AviatorString("hello").compare(new AviatorJavaType("a"), null));
  }

  @Test
  public void testCompareDate() {
    final Map<String, Object> env = new HashMap<String, Object>();
    final Date date = new Date();
    final String dateStr = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss:SS").format(date);
    env.put("date", date);
    env.put("dateStr", dateStr);
    final Date futureDate = new Date();
    futureDate.setYear(2200);
    env.put("future", futureDate);
    final Date oldDate = new Date();
    oldDate.setYear(20);
    env.put("old", oldDate);
    assertEquals(0, new AviatorString(dateStr).compare(new AviatorJavaType("date"), env));
    assertEquals(0, new AviatorString(dateStr).compare(new AviatorJavaType("dateStr"), env));
    assertEquals(-1, new AviatorString(dateStr).compare(new AviatorJavaType("future"), env));
    assertEquals(1, new AviatorString(dateStr).compare(new AviatorJavaType("old"), env));
  }

  @Test
  public void testAddNil() {
    assertEquals("null", new AviatorString("").add(AviatorNil.NIL, null).getValue(null));
    assertEquals("hello null",
        new AviatorString("hello ").add(AviatorNil.NIL, null).getValue(null));
    assertEquals("hello null",
        new AviatorString("hello ").add(new AviatorJavaType("a"), null).getValue(null));
  }
}
