package com.googlecode.aviator.runtime.function.system;

import java.text.SimpleDateFormat;
import com.googlecode.aviator.utils.LRUMap;


/**
 * DateFormat cache
 * 
 * @author boyan
 * 
 */
public class DateFormatCache {

  private static int maxSize =
      Integer.valueOf(System.getProperty("aviator.date_format.cache.max", "256"));

  private static ThreadLocal<LRUMap<String/* format */, SimpleDateFormat>> formatCache =
      new ThreadLocal<LRUMap<String, SimpleDateFormat>>();


  public static SimpleDateFormat getOrCreateDateFormat(String format) {
    LRUMap<String/* format */, SimpleDateFormat> cache = formatCache.get();
    if (cache == null) {
      cache = new LRUMap<String, SimpleDateFormat>(maxSize);
      formatCache.set(cache);
    }
    SimpleDateFormat rt = cache.get(format);
    if (rt == null) {
      rt = new SimpleDateFormat(format);
      cache.put(format, rt);
    }
    return rt;
  }

}
