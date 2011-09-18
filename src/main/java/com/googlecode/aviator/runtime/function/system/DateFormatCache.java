package com.googlecode.aviator.runtime.function.system;

import java.text.SimpleDateFormat;

import com.googlecode.aviator.utils.SyncLRUMap;


/**
 * DateFormat cache
 * 
 * @author boyan
 * 
 */
public class DateFormatCache {

    private static int maxSize = Integer.valueOf(System.getProperty("aviator.date_format.cache.max", "256"));

    private static SyncLRUMap<String/* format */, SimpleDateFormat> formatCache =
            new SyncLRUMap<String, SimpleDateFormat>(maxSize);


    public static SimpleDateFormat getOrCreateDateFormat(String format) {
        SimpleDateFormat rt = formatCache.get(format);
        if (rt == null) {
            synchronized (formatCache) {
                rt = formatCache.get(format);
                if (rt == null) {
                    rt = new SimpleDateFormat(format);
                }

            }
        }
        return rt;
    }

}
