package com.googlecode.aviator.example;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;


public class CollectionExample {
  public static void main(String[] args) {
    final List<String> list = new ArrayList<String>();
    list.add("hello");
    list.add(" world");

    final int[] array = new int[3];
    array[0] = 0;
    array[1] = 1;
    array[2] = 3;

    final Map<String, Date> map = new HashMap<String, Date>();
    map.put("date", new Date());

    Map<String, Object> env = new HashMap<String, Object>();
    env.put("list", list);
    env.put("array", array);
    env.put("mmap", map);

    System.out.println(AviatorEvaluator.execute(
        "list[0]+list[1]+'\narray[0]+array[1]+array[2]='+(array[0]+array[1]+array[2]) +' \ntoday is '+mmap.date ",
        env));
  }
}
