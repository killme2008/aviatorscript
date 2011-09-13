import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.googlecode.aviator.AviatorEvaluator;

public class Example {
	public static void main(String[] args) {
		// String email = "killme2008@gmail.com";
		// Map<String, Object> env = new HashMap<String, Object>();
		// env.put("email", email);
		// String username = (String) AviatorEvaluator.execute(
		// "email=~/([\\w0-8]+)@\\w+[\\.\\w+]+/ ? $1:'unknow'", env);
		// System.out.println(username);
		String exp = "include(list1,a) && include(list2,b)";
		List<Integer> list1 = new ArrayList<Integer>();
		list1.add(1);
		list1.add(2);
		list1.add(3);

		List<Integer> list2 = new ArrayList<Integer>();
		list2.add(1);
		list2.add(4);

		Map<String, Object> env = new HashMap<String, Object>();
		env.put("list1", list1);
		env.put("list2", list2);
		env.put("a", 10);
		env.put("b", 1);

		System.out.println(AviatorEvaluator.execute(exp, env));

	}
}
