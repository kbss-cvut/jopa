package cz.cvut.kbss.jopa.test.environment;

import java.util.Map;
import java.util.Random;
import java.util.Set;

public class TestEnvironmentUtils {

    private static final Random RAND = new Random();

    public static int randomInt(int max) {
        return RAND.nextInt(max);
    }

    public static boolean arePropertiesEqual(Map<String, Set<String>> pOne,
                                             Map<String, Set<String>> pTwo) {
        if (pOne.size() != pTwo.size()) {
            return false;
        }
        for (Map.Entry<String, Set<String>> e : pOne.entrySet()) {
            if (!pTwo.containsKey(e.getKey())) {
                return false;
            }
            final Set<String> set = pTwo.get(e.getKey());
            if (!e.getValue().equals(set)) {
                return false;
            }
        }
        return true;
    }
}
