/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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
