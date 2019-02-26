/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.util;

import java.net.URI;
import java.util.Objects;
import java.util.Random;

/**
 * Utility for automatic identifier generation.
 */
public class IdentifierUtils {

    private static final Random RANDOM = new Random();

    private IdentifierUtils() {
        throw new AssertionError();
    }

    /**
     * Generates a (pseudo) random identifier based on the specified class URI.
     * <p>
     * The identifier consists of the class URI and then contains the string 'instance' and a random integer to ensure
     * uniqueness. The 'instance' part is appended after a slash or a _, if the class URI contains a hash fragment.
     *
     * @param classUri Class URI used as identifier base
     * @return Generated identifier
     */
    public static URI generateIdentifier(URI classUri) {
        Objects.requireNonNull(classUri);
        if (classUri.getFragment() != null) {
            return URI.create(classUri.toString() + "_instance" + RANDOM.nextInt());
        } else {
            String base = classUri.toString();
            if (base.endsWith("/")) {
                return URI.create(base + "instance" + RANDOM.nextInt());
            } else {
                return URI.create(base + "/instance" + RANDOM.nextInt());
            }
        }
    }
}
