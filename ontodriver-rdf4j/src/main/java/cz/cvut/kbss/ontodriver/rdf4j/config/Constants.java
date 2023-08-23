/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.config;

public class Constants {

    /**
     * How many assertions will trigger load-all statement loading strategy.
     *
     * @see Rdf4jOntoDriverProperties#LOAD_ALL_THRESHOLD
     */
    public static final int DEFAULT_LOAD_ALL_THRESHOLD = 5;

    /**
     * Default language to use when an {@link cz.cvut.kbss.ontodriver.model.Assertion} does not specify a language.
     * <p>
     * The {@code null} value ensures that strings will be saved as xsd:string and loaded with any language tag (or
     * without a language tag at all).
     */
    public static final String DEFAULT_LANG = null;

    /**
     * Default number of tries to connect to a remote repository.
     *
     * @see Rdf4jOntoDriverProperties#RECONNECT_ATTEMPTS
     */
    public static final int DEFAULT_RECONNECT_ATTEMPTS_COUNT = 5;

    /**
     * Default maximum connection pool size.
     *
     * @see Rdf4jOntoDriverProperties#MAX_CONNECTION_POOL_SIZE
     */
    public static final int DEFAULT_MAX_CONNECTIONS = 5;

    /**
     * Default connection request timeout (in milliseconds).
     *
     * @see Rdf4jOntoDriverProperties#CONNECTION_REQUEST_TIMEOUT
     */
    public static final int DEFAULT_CONNECTION_REQUEST_TIMEOUT = 1000;

    private Constants() {
        throw new AssertionError();
    }
}
