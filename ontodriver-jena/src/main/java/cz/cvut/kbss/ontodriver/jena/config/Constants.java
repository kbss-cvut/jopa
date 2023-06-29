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
package cz.cvut.kbss.ontodriver.jena.config;

/**
 * Constants and default values for the Jena driver.
 */
public class Constants {

    /**
     * Default transaction isolation strategy used by the driver.
     *
     * @see JenaOntoDriverProperties#JENA_ISOLATION_STRATEGY
     */
    public static final String DEFAULT_ISOLATION_STRATEGY = JenaOntoDriverProperties.READ_COMMITTED;

    /**
     * Default auto commit setting for connections.
     *
     * @see cz.cvut.kbss.ontodriver.config.OntoDriverProperties#CONNECTION_AUTO_COMMIT
     */
    public static final boolean DEFAULT_AUTO_COMMIT = false;

    private Constants() {
        throw new AssertionError();
    }
}
