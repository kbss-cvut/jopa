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
package cz.cvut.kbss.jopa.example02.model;

public class Vocabulary {

    private Vocabulary() {
        throw new AssertionError();
    }

    public static final String BASE_URI = "http://krizik.felk.cvut.cz/ontologies/jopa/example02#";

    public static final String JEDI = BASE_URI + "Jedi";
    public static final String HAS_CHILD = BASE_URI + "hasChild";
    public static final String HAS_FATHER = BASE_URI + "hasFather";

    public static final String FIRST_NAME = "http://xmlns.com/foaf/0.1/firstName";
    public static final String LAST_NAME = "http://xmlns.com/foaf/0.1/lastName";
    public static final String NICKNAME = "http://xmlns.com/foaf/0.1/nick";
}
