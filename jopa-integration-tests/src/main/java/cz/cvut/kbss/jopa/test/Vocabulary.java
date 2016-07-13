/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test;

public class Vocabulary {

    public static final String cOwlClassB = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassB";

    public static final String pAStringAttribute = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute";
    public static final String pBStringAttribute = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute";

    private Vocabulary() {
        throw new AssertionError();
    }
}
