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

    public static final String CLASS_IRI_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#";
    public static final String ATTRIBUTE_IRI_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#";

    public static final String cOwlClassA = CLASS_IRI_BASE + "OWLClassA";
    public static final String cOwlClassB = CLASS_IRI_BASE + "OWLClassB";
    public static final String cOWLClassS = CLASS_IRI_BASE + "OWLClassS";
    public static final String cOWLClassT = CLASS_IRI_BASE + "OWLClassT";
    public static final String cOWLClassQ = CLASS_IRI_BASE + "OWLClassQ";
    public static final String cOWLClassSParent = CLASS_IRI_BASE + "OWLClassSParent";

    public static final String pAStringAttribute = ATTRIBUTE_IRI_BASE + "A-stringAttribute";
    public static final String pBStringAttribute = ATTRIBUTE_IRI_BASE + "B-stringAttribute";

    public static final String tIntegerAttribute = ATTRIBUTE_IRI_BASE + "T-integerAttribute";
    public static final String hasOwlClassA = ATTRIBUTE_IRI_BASE + "hasOwlClassA";

    public static final String qParentStringAttribute = ATTRIBUTE_IRI_BASE + "QParent-stringAttribute";
    public static final String qStringAttribute = ATTRIBUTE_IRI_BASE + "Q-stringAttribute";

    private Vocabulary() {
        throw new AssertionError();
    }
}
