/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.ontodriver.model.Assertion;

public class Vocabulary {

    private static final String ATTRIBUTE_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#";
    public static final String CLASS_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#";

    public static final String c_OwlClassM = CLASS_BASE + "OWLClassM";
    public static final String C_OWLClassR = CLASS_BASE + "OWLClassR";
    public static final String c_OwlClassS = CLASS_BASE + "OWLClassS";

    public static final String p_m_booleanAttribute = ATTRIBUTE_BASE + "m-booleanAttribute";
    public static final String p_m_intAttribute = ATTRIBUTE_BASE + "m-intAttribute";
    public static final String p_m_longAttribute = ATTRIBUTE_BASE + "m-longAttribute";
    public static final String p_m_doubleAttribute = ATTRIBUTE_BASE + "m-doubleAttribute";
    public static final String p_m_dateAttribute = ATTRIBUTE_BASE + "m-dateAttribute";
    public static final String p_m_enumAttribute = ATTRIBUTE_BASE + "m-enumAttribute";
    public static final String p_m_IntegerSet = ATTRIBUTE_BASE + "m-pluralIntAttribute";

    public static final String P_R_STRING_ATTRIBUTE = ATTRIBUTE_BASE + "R-stringAttribute";
    public static final String P_HAS_A = ATTRIBUTE_BASE + "hasA";

    private Vocabulary() {
        throw new AssertionError();
    }
}
