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

    public static final String C_THING = "http://www.w3.org/2002/07/owl#Thing";

    public static final String C_OWL_CLASS_A = CLASS_IRI_BASE + "OWLClassA";
    public static final String C_OWL_CLASS_B = CLASS_IRI_BASE + "OWLClassB";
    public static final String C_OWL_CLASS_D = CLASS_IRI_BASE + "OWLClassD";
    public static final String C_OWL_CLASS_E = CLASS_IRI_BASE + "OWLClassE";
    public static final String C_OWL_CLASS_M = CLASS_IRI_BASE + "OWLClassM";
    public static final String C_OWL_CLASS_S = CLASS_IRI_BASE + "OWLClassS";
    public static final String C_OWL_CLASS_T = CLASS_IRI_BASE + "OWLClassT";
    public static final String C_OWL_CLASS_U = CLASS_IRI_BASE + "OWLClassU";
    public static final String C_OWL_CLASS_Q = CLASS_IRI_BASE + "OWLClassQ";
    public static final String C_OWL_CLASS_S_PARENT = CLASS_IRI_BASE + "OWLClassSParent";
    public static final String C_OWL_CLASS_V = CLASS_IRI_BASE + "OWLClassV";

    public static final String p_m_booleanAttribute = ATTRIBUTE_IRI_BASE + "m-booleanAttribute";
    public static final String p_m_intAttribute = ATTRIBUTE_IRI_BASE + "m-intAttribute";
    public static final String p_m_longAttribute = ATTRIBUTE_IRI_BASE + "m-longAttribute";
    public static final String p_m_doubleAttribute = ATTRIBUTE_IRI_BASE + "m-doubleAttribute";
    public static final String p_m_dateAttribute = ATTRIBUTE_IRI_BASE + "m-dateAttribute";
    public static final String p_m_enumAttribute = ATTRIBUTE_IRI_BASE + "m-enumAttribute";
    public static final String p_m_IntegerSet = ATTRIBUTE_IRI_BASE + "m-pluralIntAttribute";

    public static final String P_A_STRING_ATTRIBUTE = ATTRIBUTE_IRI_BASE + "A-stringAttribute";
    public static final String P_B_STRING_ATTRIBUTE = ATTRIBUTE_IRI_BASE + "B-stringAttribute";
    public static final String P_E_STRING_ATTRIBUTE = ATTRIBUTE_IRI_BASE + "E-stringAttribute";

    public static final String P_T_INTEGER_ATTRIBUTE = ATTRIBUTE_IRI_BASE + "T-integerAttribute";
    public static final String P_HAS_OWL_CLASS_A = ATTRIBUTE_IRI_BASE + "hasOwlClassA";

    public static final String P_Q_PARENT_STRING_ATTRIBUTE = ATTRIBUTE_IRI_BASE + "QParent-stringAttribute";
    public static final String P_Q_STRING_ATTRIBUTE = ATTRIBUTE_IRI_BASE + "Q-stringAttribute";

    public static final String P_HAS_OWL_CLASS_S = ATTRIBUTE_IRI_BASE + "hasOwlClassS";

    public static final String V_HAS_THING = ATTRIBUTE_IRI_BASE + "hasThing";

    private Vocabulary() {
        throw new AssertionError();
    }
}
