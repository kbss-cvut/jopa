/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.environment;

public class Vocabulary {

    public static final String ATTRIBUTE_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#";
    public static final String CLASS_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/types#";
    public static final String INDIVIDUAL_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#";

    public static final String c_OwlClassA = CLASS_BASE + "OWLClassA";
    public static final String c_OwlClassB = CLASS_BASE + "OWLClassB";
    public static final String c_OwlClassC = CLASS_BASE + "OWLClassC";
    public static final String c_OwlClassD = CLASS_BASE + "OWLClassD";
    public static final String c_OwlClassE = CLASS_BASE + "OWLClassE";
    public static final String c_OwlClassF = CLASS_BASE + "OWLClassF";
    public static final String c_OwlClassG = CLASS_BASE + "OWLClassG";
    public static final String c_OwlClassH = CLASS_BASE + "OWLClassH";
    public static final String c_OwlClassJ = CLASS_BASE + "OWLClassJ";
    public static final String c_OwlClassL = CLASS_BASE + "OWLClassL";
    public static final String c_OwlClassM = CLASS_BASE + "OWLClassM";
    public static final String c_OwlClassN = CLASS_BASE + "OWLClassN";
    public static final String c_OwlClassQ = CLASS_BASE + "OWLClassQ";
    public static final String C_OWLClassR = CLASS_BASE + "OWLClassR";
    public static final String c_OwlClassS = CLASS_BASE + "OWLClassS";
    public static final String c_OwlClassT = CLASS_BASE + "OWLClassT";
    public static final String c_OwlClassU = CLASS_BASE + "OWLClassU";
    public static final String c_OwlClassV = CLASS_BASE + "OWLClassV";
    public static final String c_OwlClassW = CLASS_BASE + "OWLClassW";
    public static final String c_OwlClassWithQueryAttr = CLASS_BASE + "OWLClassWithQueryAttr";
    public static final String c_Person = CLASS_BASE + "Person";
    public static final String c_Phone = CLASS_BASE + "Phone";

    public static final String p_f_stringAttribute = ATTRIBUTE_BASE + "f-stringAttribute";
    public static final String p_f_setAttribute = ATTRIBUTE_BASE + "f-hasA";

    public static final String p_g_hasH = ATTRIBUTE_BASE + "hasH";
    public static final String p_h_hasA = ATTRIBUTE_BASE + "hasA";
    public static final String p_h_hasG = ATTRIBUTE_BASE + "hasG";

    public static final String p_l_singleA = ATTRIBUTE_BASE + "hasAExtra";

    public static final String p_a_stringAttribute = ATTRIBUTE_BASE + "A-stringAttribute";
    public static final String p_e_stringAttribute = ATTRIBUTE_BASE + "E-stringAttribute";
    public static final String p_m_booleanAttribute = ATTRIBUTE_BASE + "m-booleanAttribute";
    public static final String p_m_intAttribute = ATTRIBUTE_BASE + "m-intAttribute";
    public static final String p_m_longAttribute = ATTRIBUTE_BASE + "m-longAttribute";
    public static final String p_m_doubleAttribute = ATTRIBUTE_BASE + "m-doubleAttribute";
    public static final String p_m_dateAttribute = ATTRIBUTE_BASE + "m-dateAttribute";
    public static final String p_m_characterAttribute = ATTRIBUTE_BASE + "m-characterAttribute";
    public static final String p_m_enumAttribute = ATTRIBUTE_BASE + "m-enumAttribute";
    public static final String p_m_ordinalEnumAttribute = ATTRIBUTE_BASE + "m-ordinalEnumAttribute";
    public static final String p_m_IntegerSet = ATTRIBUTE_BASE + "m-pluralIntAttribute";
    public static final String p_m_lexicalForm = ATTRIBUTE_BASE + "m-lexicalForm";
    public static final String p_m_simpleLiteral = ATTRIBUTE_BASE + "m-simpleLiteral";
    public static final String p_m_explicitDatatype = ATTRIBUTE_BASE + "m-explicitDatatype";
    public static final String p_m_withConverter = ATTRIBUTE_BASE + "m-withConverter";
    public static final String p_m_objectOneOfEnumAttribute = ATTRIBUTE_BASE + "m-objectOneOfEnumAttribute";

    public static final String p_q_parentStringAttribute = ATTRIBUTE_BASE + "q-parentStringAttribute";
    public static final String p_q_stringAttribute = ATTRIBUTE_BASE + "q-stringAttribute";

    public static final String P_R_STRING_ATTRIBUTE = ATTRIBUTE_BASE + "R-stringAttribute";
    public static final String P_HAS_A = ATTRIBUTE_BASE + "hasA";

    public static final String P_HAS_SIMPLE_LIST = ATTRIBUTE_BASE + "C-hasSimpleSequence";
    public static final String P_HAS_REFERENCED_LIST = ATTRIBUTE_BASE + "C-hasReferencedSequence";
    public static final String P_HAS_RDF_COLLECTION = ATTRIBUTE_BASE + "C-hasRDFCollection";
    public static final String P_HAS_RDF_SEQ = ATTRIBUTE_BASE + "C-hasRDFSeq";

    public static final String P_T_LOCAL_DATE_ATTRIBUTE = ATTRIBUTE_BASE + "tLocalDate";
    public static final String P_T_LOCAL_DATETIME_ATTRIBUTE = ATTRIBUTE_BASE + "tLocalDateTime";
    public static final String P_T_HAS_S = ATTRIBUTE_BASE + "tHasS";

    public static final String P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE = ATTRIBUTE_BASE + "singularMultilingualAttribute";
    public static final String P_U_PLURAL_MULTILINGUAL_ATTRIBUTE = ATTRIBUTE_BASE + "pluralMultilingualAttribute";

    public static final String P_V_SINGULAR_DYNAMIC_ATTRIBUTE = ATTRIBUTE_BASE + "singularDynamicAttribute";
    public static final String P_V_PLURAL_DYNAMIC_ATTRIBUTE = ATTRIBUTE_BASE + "pluralDynamicAttribute";

    public static final String P_W_SET_STRING_ATTRIBUTE = ATTRIBUTE_BASE + "setStringAttribute";
    public static final String P_W_LIST_STRING_ATTRIBUTE = ATTRIBUTE_BASE + "listStringAttribute";
    public static final String P_W_COLLECTION_STRING_ATTRIBUTE = ATTRIBUTE_BASE + "collectionStringAttribute";

    public static final String p_p_firstName = ATTRIBUTE_BASE + "firstName";
    public static final String p_p_lastName = ATTRIBUTE_BASE + "lastName";
    public static final String p_p_username = ATTRIBUTE_BASE + "username";
    public static final String p_p_age = ATTRIBUTE_BASE + "age";
    public static final String p_p_hasPhone = ATTRIBUTE_BASE + "hasPhone";
    public static final String p_p_gender = ATTRIBUTE_BASE + "gender";
    public static final String p_p_phoneNumber = ATTRIBUTE_BASE + "phoneNumber";
    public static final String p_p_phoneBrand = ATTRIBUTE_BASE + "phoneBrand";

    private Vocabulary() {
        throw new AssertionError();
    }
}
