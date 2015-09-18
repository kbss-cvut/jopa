/**
 * Copyright (C) 2011 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.jopa.model;

import java.net.URI;

/* GENERATED CODE - DO NOT MODIFY */
public class SequencesVocabulary {

    private SequencesVocabulary() {
        throw new AssertionError();
    }

    // http://krizik.felk.cvut.cz/ontologies/2008/sequences.owl//
    // ========================= Classes =========================
    public static final String s_c_Nothing = "http://www.w3.org/2002/07/owl#Nothing";
    public static final URI c_Nothing = URI
            .create("http://www.w3.org/2002/07/owl#Nothing");
    public static final String s_c_EmptyList = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#EmptyList";
    public static final URI c_EmptyList = URI
            .create("http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#EmptyList");
    public static final String s_c_OWLList = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#OWLList";
    public static final URI c_OWLList = URI
            .create("http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#OWLList");
    public static final URI c_OWLSimpleList = URI
            .create("http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#OWLSimpleList");
    public static final URI c_OWLReferencedList = URI
            .create("http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#OWLComplexList");

    // ========================= Object Properties =========================
    public static final String s_p_hasListItem = "http://krizik.felk.cvut.cz/ontologies/2008/sequences.owl#hasListItem";
    public static final URI p_hasListItem = URI
            .create("http://krizik.felk.cvut.cz/ontologies/2008/sequences.owl#hasListItem");
    public static final String s_p_hasListProperty = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasListProperty";
    public static final URI p_hasListProperty = URI
            .create("http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasListProperty");
    public static final String s_p_hasContents = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasContents";
    public static final URI p_hasContents = URI
            .create("http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasContents");
    public static final String s_p_hasNext = "http://krizik.felk.cvut.cz/ontologies/2008/sequences.owl#hasNext";
    public static final URI p_hasNext = URI
            .create("http://krizik.felk.cvut.cz/ontologies/2008/sequences.owl#hasNext");
    public static final String s_p_isFollowedBy = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#isFollowedBy";
    public static final URI p_isFollowedBy = URI
            .create("http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#isFollowedBy");

    // ======================= collections.owl ================================
    public static final String c_List = "http://swan.mindinformatics.org/ontologies/1.2/collections/List";
    public static final String p_element = "http://swan.mindinformatics.org/ontologies/1.2/collections/element";
    public static final String c_Collection = "http://swan.mindinformatics.org/ontologies/1.2/collections/Collection";


}