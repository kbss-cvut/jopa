/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.vocabulary;

/**
 * Constants representing the Simple Knowledge Organization Scheme (SKOS) classes and properties.
 *
 * @see <a href="https://www.w3.org/2009/08/skos-reference/skos.html">https://www.w3.org/2009/08/skos-reference/skos.html</a>
 */
public final class SKOS {

    /**
     * SKOS namespace.
     */
    public static final String NAMESPACE = "http://www.w3.org/2004/02/skos/core#";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#Collection">Collection</a> class.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#collections">http://www.w3.org/TR/skos-reference/#collections</a>
     */
    public static final String COLLECTION = NAMESPACE + "Collection";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#Concept">Concept</a> class.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#concepts">http://www.w3.org/TR/skos-reference/#concepts</a>
     */
    public static final String CONCEPT = NAMESPACE + "Concept";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#ConceptScheme">Concept Scheme</a> class.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#schemes">http://www.w3.org/TR/skos-reference/#schemes</a>
     */
    public static final String CONCEPT_SCHEME = NAMESPACE + "ConceptScheme";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#OrderedCollection">Ordered Collection</a>
     * class.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#collections">http://www.w3.org/TR/skos-reference/#collections</a>
     */
    public static final String ORDERED_COLLECTION = NAMESPACE + "OrderedCollection";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#altLabel">alternative label</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#labels">http://www.w3.org/TR/skos-reference/#labels</a>
     */
    public static final String ALT_LABEL = NAMESPACE + "altLabel";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#broadMatch">has broader match</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#mapping">http://www.w3.org/TR/skos-reference/#mapping</a>
     */
    public static final String BROAD_MATCH = NAMESPACE + "broadMatch";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#broader">has broader</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#semantic-relations">http://www.w3.org/TR/skos-reference/#semantic-relations</a>
     */
    public static final String BROADER = NAMESPACE + "broader";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#broaderTransitive">has broader transitive</a>
     * property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#semantic-relations">http://www.w3.org/TR/skos-reference/#semantic-relations</a>
     */
    public static final String BROADER_TRANSITIVE = NAMESPACE + "broaderTransitive";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#changeNote">change note</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#notes">http://www.w3.org/TR/skos-reference/#notes</a>
     */
    public static final String CHANGE_NOTE = NAMESPACE + "changeNote";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#closeMatch">has close match</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#mapping">http://www.w3.org/TR/skos-reference/#mapping</a>
     */
    public static final String CLOSE_MATCH = NAMESPACE + "closeMatch";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#definition">definition</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#notes">http://www.w3.org/TR/skos-reference/#notes</a>
     */
    public static final String DEFINITION = NAMESPACE + "definition";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#editorialNote">editorial note</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#notes">http://www.w3.org/TR/skos-reference/#notes</a>
     */
    public static final String EDITORIAL_NOTE = NAMESPACE + "editorialNote";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#exactMatch">has exact match</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#mapping">http://www.w3.org/TR/skos-reference/#mapping</a>
     */
    public static final String EXACT_MATCH = NAMESPACE + "exactMatch";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#example">example</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#notes">http://www.w3.org/TR/skos-reference/#notes</a>
     */
    public static final String EXAMPLE = NAMESPACE + "example";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#hasTopConcept">has top concept</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#schemes">http://www.w3.org/TR/skos-reference/#schemes</a>
     */
    public static final String HAS_TOP_CONCEPT = NAMESPACE + "hasTopConcept";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#hiddenLabel">hidden label</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#labels">http://www.w3.org/TR/skos-reference/#labels</a>
     */
    public static final String HIDDEN_LABEL = NAMESPACE + "hiddenLabel";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#historyNote">history note</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#notes">http://www.w3.org/TR/skos-reference/#notes</a>
     */
    public static final String HISTORY_NOTE = NAMESPACE + "historyNote";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#inScheme">is in scheme</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#schemes">http://www.w3.org/TR/skos-reference/#schemes</a>
     */
    public static final String IN_SCHEME = NAMESPACE + "inScheme";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#mappingRelation">is in mapping relation
     * with</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#mapping">http://www.w3.org/TR/skos-reference/#mapping</a>
     */
    public static final String MAPPING_RELATION = NAMESPACE + "mappingRelation";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#member">has member</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#collections">http://www.w3.org/TR/skos-reference/#collections</a>
     */
    public static final String MEMBER = NAMESPACE + "member";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#memberList">has member list</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#collections">http://www.w3.org/TR/skos-reference/#collections</a>
     */
    public static final String MEMBER_LIST = NAMESPACE + "memberList";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#narrowMatch">has narrower match</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#mapping">http://www.w3.org/TR/skos-reference/#mapping</a>
     */
    public static final String NARROW_MATCH = NAMESPACE + "narrowMatch";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#narrower">has narrower</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#semantic-relations">http://www.w3.org/TR/skos-reference/#semantic-relations</a>
     */
    public static final String NARROWER = NAMESPACE + "narrower";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#narrowerTransitive">has narrower transitive</a>
     * property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#semantic-relations">http://www.w3.org/TR/skos-reference/#semantic-relations</a>
     */
    public static final String NARROWER_TRANSITIVE = NAMESPACE + "narrowerTransitive";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#notation">notation</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#notations">http://www.w3.org/TR/skos-reference/#notations</a>
     */
    public static final String NOTATION = NAMESPACE + "notation";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#note">note</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#notes">http://www.w3.org/TR/skos-reference/#notes</a>
     */
    public static final String NOTE = NAMESPACE + "note";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#prefLabel">preferred label</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#labels">http://www.w3.org/TR/skos-reference/#labels</a>
     */
    public static final String PREF_LABEL = NAMESPACE + "prefLabel";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#related">has related</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#semantic-relations">http://www.w3.org/TR/skos-reference/#semantic-relations</a>
     */
    public static final String RELATED = NAMESPACE + "related";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#relatedMatch">has related match</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#mapping">http://www.w3.org/TR/skos-reference/#mapping</a>
     */
    public static final String RELATED_MATCH = NAMESPACE + "relatedMatch";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#scopeNote">scope note</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#notes">http://www.w3.org/TR/skos-reference/#notes</a>
     */
    public static final String SCOPE_NOTE = NAMESPACE + "scopeNote";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#semanticRelation">is in semantic relation
     * with</a> property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#semantic-relations">http://www.w3.org/TR/skos-reference/#semantic-relations</a>
     */
    public static final String SEMANTIC_RELATION = NAMESPACE + "semanticRelation";

    /**
     * SKOS <a href="https://www.w3.org/2009/08/skos-reference/skos.html#topConceptOf">is top concept in scheme</a>
     * property.
     *
     * @see <a href="http://www.w3.org/TR/skos-reference/#schemes">http://www.w3.org/TR/skos-reference/#schemes</a>
     */
    public static final String TOP_CONCEPT_OF = NAMESPACE + "topConceptOf";

    private SKOS() {
        throw new AssertionError();
    }
}
