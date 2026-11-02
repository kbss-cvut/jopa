/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
 * The OWL vocabulary.
 * <p>
 * Contains vocabulary declared by both OWL and OWL 2.
 */
public final class OWL {

    /**
     * OWL vocabulary namespace.
     */
    public static final String NAMESPACE = "http://www.w3.org/2002/07/owl#";

    /**
     * Typical prefix used for {@link #NAMESPACE}.
     */
    public static final String PREFIX = "owl";

    /**
     * {@code owl:Class} class.
     */
    public static final String CLASS = NAMESPACE + "Class";

    /**
     * {@code owl:DeprecatedClass}
     */
    public static final String DEPRECATED_CLASS = NAMESPACE + "DeprecatedClass";

    /**
     * {@code owl:Annotation}
     */
    public static final String ANNOTATION = NAMESPACE + "Annotation";

    /**
     * {@code owl:Axiom}
     */
    public static final String AXIOM = NAMESPACE + "Axiom";

    /**
     * {@code owl:NegativePropertyAssertion}
     */
    public static final String NEGATIVE_PROPERTY_ASSERTION = NAMESPACE + "NegativePropertyAssertion";

    /**
     * {@code owl:annotatedProperty}
     */
    public static final String ANNOTATED_PROPERTY = NAMESPACE + "annotatedProperty";

    /**
     * {@code owl:annotatedSource}
     */
    public static final String ANNOTATED_SOURCE = NAMESPACE + "annotatedSource";

    /**
     * {@code owl:annotatedTarget}
     */
    public static final String ANNOTATED_TARGET = NAMESPACE + "annotatedTarget";

    /**
     * {@code owl:assertionProperty}
     */
    public static final String ASSERTION_PROPERTY = NAMESPACE + "assertionProperty";

    /**
     * {@code owl:sourceIndividual}
     */
    public static final String SOURCE_INDIVIDUAL = NAMESPACE + "sourceIndividual";

    /**
     * {@code owl:targetIndividual}
     */
    public static final String TARGET_INDIVIDUAL = NAMESPACE + "targetIndividual";

    /**
     * {@code owl:targetValue}
     */
    public static final String TARGET_VALUE = NAMESPACE + "targetValue";

    /**
     * {@code owl:deprecated}
     */
    public static final String DEPRECATED = NAMESPACE + "deprecated";

    /**
     * {@code owl:Thing}
     */
    public static final String THING = NAMESPACE + "Thing";

    /**
     * {@code owl:Nothing}
     */
    public static final String NOTHING = NAMESPACE + "Nothing";

    /**
     * {@code owl:minCardinality}
     */
    public static final String MIN_CARDINALITY = NAMESPACE + "minCardinality";

    /**
     * {@code owl:maxCardinality}
     */
    public static final String MAX_CARDINALITY = NAMESPACE + "maxCardinality";

    /**
     * {@code owl:cardinality}
     */
    public static final String CARDINALITY = NAMESPACE + "cardinality";

    /**
     * {@code owl:sameAs}
     */
    public static final String SAME_AS = NAMESPACE + "sameAs";

    /**
     * {@code owl:oneOf}
     */
    public static final String ONE_OF = NAMESPACE + "oneOf";

    /**
     * {@code owl:unionOf}
     */
    public static final String UNION_OF = NAMESPACE + "unionOf";

    /**
     * {@code owl:inverseOf}
     */
    public static final String INVERSE_OF = NAMESPACE + "inverseOf";

    /**
     * {@code owl:complementOf}
     */
    public static final String COMPLEMENT_OF = NAMESPACE + "complementOf";

    /**
     * {@code owl:disjointWith}
     */
    public static final String DISJOINT_WITH = NAMESPACE + "disjointWith";

    /**
     * {@code owl:intersectionOf}
     */
    public static final String INTERSECTION_OF = NAMESPACE + "intersectionOf";

    /**
     * {@code owl:differentFrom}
     */
    public static final String DIFFERENT_FROM = NAMESPACE + "differentFrom";

    /**
     * {@code owl:distinctMembers}
     */
    public static final String DISTINCT_MEMBERS = NAMESPACE + "distinctMembers";

    /**
     * {@code owl:members}
     */
    public static final String MEMBERS = NAMESPACE + "members";

    /**
     * {@code owl:disjointUnionOf}
     */
    public static final String DISJOINT_UNION_OF = NAMESPACE + "disjointUnionOf";

    /**
     * {@code owl:allValuesFrom}
     */
    public static final String ALL_VALUES_FROM = NAMESPACE + "allValuesFrom";

    /**
     * {@code owl:someValuesFrom}
     */
    public static final String SOME_VALUES_FROM = NAMESPACE + "someValuesFrom";

    /**
     * {@code owl:hasValue}
     */
    public static final String HAS_VALUE = NAMESPACE + "hasValue";

    /**
     * {@code owl:hasSelf}
     */
    public static final String HAS_SELF = NAMESPACE + "hasSelf";

    /**
     * {@code owl:hasKey}
     */
    public static final String HAS_KEY = NAMESPACE + "hasKey";

    /**
     * {@code owl:qualifiedCardinality}
     */
    public static final String QUALIFIED_CARDINALITY = NAMESPACE + "qualifiedCardinality";

    /**
     * {@code owl:minQualifiedCardinality}
     */
    public static final String MIN_QUALIFIED_CARDINALITY = NAMESPACE + "minQualifiedCardinality";

    /**
     * {@code owl:maxQualifiedCardinality}
     */
    public static final String MAX_QUALIFIED_CARDINALITY = NAMESPACE + "maxQualifiedCardinality";

    /**
     * {@code owl:onClass}
     */
    public static final String ON_CLASS = NAMESPACE + "onClass";

    /**
     * {@code owl:onDataRange}
     */
    public static final String ON_DATA_RANGE = NAMESPACE + "onDataRange";

    /**
     * {@code owl:onDatatype}
     */
    public static final String ON_DATATYPE = NAMESPACE + "onDatatype";

    /**
     * {@code owl:onProperties}
     */
    public static final String ON_PROPERTIES = NAMESPACE + "onProperties";

    /**
     * {@code owl:datatypeComplementOf}
     */
    public static final String DATATYPE_COMPLEMENT_OF = NAMESPACE + "datatypeComplementOf";

    /**
     * {@code owl:withRestrictions}
     */
    public static final String WITH_RESTRICTIONS = NAMESPACE + "withRestrictions";

    /**
     * {@code owl:AnnotationProperty}
     */
    public static final String ANNOTATION_PROPERTY = NAMESPACE + "AnnotationProperty";

    /**
     * {@code owl:DatatypeProperty}
     */
    public static final String DATATYPE_PROPERTY = NAMESPACE + "DatatypeProperty";

    /**
     * {@code owl:ObjectProperty}
     */
    public static final String OBJECT_PROPERTY = NAMESPACE + "ObjectProperty";

    /**
     * {@code owl:DataRange}
     */
    public static final String DATA_RANGE = NAMESPACE + "DataRange";

    /**
     * {@code owl:Restriction}
     */
    public static final String RESTRICTION = NAMESPACE + "Restriction";

    /**
     * {@code owl:AllDifferent}
     */
    public static final String ALL_DIFFERENT = NAMESPACE + "AllDifferent";

    /**
     * {@code owl:AllDisjointClasses}
     */
    public static final String ALL_DISJOINT_CLASSES = NAMESPACE + "AllDisjointClasses";

    /**
     * {@code owl:AllDisjointProperties}
     */
    public static final String ALL_DISJOINT_PROPERTIES = NAMESPACE + "AllDisjointProperties";

    /**
     * {@code owl:SymmetricProperty}
     */
    public static final String SYMMETRIC_PROPERTY = NAMESPACE + "SymmetricProperty";

    /**
     * {@code owl:AsymmetricProperty}
     */
    public static final String ASYMMETRIC_PROPERTY = NAMESPACE + "AsymmetricProperty";

    /**
     * {@code owl:ReflexiveProperty}
     */
    public static final String REFLEXIVE_PROPERTY = NAMESPACE + "ReflexiveProperty";

    /**
     * {@code owl:IrreflexiveProperty}
     */
    public static final String IRREFLEXIVE_PROPERTY = NAMESPACE + "IrreflexiveProperty";

    /**
     * {@code owl:TransitiveProperty}
     */
    public static final String TRANSITIVE_PROPERTY = NAMESPACE + "TransitiveProperty";

    /**
     * {@code owl:FunctionalProperty}
     */
    public static final String FUNCTIONAL_PROPERTY = NAMESPACE + "FunctionalProperty";

    /**
     * {@code owl:InverseFunctionalProperty}
     */
    public static final String INVERSE_FUNCTIONAL_PROPERTY = NAMESPACE + "InverseFunctionalProperty";

    /**
     * {@code owl:DeprecatedProperty}
     */
    public static final String DEPRECATED_PROPERTY = NAMESPACE + "DeprecatedProperty";

    /**
     * {@code owl:topObjectProperty}
     */
    public static final String TOP_OBJECT_PROPERTY = NAMESPACE + "topObjectProperty";

    /**
     * {@code owl:bottomObjectProperty}
     */
    public static final String BOTTOM_OBJECT_PROPERTY = NAMESPACE + "bottomObjectProperty";

    /**
     * {@code owl:topDataProperty}
     */
    public static final String TOP_DATA_PROPERTY = NAMESPACE + "topDataProperty";

    /**
     * {@code owl:bottomDataProperty}
     */
    public static final String BOTTOM_DATA_PROPERTY = NAMESPACE + "bottomDataProperty";

    /**
     * {@code owl:propertyChainAxiom}
     */
    public static final String PROPERTY_CHAIN_AXIOM = NAMESPACE + "propertyChainAxiom";

    /**
     * {@code owl:propertyDisjointWith}
     */
    public static final String PROPERTY_DISJOINT_WITH = NAMESPACE + "propertyDisjointWith";

    /**
     * {@code owl:OntologyProperty}
     */
    public static final String ONTOLOGY_PROPERTY = NAMESPACE + "OntologyProperty";

    /**
     * {@code owl:equivalentProperty}
     */
    public static final String EQUIVALENT_PROPERTY = NAMESPACE + "equivalentProperty";

    /**
     * {@code owl:equivalentClass}
     */
    public static final String EQUIVALENT_CLASS = NAMESPACE + "equivalentClass";

    /**
     * {@code owl:onProperty}
     */
    public static final String ON_PROPERTY = NAMESPACE + "onProperty";

    /**
     * {@code owl:Ontology}
     */
    public static final String ONTOLOGY = NAMESPACE + "Ontology";

    /**
     * {@code owl:NamedIndividual}
     */
    public static final String NAMED_INDIVIDUAL = NAMESPACE + "NamedIndividual";

    /**
     * {@code owl:versionInfo}
     */
    public static final String VERSION_INFO = NAMESPACE + "versionInfo";

    /**
     * {@code owl:priorVersion}
     */
    public static final String PRIOR_VERSION = NAMESPACE + "priorVersion";

    /**
     * {@code owl:versionIRI}
     */
    public static final String VERSION_IRI = NAMESPACE + "versionIRI";

    /**
     * {@code owl:incompatibleWith}
     */
    public static final String INCOMPATIBLE_WITH = NAMESPACE + "incompatibleWith";

    /**
     * {@code owl:backwardCompatibleWith}
     */
    public static final String BACKWARD_COMPATIBLE_WITH = NAMESPACE + "backwardCompatibleWith";

    /**
     * {@code owl:imports}
     */
    public static final String IMPORTS = NAMESPACE + "imports";

    private OWL() {
        throw new AssertionError();
    }
}
