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
package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.QueryResultLoader;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.util.AxiomBasedLoadingConfigGroup;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Loads query results as axioms and then uses {@link cz.cvut.kbss.jopa.sessions.UnitOfWork} to create the actual entity
 * instances.
 * <p>
 * Groups rows concerning the same subject so that plural attribute values are supported.
 * <p>
 * Assumptions made by this loader:
 * <ul>
 *     <li>Query result contains at least one column</li>
 *     <li>The first column contains the subject URI</li>
 *     <li>The remaining columns contain attribute values. Each column is named as {@code subjectName + "_" + attributeName}</li>
 * </ul>
 * <p>
 * If the loader is unable to load the entity (e.g., due to cardinality constraint violation), it falls back to
 * regular entity loading.
 *
 * @param <T> Result type
 */
class AttributeBasedRowsToAxiomsQueryResultLoader<T> implements QueryResultLoader<T> {

    private static final Logger LOG = LoggerFactory.getLogger(AttributeBasedRowsToAxiomsQueryResultLoader.class);

    private final UnitOfWork uow;
    private final Class<T> resultType;
    private final Descriptor descriptor;
    private final IdentifiableEntityType<T> entityType;
    private final EntityGraph<T> fetchGraph;
    private final Supplier<List<QueryVariableMapping>> mappingSupplier;

    private List<QueryVariableMapping> mappings;

    private Set<Axiom<?>> currentEntityAxioms = Set.of();
    private NamedResource currentSubject;

    AttributeBasedRowsToAxiomsQueryResultLoader(UnitOfWork uow, Class<T> resultType, Descriptor descriptor,
                                                EntityGraph<T> fetchGraph) {
        this(uow, resultType, descriptor, fetchGraph, () -> null);
    }

    AttributeBasedRowsToAxiomsQueryResultLoader(UnitOfWork uow, Class<T> resultType, Descriptor descriptor,
                                                EntityGraph<T> fetchGraph,
                                                Supplier<List<QueryVariableMapping>> mappingSupplier) {
        this.uow = uow;
        this.resultType = resultType;
        this.descriptor = descriptor;
        this.mappingSupplier = mappingSupplier != null ? mappingSupplier : () -> null;
        this.entityType = uow.getMetamodel().entity(resultType);
        this.fetchGraph = fetchGraph;
    }

    @Override
    public void init(ResultSet resultSet) {
        this.mappings = mappingSupplier.get();
    }

    private void createProjectionMappings(List<String> projectedVars) {
        if (mappings == null) {
            this.mappings = mappingSupplier.get();
        }
        if (mappings != null) {
            return;
        }
        assert !projectedVars.isEmpty();
        final String subjectVar = projectedVars.get(0);
        this.mappings = new ArrayList<>(projectedVars.size() - 1);
        final Map<String, Attribute<?, ?>> atts = FetchGraphProcessor.attributes(entityType).stream()
                                                                     .collect(Collectors.toMap(Attribute::getName, Function.identity()));
        for (int i = 1; i < projectedVars.size(); i++) {
            final String projectedVar = projectedVars.get(i);
            if (atts.containsKey(projectedVar)) {
                // ?stringAttribute
                mappings.add(new QueryVariableMapping(subjectVar, projectedVar, atts.get(projectedVar)));
            } else if (atts.containsKey(projectedVar.substring(subjectVar.length() + 1))) {
                // ?x_stringAttribute
                mappings.add(new QueryVariableMapping(subjectVar, projectedVar, atts.get(projectedVar.substring(subjectVar.length() + 1))));
            } else if (projectedVar.endsWith(AttributeEnumeratingSparqlAssemblyModifier.TYPES_VAR_NAME)) {
                // ?x_types or ?types
                mappings.add(new QueryVariableMapping(subjectVar, projectedVar, entityType.getTypes()));
            } else {
                LOG.warn("Variable '{}' projected from the query cannot be mapped to any attributes in entity class {}.", projectedVar, entityType);
            }
        }
    }

    @Override
    public Optional<T> loadResult(ResultRow resultRow) {
        assert resultRow.getColumnCount() > 0;
        try {
            final URI subject = resultRow.getObject(0, URI.class);
            if (currentSubject == null) {
                reset(NamedResource.create(subject));
                createProjectionMappings(resultRow.getColumnNames());
            }
            if (subject.equals(currentSubject.getIdentifier())) {
                rowToAxioms(resultRow);
                return Optional.empty();
            } else {
                final T result = loadEntity();
                final NamedResource newSubject = NamedResource.create(subject);
                reset(newSubject);
                rowToAxioms(resultRow);
                return Optional.ofNullable(result);
            }
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException("Unable to load query result as entity of type " + resultType, e);
        }
    }

    private void reset(NamedResource newSubject) {
        this.currentEntityAxioms = new HashSet<>();
        this.currentSubject = newSubject;
    }

    private void rowToAxioms(ResultRow row) throws OntoDriverException {
        for (QueryVariableMapping mapping : mappings) {
            if (row.isBound(mapping.attributeVar())) {
                assert row.isBound(mapping.subjectVar());
                final NamedResource subject = NamedResource.create(row.getObject(mapping.subjectVar(), URI.class));
                final Assertion assertion = attributeToAssertion(mapping.attribute());
                extractValue(row, mapping).map(v -> new AxiomImpl<>(subject, assertion, v))
                                          .forEach(currentEntityAxioms::add);
            }
        }
    }

    private static Assertion attributeToAssertion(FieldSpecification<?, ?> fs) {
        if (fs != null && fs.isMappedAttribute()) {
            final Attribute<?, ?> attribute = (Attribute<?, ?>) fs;
            return switch (attribute.getPersistentAttributeType()) {
                case OBJECT ->
                        Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
                case DATA -> Assertion.createDataPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
                case ANNOTATION ->
                        Assertion.createAnnotationPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
            };
        } else {
            if (fs != null) {
                assert fs instanceof TypesSpecification<?, ?>;
                return Assertion.createClassAssertion(fs.isInferred());
            } else {
                return Assertion.createClassAssertion(false);
            }
        }
    }

    private Stream<Value<?>> extractValue(ResultRow row,
                                          QueryVariableMapping variableMapping) throws OntoDriverException {
        if (variableMapping.canGroupConcat()) {
            final String values = row.getString(variableMapping.attributeVar());
            // This does not handle blank nodes
            return Stream.of(values.split(AttributeEnumeratingSparqlAssemblyModifier.GROUP_CONCAT_SEPARATOR))
                         .map(URI::create).map(Value::new);
        } else {
            return Stream.of(new Value<>(row.getObject(variableMapping.attributeVar())));
        }
    }

    private T loadEntity() {
        try {
            return uow.readObjectFromAxioms(resultType, currentEntityAxioms, new AxiomBasedLoadingConfigGroup<>(currentSubject.getIdentifier(), descriptor, fetchGraph));
        } catch (CardinalityConstraintViolatedException e) {
            // Axioms may contain more statements than expected due to query evaluation containing inferred results.
            // If the entity class declares ICs on non-inferred attributes, this may lead to IC violation exception.
            // In that case, fall back to regular entity loading which uses the underlying repository access API and thus explicitly handles asserted and inferred statements
            LOG.debug(
                    "Unable to load entity from axioms due to cardinality constraint violation, using regular entity loading.",
                    e);
            return uow.readObject(resultType, currentSubject.getIdentifier(), descriptor);
        }
    }

    @Override
    public Optional<T> loadLastPending() {
        if (!currentEntityAxioms.isEmpty()) {
            return Optional.ofNullable(loadEntity());
        }
        return Optional.empty();
    }
}
