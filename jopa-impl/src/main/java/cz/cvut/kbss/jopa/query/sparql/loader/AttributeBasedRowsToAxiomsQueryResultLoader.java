package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.QueryResultLoader;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
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
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

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
 *     <li>The remaining columns contain attribute values. Each column is named as {@code subjectName + attributeName}</li>
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
    private final EntityType<T> entityType;

    private Set<Axiom<?>> currentEntityAxioms = Set.of();
    private NamedResource currentSubject;

    AttributeBasedRowsToAxiomsQueryResultLoader(UnitOfWork uow, Class<T> resultType, Descriptor descriptor) {
        this.uow = uow;
        this.resultType = resultType;
        this.descriptor = descriptor;
        this.entityType = uow.getMetamodel().entity(resultType);
    }

    @Override
    public Optional<T> loadResult(ResultRow resultRow) {
        assert resultRow.getColumnCount() > 0;
        try {
            final URI subject = resultRow.getObject(0, URI.class);
            if (currentSubject == null) {
                reset(NamedResource.create(subject));
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
        final VariableNameMapper varNameMapper = createVarNameMapper(row);
        for (Attribute<? super T, ?> attribute : entityType.getAttributes()) {
            final String varName = varNameMapper.getAttributeVarName(attribute);
            if (row.isBound(varName)) {
                currentEntityAxioms.add(new AxiomImpl<>(currentSubject, attributeToAssertion(attribute), new Value<>(row.getObject(varName))));
            }
        }
        final Optional<TypesSpecification<? super T, ?>> typesSpec = Optional.ofNullable(entityType.getTypes());
        final String varName = varNameMapper.getTypesVarName(typesSpec.orElse(null));
        if (row.isBound(varName)) {
            currentEntityAxioms.add(new AxiomImpl<>(currentSubject,
                    Assertion.createClassAssertion(typesSpec.map(FieldSpecification::isInferred).orElse(false)),
                    new Value<>(row.getObject(varName))));
        }
    }

    private VariableNameMapper createVarNameMapper(ResultRow row) {
        assert !row.getColumnNames().isEmpty();
        final String subjectVarName = row.getColumnNames().get(0);
        final boolean optimized = row.getColumnNames().stream()
                .allMatch(name -> name.startsWith(subjectVarName));
        return optimized ? new OptimizedQueryVariableNameMapper(subjectVarName) : new DefaultVariableNameMapper();
    }

    private static Assertion attributeToAssertion(Attribute<?, ?> attribute) {
        return switch (attribute.getPersistentAttributeType()) {
            case OBJECT -> Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
            case DATA -> Assertion.createDataPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
            case ANNOTATION ->
                    Assertion.createAnnotationPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
        };
    }

    private T loadEntity() {
        try {
            return uow.readObjectFromAxioms(resultType, currentEntityAxioms, descriptor);
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

    private interface VariableNameMapper {
        String getAttributeVarName(Attribute<?, ?> attribute);
        String getTypesVarName(TypesSpecification<?, ?> types);
    }

    private record OptimizedQueryVariableNameMapper(String subjectVarName) implements VariableNameMapper {
        @Override
            public String getAttributeVarName(Attribute<?, ?> attribute) {
                return subjectVarName + attribute.getName();
            }

            @Override
            public String getTypesVarName(TypesSpecification<?, ?> types) {
                return subjectVarName + AttributeEnumeratingSparqlAssemblyModifier.TYPES_VAR_SUFFIX;
            }
        }

    private static class DefaultVariableNameMapper implements VariableNameMapper {
        @Override
        public String getAttributeVarName(Attribute<?, ?> attribute) {
            return attribute.getName();
        }

        @Override
        public String getTypesVarName(TypesSpecification<?, ?> types) {
            return types != null ? types.getName() : AttributeEnumeratingSparqlAssemblyModifier.TYPES_VAR_SUFFIX;
        }
    }
}
