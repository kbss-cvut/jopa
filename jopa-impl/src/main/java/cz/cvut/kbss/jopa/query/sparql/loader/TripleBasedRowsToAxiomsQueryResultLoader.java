package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.QueryResultLoader;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.vocabulary.RDF;
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
import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * Loads entity by aggregating rows with the same subject into axioms and then using
 * {@link UnitOfWork#readObjectFromAxioms(Class, Collection, Descriptor)} to read the entity.
 * <p>
 * It expects the query result rows to have three columns corresponding to the triple subject, property and object.
 *
 * @param <T> Result type
 */
class TripleBasedRowsToAxiomsQueryResultLoader<T> implements QueryResultLoader<T> {

    private static final Logger LOG = LoggerFactory.getLogger(TripleBasedRowsToAxiomsQueryResultLoader.class);

    private final UnitOfWork uow;
    private final Class<T> resultType;
    private final Descriptor descriptor;

    private List<Axiom<?>> currentEntityAxioms = List.of();
    private NamedResource currentSubject;

    TripleBasedRowsToAxiomsQueryResultLoader(UnitOfWork uow, Class<T> resultType, Descriptor descriptor) {
        this.uow = uow;
        this.resultType = resultType;
        this.descriptor = descriptor;
    }

    @Override
    public Optional<T> loadResult(ResultRow resultRow) {
        assert resultRow.getColumnCount() == 3;
        try {
            final URI subject = resultRow.getObject(0, URI.class);
            final URI property = resultRow.getObject(1, URI.class);
            final Object value = resultRow.getObject(2);
            if (currentSubject == null) {
                reset(NamedResource.create(subject));
            }
            if (subject.equals(currentSubject.getIdentifier())) {
                currentEntityAxioms.add(new AxiomImpl<>(currentSubject, propertyToAssertion(property), new Value<>(value)));
                return Optional.empty();
            } else {
                final T result = loadEntity();
                final NamedResource newSubject = NamedResource.create(subject);
                reset(newSubject);
                currentEntityAxioms.add(new AxiomImpl<>(newSubject, propertyToAssertion(property), new Value<>(value)));
                return Optional.ofNullable(result);
            }
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException("Unable to load query result as entity of type " + resultType, e);
        }
    }

    private Assertion propertyToAssertion(URI property) {
        if (RDF.TYPE.equals(property.toString())) {
            return Assertion.createClassAssertion(false);
        }
        final EntityType<T> et = uow.getMetamodel().entity(resultType);
        final Optional<Attribute<? super T, ?>> attribute = et.getAttributes().stream()
                                                              .filter(att -> att.getIRI().toString()
                                                                                .equals(property.toString()))
                                                              .findFirst();
        return attribute.map(att -> switch (att.getPersistentAttributeType()) {
            case OBJECT -> Assertion.createObjectPropertyAssertion(property, att.isInferred());
            case DATA -> Assertion.createDataPropertyAssertion(property, att.isInferred());
            case ANNOTATION -> Assertion.createAnnotationPropertyAssertion(property, att.isInferred());
        }).orElseGet(() -> Assertion.createPropertyAssertion(property, false));
    }

    private T loadEntity() {
        try {
            return uow.readObjectFromAxioms(resultType, currentEntityAxioms, descriptor);
        } catch (CardinalityConstraintViolatedException e) {
            // Axioms may contain more statements than expected due to query evaluation containing inferred results.
            // If the entity class declares ICs on non-inferred attributes, this may lead to IC violation exception.
            // In that case, fall back to regular entity loading which uses the underlying repository access API and thus explicitly handles asserted and inferred statements
            LOG.debug("Unable to load entity from axioms due to cardinality constraint violation, using regular entity loading.", e);
            return uow.readObject(resultType, currentSubject.getIdentifier(), descriptor);
        }
    }

    private void reset(NamedResource newSubject) {
        this.currentEntityAxioms = new ArrayList<>();
        this.currentSubject = newSubject;
    }

    @Override
    public Optional<T> loadLastPending() {
        if (!currentEntityAxioms.isEmpty()) {
            return Optional.ofNullable(loadEntity());
        }
        return Optional.empty();
    }
}
