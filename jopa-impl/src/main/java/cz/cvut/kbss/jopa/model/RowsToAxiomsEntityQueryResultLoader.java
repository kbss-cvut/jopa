package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
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

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * Loads entity by aggregating rows with the same subject into axioms and then using
 * {@link UnitOfWork#readObjectFromAxioms(Class, Collection, Descriptor)} to read the entity.
 *
 * @param <T> Result type
 */
class RowsToAxiomsEntityQueryResultLoader<T> implements QueryResultLoader<T> {

    private final UnitOfWork uow;
    private final Class<T> resultType;
    private final Descriptor descriptor;

    private List<Axiom<?>> currentEntityAxioms;
    private NamedResource currentSubject;

    RowsToAxiomsEntityQueryResultLoader(UnitOfWork uow, Class<T> resultType, Descriptor descriptor) {
        this.uow = uow;
        this.resultType = resultType;
        this.descriptor = descriptor;
    }

    @Override
    public Optional<T> loadEntityInstance(ResultRow resultRow) {
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
                final T result = uow.readObjectFromAxioms(resultType, currentEntityAxioms, descriptor);
                final NamedResource newSubject = NamedResource.create(subject);
                reset(newSubject);
                currentEntityAxioms.add(new AxiomImpl<>(newSubject, propertyToAssertion(property), new Value<>(value)));
                return Optional.of(result);
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
            case OBJECT -> Assertion.createObjectPropertyAssertion(property, false);
            case DATA -> Assertion.createDataPropertyAssertion(property, false);
            case ANNOTATION -> Assertion.createAnnotationPropertyAssertion(property, false);
        }).orElseGet(() -> Assertion.createPropertyAssertion(property, false));
    }

    private void reset(NamedResource newSubject) {
        this.currentEntityAxioms = new ArrayList<>();
        this.currentSubject = newSubject;
    }

    @Override
    public Optional<T> loadLastPending() {
        if (!currentEntityAxioms.isEmpty()) {
            return Optional.of(uow.readObjectFromAxioms(resultType, currentEntityAxioms, descriptor));
        }
        return Optional.empty();
    }
}
