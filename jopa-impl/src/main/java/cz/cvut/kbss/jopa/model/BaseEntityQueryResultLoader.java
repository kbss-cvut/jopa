package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;

import java.net.URI;
import java.util.Optional;

/**
 * Default entity loader just calls {@link UnitOfWork} to load an instance with the identifier retrieved from the result
 * row.
 *
 * @param <T> Result type
 */
public class BaseEntityQueryResultLoader<T> implements QueryResultLoader<T> {

    private final UnitOfWork uow;
    private final Class<T> resultType;
    private final Descriptor descriptor;

    public BaseEntityQueryResultLoader(UnitOfWork uow, Class<T> resultType, Descriptor descriptor) {
        this.uow = uow;
        this.resultType = resultType;
        this.descriptor = descriptor;
    }

    @Override
    public Optional<T> loadEntityInstance(ResultRow resultRow) {
        try {
            assert resultRow.isBound(0);
            final URI uri = URI.create(resultRow.getString(0));
            return Optional.ofNullable(uow.readObject(resultType, uri, descriptor));
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException("Unable to load query result as entity of type " + resultType, e);
        }
    }
}
