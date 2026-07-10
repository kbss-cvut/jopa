package cz.cvut.kbss.jopa.id;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.Set;

/**
 * Common utilities for identifier generation.
 */
public abstract class AbstractIdentifierGenerator implements IdentifierGenerator {

    /**
     * Checks if a class assertion for the specified identifier exists in the repository.
     * <p>
     * This check is done against the default context.
     *
     * @param identifier Instance identifier
     * @param classIri   Class identifier
     * @param connection Repository connection
     * @return {@code true} if the class assertion exists, {@code false} otherwise
     * @throws IdentifierGenerationException If unable to check if the identifier exists
     */
    protected boolean exists(URI identifier, URI classIri, Connection connection) {
        try {
            return connection.contains(new AxiomImpl<>(NamedResource.create(identifier),
                    Assertion.createClassAssertion(false),
                    new Value<>(classIri)), Set.of());
        } catch (OntoDriverException e) {
            throw new IdentifierGenerationException("Unable to check if identifier '" + identifier + "' exists.", e);
        }
    }

    /**
     * Resolves the identifier of the ontological class of the specified entity.
     *
     * @param entity    Entity whose class IRI to resolve
     * @param metamodel Metamodel
     * @return Class IRI
     */
    protected URI getEntityClassUri(Object entity, Metamodel metamodel) {
        final EntityType<?> et = metamodel.entity(entity.getClass());
        return et.getIRI().toURI();
    }
}
