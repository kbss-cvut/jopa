package cz.cvut.kbss.ontodriver_new;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

import java.net.URI;
import java.util.Set;

/**
 * This interface is used for working with individuals' types.
 *
 * @author ledvima1
 */
public interface Types {

    /**
     * Gets types associated with the specified individual.
     *
     * @param individual      Resource for which types should be found
     * @param context         Context in which to look for the types
     * @param includeInferred Whether to include inferred types as well
     * @return Set of type URIs
     * @throws OntoDriverException When an ontology access error occurs
     */
    public Set<Axiom<URI>> getTypes(NamedResource individual, URI context, boolean includeInferred) throws OntoDriverException;

    /**
     * Persists the specified types of the specified individual.
     *
     * @param individual Individual whose types are being persisted
     * @param context    Context into which the types will be added
     * @param types      The types to persist
     * @throws OntoDriverException When an ontology access error occurs
     */
    public void persistTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException;

    /**
     * Updates types of the specified individual with the new values.
     * <p/>
     * It is up to the driver implementation whether this operation removes the previous types and replaces them with the new ones or whether it will perform a merge.
     *
     * @param individual Individual whose types are being updated
     * @param context    Context in which the types are being updated
     * @param types      The new types
     * @throws OntoDriverException When an ontology access error occurs
     */
    public void updateTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException;
}
