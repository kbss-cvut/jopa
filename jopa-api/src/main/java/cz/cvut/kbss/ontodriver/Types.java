package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

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
     * Adds the specified types to the named individual in the ontology.
     *
     * @param individual The types subject
     * @param context    Context into which the type statements will be added
     * @param types      The types to add
     * @throws OntoDriverException When an ontology access error occurs
     */
    public void addTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException;

    /**
     * Removes the specified types of the named individual in the ontology.
     *
     * @param individual The types subject
     * @param context    Context into which the type statements will be added
     * @param types      The types to add
     * @throws OntoDriverException When an ontology access error occurs
     */
    public void removeTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException;
}
