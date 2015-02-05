package cz.cvut.kbss.ontodriver_new;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

/**
 * This interface provides access to properties not mapped by the object model.
 *
 * @author ledvima1
 */
public interface Properties {

    /**
     * Loads property values for the specified individual.
     * <p/>
     * This method essentially does the same as
     * {@link cz.cvut.kbss.ontodriver_new.Connection#find(cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor)}.
     * It is up to the OOM provider to decide which values are part of the object model and which are unmapped.
     *
     * @param individual      Individual for which property values should be loaded
     * @param context         Context from which to load the property values
     * @param includeInferred Whether to included inferred knowledge
     * @return Collection of axioms representing property values
     */
    public Collection<Axiom<?>> getProperties(NamedResource individual, URI context, boolean includeInferred) throws OntoDriverException;

    /**
     * Adds the specified property values into the ontology.
     * <p/>
     * The property values are either URIs (in case of object properties) or data literals of the appropriate Java type.
     *
     * @param individual Property subject
     * @param context    Context into which to store the property values
     * @param properties The values to add
     */
    public void addProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties) throws OntoDriverException;

    /**
     * Removes the specified property values from the ontology.
     * <p/>
     * The property values are either URIs (in case of object properties) or data literals of the appropriate Java type.
     *
     * @param individual Property subject
     * @param context    Context from which to remove the property values
     * @param properties The values to remove
     */
    public void removeProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties) throws OntoDriverException;

}
