/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

class MainAxiomLoader {

    private final AbstractAxiomLoader inferredLoader;
    private final ExplicitAxiomLoader explicitLoader;

    MainAxiomLoader(StorageConnector connector, InferredStorageConnector inferredConnector) {
        this.explicitLoader = new ExplicitAxiomLoader(connector);
        // It is possible that the inferred connector is null - if we are using the read_committed strategy or only snapshot,
        // without inference
        this.inferredLoader = new InferredAxiomLoader(inferredConnector);
    }

    /**
     * Checks whether the storage contains the specified axiom.
     *
     * @param axiom   Axiom whose existence should be verified
     * @param contexts Contexts to search, optional (empty indicates default context)
     * @return {@code true} if the axiom exists, {@code false} otherwise
     */
    boolean contains(Axiom<?> axiom, Set<URI> contexts) {
        return axiom.getAssertion().isInferred() ? inferredLoader.contains(axiom, contexts) :
                explicitLoader.contains(axiom, contexts);
    }

    /**
     * Loads axioms corresponding to the specified descriptor.
     *
     * @param descriptor Descriptor of axioms to load
     * @return Matching axioms
     */
    Collection<Axiom<?>> find(AxiomDescriptor descriptor) {
        final Map<String, Assertion> asserted = new HashMap<>(descriptor.getAssertions().size());
        final Map<String, Assertion> inferred = new HashMap<>(descriptor.getAssertions().size());
        mapAssertions(descriptor, asserted, inferred);
        final Collection<Axiom<?>> result = explicitLoader.find(descriptor, asserted);
        result.addAll(inferredLoader.find(descriptor, inferred));
        return result;
    }

    private static void mapAssertions(AxiomDescriptor descriptor, Map<String, Assertion> asserted,
                                      Map<String, Assertion> inferred) {
        for (Assertion a : descriptor.getAssertions()) {
            if (a.isInferred()) {
                inferred.put(a.getIdentifier().toString(), a);
            } else {
                asserted.put(a.getIdentifier().toString(), a);
            }
        }
    }

    /**
     * Loads all asserted property statements with the specified subject.
     * <p>
     * Note that type assertion statements (those with property {@code rdf:type}) are skipped.
     *
     * @param subject Statement subject
     * @param context Context identifier, optional
     * @return Matching statements
     */
    Collection<Axiom<?>> find(NamedResource subject, URI context) {
        return explicitLoader.find(subject, context);
    }

    /**
     * Loads all property statements with the specified subject, including inferred ones.
     * <p>
     * Note that type assertion statements (those with property {@code rdf:type}) are skipped.
     *
     * @param subject Statement subject
     * @param context Context identifier, optional
     * @return Matching statements
     */
    Collection<Axiom<?>> findWithInference(NamedResource subject, URI context) {
        return inferredLoader.find(subject, context);
    }
}
