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
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Set;

/**
 * This interface is used for working with individuals' types.
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
    Set<Axiom<URI>> getTypes(NamedResource individual, URI context, boolean includeInferred) throws OntoDriverException;

    /**
     * Adds the specified types to the named individual in the ontology.
     *
     * @param individual The types subject
     * @param context    Context into which the type statements will be added
     * @param types      The types to add
     * @throws OntoDriverException When an ontology access error occurs
     */
    void addTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException;

    /**
     * Removes the specified types of the named individual in the ontology.
     *
     * @param individual The types subject
     * @param context    Context into which the type statements will be added
     * @param types      The types to add
     * @throws OntoDriverException When an ontology access error occurs
     */
    void removeTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException;
}
